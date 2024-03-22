use crate::ffi::*;
use crate::make_cstring;
use crate::logic;
use std::sync::{Mutex, TryLockError};

// `dlsym` invokes `calloc` behind the scenes. So `calloc` must know
// whether `dlsym` has been called --> if not, proceed w/ normal flow
static DLSYM_CALLED: Mutex<()> = Mutex::new(());

pub fn fake_malloc(size: size_t) -> *mut void {
    // Concurrent calls are serialized to the order
    // that they were traced. All threads wait in
    // the lobby until the next expected request, read
    // from the trace, matches the one currently held.
    logic::lobby(logic::ReqType::Malloc(size))
}

pub fn fake_free(p: *mut void) {
    if !p.is_null() {
        logic::lobby(logic::ReqType::Free(p as usize));
    }
}

pub fn fake_calloc(nobj: size_t, size: size_t) -> *mut void {
    logic::lobby(logic::ReqType::Calloc(nobj, size))
}

pub fn fake_realloc(p: *mut void, size: size_t) -> *mut void {
    logic::lobby(logic::ReqType::ReAlloc(p as usize, size))
}

pub fn fake_aligned_alloc(alignment: size_t, size: size_t) -> *mut void {
    logic::lobby(logic::ReqType::AlignedAlloc(alignment, size))
}

pub fn fake_memalign(alignment: size_t, size: size_t) -> *mut void {
    logic::lobby(logic::ReqType::MemAlign(alignment, size))
}

pub fn fake_posix_memalign(memptr: *mut *mut void, alignment: size_t, size: size_t) -> int {
    logic::lobby(logic::ReqType::PosixMemAlign(memptr as usize, alignment, size));

    0
}

/// Used internally by the copycat. Invokes either system `malloc`,
/// or other `LD_PRELOAD`-ed implementation.
pub unsafe fn true_malloc(size: size_t) -> *mut void {
    // Only one thread is allowed to initialize,
    // i.e., call `dlsym` etc.
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    // True function has to be fetched only once.
    static mut FUNC: Option<CMalloc> = None;
    // Value to be returned.
    let ptr: *mut void;

    match FUNC {
        Some(malloc) => {
            // Happy path: function already initialized.
            // `malloc` is thread-safe!
            // [source: https://man7.org/linux/man-pages/man3/malloc.3.html#ATTRIBUTES]
            ptr = malloc(size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    // Function not initialized, this is the first thread to do it.
                    // `dlsym` is thread-safe! Thus other functions can be initialized in parallel.
                    // [source: https://man7.org/linux/man-pages/man3/dlsym.3.html#ATTRIBUTES]
                    match DLSYM_CALLED.lock() {
                        // Mutex needed to communicate to `calloc` the need to return a buffer
                        // for `dlsym` instead of running the standard flow.
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("malloc").as_ptr());
                            // `dlerror` is thread-safe!
                            // [source: https://man7.org/linux/man-pages/man3/dlerror.3.html#ATTRIBUTES]
                            check_dlerror();

                            // Too arcane for yours truly.
                            let func_ptr = address as *const ();
                            let malloc = std::mem::transmute::<*const (), CMalloc>(func_ptr);
                            ptr = malloc(size);
                            FUNC = Some(malloc);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e) => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            // Do not tolerate poisoned threads.
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            // Edge case: thread tries to initialize while another
                            // thread has already started initialization.
                            while let None = FUNC {/* Wait until initialization is complete. */};
                            ptr = FUNC.unwrap()(size);
                        }
                    }
                }
            }
        }
    }

    ptr
}

pub unsafe fn true_free(p: *mut void) {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CFree> = None;
    match FUNC {
        Some(free) => {
            free(p);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    match DLSYM_CALLED.lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("free").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let free = std::mem::transmute::<*const (), CFree>(func_ptr);
                            free(p);
                            FUNC = Some(free);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e)  => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            while let None = FUNC {};
                            FUNC.unwrap()(p);
                        }
                    }
                }
            }
        }
    }
}

pub unsafe fn true_calloc(nobj: size_t, size: size_t) -> *mut void {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CCalloc> = None;
    // `dlsym` calls `calloc` internally--avoid infinite recursion.
    static mut DLSYM_BUFFER: [int; 8192] = [0; 8192];
    let ptr: *mut void;

    match FUNC {
        Some(calloc) => {
            ptr = calloc(nobj, size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    // In `calloc`, and only here, we use `try_lock` on the `dlsym` mutex,
                    // since it's possible that some other thread is already initializing
                    // some other function.
                    match DLSYM_CALLED.try_lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("calloc").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let calloc = std::mem::transmute::<*const (), CCalloc>(func_ptr);
                            ptr = calloc(nobj, size);
                            FUNC = Some(calloc);
                        },
                        Err(e)  => {
                            match e {
                                TryLockError::Poisoned(_)   => {
                                    graceful_exit("Poisoned mutex encountered.");
                                },
                                TryLockError::WouldBlock    => {
                                    // Some other function has already called `dlsym`.
                                    return DLSYM_BUFFER.as_mut_ptr() as *mut void;
                                }
                            }
                        }
                    }
                },
                Err(e)  => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            match DLSYM_CALLED.try_lock() {
                                Ok(guard)   => {
                                    // Edge case: the thread initializing `calloc` did not
                                    // lock DLSYM mutex in time--some other thread got it.
                                    // It must wait for the initialization to complete.
                                    drop(guard);
                                    while let None = FUNC {};
                                    ptr = FUNC.unwrap()(nobj, size);
                                },
                                Err(e)  => {
                                    match e {
                                        TryLockError::Poisoned(_)   => {
                                            graceful_exit("Poisoned mutex encountered.");
                                        },
                                        TryLockError::WouldBlock    => {
                                            // `dlsym` called in the context of `calloc` initialization.
                                            return DLSYM_BUFFER.as_mut_ptr() as *mut void;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    ptr
}

pub unsafe fn true_realloc(p: *mut void, size: size_t)  -> *mut void {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CRealloc> = None;
    let ptr: *mut void;

    match FUNC {
        Some(realloc) => {
            ptr = realloc(p, size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    match DLSYM_CALLED.lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("realloc").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let realloc = std::mem::transmute::<*const (), CRealloc>(func_ptr);
                            ptr = realloc(p, size);
                            FUNC = Some(realloc);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e) => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            while let None = FUNC {};
                            ptr = FUNC.unwrap()(p, size);
                        }
                    }
                }
            }
        }
    }

    ptr
}

pub unsafe fn true_aligned_alloc(alignment: size_t, size: size_t)  -> *mut void {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CAlignedAlloc> = None;
    let ptr: *mut void;

    match FUNC {
        Some(aligned_alloc) => {
            ptr = aligned_alloc(alignment, size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    match DLSYM_CALLED.lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("aligned_alloc").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let aligned_alloc = std::mem::transmute::<*const (), CAlignedAlloc>(func_ptr);
                            ptr = aligned_alloc(alignment, size);
                            FUNC = Some(aligned_alloc);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e) => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            while let None = FUNC {};
                            ptr = FUNC.unwrap()(alignment, size);
                        }
                    }
                }
            }
        }
    }

    ptr
}

pub unsafe fn true_memalign(alignment: size_t, size: size_t)  -> *mut void {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CMemalign> = None;
    let ptr: *mut void;

    match FUNC {
        Some(memalign) => {
            ptr = memalign(alignment, size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    match DLSYM_CALLED.lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("memalign").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let memalign = std::mem::transmute::<*const (), CMemalign>(func_ptr);
                            ptr = memalign(alignment, size);
                            FUNC = Some(memalign);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e) => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            while let None = FUNC {};
                            ptr = FUNC.unwrap()(alignment, size);
                        }
                    }
                }
            }
        }
    }

    ptr
}

pub unsafe fn true_posix_memalign(memptr: *mut *mut void, alignment: size_t, size: size_t)  -> int {
    static INIT_LOCK: Mutex<()> = Mutex::new(());
    static mut FUNC: Option<CPosixMemalign> = None;
    let res: int;

    match FUNC {
        Some(posix_memalign) => {
            res = posix_memalign(memptr, alignment, size);
        },
        None    => {
            match INIT_LOCK.try_lock() {
                Ok(_)   => {
                    match DLSYM_CALLED.lock() {
                        Ok(_)   => {
                            let address = dlsym(RTLD_NEXT, make_cstring!("posix_memalign").as_ptr());
                            check_dlerror();
                            let func_ptr = address as *const ();
                            let posix_memalign = std::mem::transmute::<*const (), CPosixMemalign>(func_ptr);
                            res = posix_memalign(memptr, alignment, size);
                            FUNC = Some(posix_memalign);
                        },
                        _   => { graceful_exit("Someone panicked in dlsym flow."); }
                    }
                },
                Err(e) => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex encountered.");
                        },
                        TryLockError::WouldBlock    => {
                            while let None = FUNC {};
                            res = FUNC.unwrap()(memptr, alignment, size);
                        }
                    }
                }
            }
        }
    }

    res
}

/* 
    ENTER ILLEGAL FUNCTIONS
    Rejected as either thread-unsafe or
    too complex to handle in this stage of development.
*/

#[no_mangle]
unsafe extern "C"
fn pvalloc(_size: size_t) -> *mut void {
    // [source: https://man7.org/linux/man-pages/man3/posix_memalign.3.html#ATTRIBUTES]
    graceful_exit("pvalloc not allowed (thread-unsafe)!");
}

#[no_mangle]
unsafe extern "C"
fn valloc(_size: size_t) -> *mut void {
    // [source: https://man7.org/linux/man-pages/man3/posix_memalign.3.html#ATTRIBUTES]
    graceful_exit("valloc not allowed (thread-unsafe)!");
}

#[no_mangle]
unsafe extern "C" fn fork() -> pid_t {
    graceful_exit("fork not allowed!");
}

#[no_mangle]
unsafe extern "C" fn vfork() -> pid_t {
    graceful_exit("vfork not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execve(_p: *const char, _rg: *const char, _nv: *const char) -> int {
    graceful_exit("execve not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execl(_p: *const char, _rg: *const char) -> int {
    graceful_exit("execl not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execlp(_p: *const char, _rg: *const char) -> int {
    graceful_exit("execlp not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execv(_p: *const char, _rg: *const char) -> int {
    graceful_exit("execv not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execvp(_p: *const char, _rg: *const char) -> int {
    graceful_exit("execvp not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execvpe(_p: *const char, _rg: *const char) -> int {
    graceful_exit("execvpe not allowed!");
}

#[no_mangle]
unsafe extern "C" fn execle(_p: *const char, _rg: *const char, _nv: *const char) -> int {
    graceful_exit("execle not allowed!");
}

#[no_mangle]
unsafe extern "C" fn system(_p: *const char) -> int {
    graceful_exit("system not allowed!");
}