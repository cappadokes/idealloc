use crate::make_cstring;

use super::ffi::*;
pub use once_cell::sync::Lazy;
pub use std::sync::{Mutex, TryLockError};
pub use std::sync::atomic::{AtomicUsize, Ordering};
use std::hash::BuildHasherDefault;
use std::ptr::addr_of;
use indexmap::IndexMap;
use ahash::AHasher;

type ShouldLogBook = IndexMap<pthread_t, bool, BuildHasherDefault<AHasher>>;

/// Each request is associated with a byte denoting: (i) its type (first 4 bits)
/// and (ii) the number of **machine words** that follow, describing the request's
/// arguments and placement.
const REQ_TYPE: [u8; 7] = [0x05, 0x12, 0x26, 0x36, 0x46, 0x56, 0x67];
/// The tracer will at times need to allocate its own memory dynamically.
/// Those requests should not be logged. A thread-safe way to convey this
/// information is via a stable-size TID-indexed map, where each entry is
/// accessed ONLY by the thread that has the same TID as the entry's index.
pub static mut SHOULD_LOG: Lazy<ShouldLogBook> = Lazy::new(|| {
    ShouldLogBook::default()
});
pub const MAX_THREADS_NUM: usize = 256;
static REQS_LOGGED: AtomicUsize = AtomicUsize::new(0);

#[inline(always)]
pub unsafe fn must_log() -> bool {
    //! Checks the TID map to deduce whether
    //! request should be logged or not.
    
    // The first time the function is called, the map is not
    // initialized, i.e. allocated, yet. Obviously this must
    // be done only once, and the other threads must wait.
    static mut INIT_LOCK: Option<Mutex<()>> = Some(Mutex::new(()));
    static mut INIT_THREAD: pthread_t = 0;

    let tid = pthread_self();
    match &*addr_of!(INIT_LOCK) {
        // Existence of the mutex is a signal that the map
        // has not been initialized.
        Some(mtx)   => {
            match mtx.try_lock() {
                Ok(_)   => {
                    INIT_THREAD = tid;
                    // This operation will trigger one more allocation...
                    SHOULD_LOG.reserve(MAX_THREADS_NUM);
                    INIT_LOCK = None;
                },
                Err(e)  => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex upon TID map init.");
                        },
                        TryLockError::WouldBlock    => {
                            if tid == INIT_THREAD {
                                // ...which will reach this block. Since we're
                                // in the context of book-keeping, no logging
                                // must be made.
                                return false;
                            }
                            // The rest of the threads must wait until
                            // initialization is complete.
                            while let Some(_) = &*addr_of!(INIT_LOCK) {}
                        }
                    }
                }
            }
        },
        None    => {
            if tid == INIT_THREAD {
                // Corner case. Unfortunately the first
                // `insert` command allocates too.
                INIT_THREAD = 0;
                if let None = SHOULD_LOG.get(&tid) {
                    return false;
                }
            }
        }
    };
    // This is the most frequently run block. The code above
    // deals with initialization.
    match SHOULD_LOG.get(&tid) {
        Some(verdict)   => { 
            *verdict 
        },
        None            => {
            if SHOULD_LOG.len() + 1 > MAX_THREADS_NUM {
                graceful_exit("More threads observed than maximum number allowed!");
            }
            SHOULD_LOG.insert(tid, true);
            true
        }
    }
}

#[inline(always)]
pub unsafe fn disable_logging() {
    let rf = SHOULD_LOG.get_mut(&pthread_self()).unwrap();
    *rf = false;
}

#[inline(always)]
pub unsafe fn enable_logging() {
    let rf = SHOULD_LOG.get_mut(&pthread_self()).unwrap();
    *rf = true;
}

#[inline(always)]
pub unsafe fn update_log(req_idx: usize, args: &[usize]) {
    // Writing to files is thread-safe; writing to files while
    // they're created isn't. We use the same Option-Mutex pattern 
    // w/ `must_log` to make sure that no thread tries to update the
    // log while it's being initialized.
    static mut INIT_LOCK: Option<Mutex<()>> = Some(Mutex::new(()));
    static mut FD: Lazy<*mut FILE> = Lazy::new(|| unsafe {
        // `init_scribe` allocates!
        disable_logging();
        let res = init_scribe();
        enable_logging();
        res
    });
    REQS_LOGGED.fetch_add(1, Ordering::Relaxed);

    // Maximum log line size is 7 words + 1 byte = 57 bytes.
    const MAX_LOG_LINE_BYTES: usize = 8 * 7 + 1;
    let mut buff: [u8; MAX_LOG_LINE_BYTES] = [0x00; MAX_LOG_LINE_BYTES];
    buff[0] = REQ_TYPE[req_idx];
    let mut idx = 1;
    for arg in args {
        let arg_bytes = arg.to_be_bytes();
        for b in &arg_bytes {
            buff[idx] = *b;
            idx += 1;
        }
    }

    let test: usize;

    match &*addr_of!(INIT_LOCK) {
        Some(mtx)   => {
            match mtx.try_lock() {
                Ok(_)   => {
                    test = fwrite(buff.as_ptr() as *const void, 1, idx, *FD);
                    INIT_LOCK = None;
                },
                Err(e)  => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex upon log init.");
                        },
                        TryLockError::WouldBlock    => {
                            while let Some(_) = &*addr_of!(INIT_LOCK) {}
                            test = fwrite(buff.as_ptr() as *const void, 1, idx, *FD);
                        }
                    }
                }
            }
        },
        None    => {
            test = fwrite(buff.as_ptr() as *const void, 1, idx, *FD);
        }
    }

    if test != idx {
        graceful_exit("Failed to write correct amount of bytes.");
    }
}

unsafe fn init_scribe() -> *mut FILE {
    //! Parses TRACEFILE environment variable and
    //! creates the file (or opens and truncates it).
    //! Also enhances writing with a buffer.
    
    static mut WRBUF: [i8; 8192] = [0; 8192];
    let path = getenv(make_cstring!("TRACEFILE").as_ptr());
    if path.is_null() {
        graceful_exit("Cannot find TRACEFILE envVar.");
    }
    let open_result = fopen(path.cast_const(), make_cstring!("w+").as_ptr());
    if open_result.is_null() {
        graceful_exit("Cannot create trace file.");
    }
    setbuf(open_result, WRBUF.as_mut_ptr());

    open_result
}