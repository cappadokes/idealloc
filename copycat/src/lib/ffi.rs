// C-reminiscent types.
pub type size_t     = libc::size_t;
pub type pthread_t  = libc::pthread_t;
pub type pid_t      = libc::pid_t;
pub type void       = libc::c_void;
pub type int        = libc::c_int;
type char           = libc::c_char;

// Linux utils.
pub use libc::{
    RTLD_NEXT,
    dlsym,
    dlerror,
    fputs,
    exit,
    pthread_self,
};

// Type aliases for the several interposed functions.
pub type CMalloc        = unsafe extern "C" fn(size_t)                              -> *mut void;
pub type CFree          = unsafe extern "C" fn(*mut void);
pub type CCalloc        = unsafe extern "C" fn(nobj: size_t, size: size_t)          -> *mut void;
pub type CRealloc       = unsafe extern "C" fn(p: *mut void, size: size_t)          -> *mut void;
pub type CAlignedAlloc  = unsafe extern "C" fn(alignment: size_t, size: size_t)     -> *mut void;
pub type CMemalign      = unsafe extern "C" fn(align: size_t, size: size_t)         -> *mut void;
pub type CPosixMemalign = unsafe extern "C" fn( memptr: *mut *mut void, 
                                                align:  size_t, 
                                                size:   size_t)                     -> int;

pub fn cstring_noalloc(c_buf: &mut [char], rust_buf: &[u8]) {
    //! Writes a pre-allocated byte literal to a pre-allocated
    //! `i8` literal. Used due to the `libc` crate treating characters
    //! as `i8`.
    //! For use in cases where no extra allocations should be
    //! triggered (as in the interposer's code, for instance).
    //! Else [`CString`](https://doc.rust-lang.org/std/ffi/struct.CString.html) is the go-to way. 
    
    let chars_num = rust_buf.len();
    for i in 0..chars_num {
        c_buf[i] = rust_buf[i] as char;
    }
    // C strings are terminated by the "null" character.
    c_buf[chars_num] = '\0' as char;
}


pub unsafe fn graceful_exit(err_msg: &str) -> ! {
    //! Prints some error message on stderr, then
    //! exits with non-zero code.
        
    crate::logic::steal_ownership();
    eprintln!("{}", err_msg);
    exit(1);
}

pub unsafe fn check_dlerror() {
    //! Checks result of `dlsym`.
    use libc_stdhandle::stderr;

    let eptr: *mut char = dlerror();
    if !eptr.is_null() {
        fputs(eptr as *const char, stderr());
        exit(1);
    }
}

#[macro_export]
macro_rules! make_cstring {
    // Returns a pre-allocated C string.
    ($text:literal) => {
        match $text {
            _lit @ &_  => {
                const RUST_BUF: &str = $text;
                // Extra space for '\0' terminating character.
                let mut c_buf = [' ' as libc::c_char; RUST_BUF.len() + 1];
                cstring_noalloc(&mut c_buf, RUST_BUF.as_bytes());

                c_buf
            }
        }
    };
}