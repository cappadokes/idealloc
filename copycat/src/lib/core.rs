use crate::ffi::*;
use crate::logic;
use crate::circus;

#[no_mangle]
/// Function interposition is used, in a similar vein to the
/// tracer, to catch all dynamic memory allocation functions.
/// 
/// Two scenarios exist: a request comes either from the application
/// itself, or from internal book-keeping needs of the copycat. In
/// the first case we reproduce the traced placement decision. In
/// the second one, we invoke a real `malloc`.
/// 
/// This logic holds across all 7 supported functions.
unsafe extern "C"
fn malloc(size: size_t) -> *mut void {
    if logic::is_app_owned() {
        circus::fake_malloc(size)
    } else {
        circus::true_malloc(size)
    }
}

#[no_mangle]
unsafe extern "C"
fn free(p: *mut void) {
    if logic::is_app_owned() {
        circus::fake_free(p)
    } else {
        circus::true_free(p)
    }
}

#[no_mangle]
unsafe extern "C"
fn calloc(nobj: size_t, size: size_t) -> *mut void {
    if logic::is_app_owned() {
        circus::fake_calloc(nobj, size)
    } else {
        circus::true_calloc(nobj, size)
    }
}

#[no_mangle]
unsafe extern "C"
fn realloc(p: *mut void, size: size_t)  -> *mut void {
    if logic::is_app_owned() {
        circus::fake_realloc(p, size)
    } else {
        circus::true_realloc(p, size)
    }
}

#[no_mangle]
unsafe extern "C"
fn aligned_alloc(alignment: size_t, size: size_t)  -> *mut void {
    if logic::is_app_owned() {
        circus::fake_aligned_alloc(alignment, size)
    } else {
        circus::true_aligned_alloc(alignment, size)
    }
}

#[no_mangle]
unsafe extern "C"
fn memalign(alignment: size_t, size: size_t)  -> *mut void {
    if logic::is_app_owned() {
        circus::fake_memalign(alignment, size)
    } else {
        circus::true_memalign(alignment, size)
    }
}

#[no_mangle]
unsafe extern "C"
fn posix_memalign(memptr: *mut *mut void, alignment: size_t, size: size_t)  -> int {
    if logic::is_app_owned() {
        circus::fake_posix_memalign(memptr, alignment, size)
    } else {
        circus::true_posix_memalign(memptr, alignment, size)
    }
}
