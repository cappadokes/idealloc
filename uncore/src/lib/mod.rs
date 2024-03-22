#[allow(non_camel_case_types)]
/// Crate is targeting Linux user-space processes which
/// invoke `malloc` and friends. We're doing it via
/// [*function interposition*](https://stackoverflow.com/questions/426230/what-is-the-ld-preload-trick).
/// We thus need the corresponding set of signatures for the
/// functions that are to be interposed.
/// 
/// This module defines said signatures, along with some assistant
/// type aliases (e.g. `void` instead of `c_void`).
mod ffi;

mod logger;

/// Contains actual interposing code.
mod tracer;

mod mapfind;