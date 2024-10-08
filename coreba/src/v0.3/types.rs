// `use crate::Job` below is used only for the link to Job
// to be made available to the documentation comment.
#![allow(unused_imports)]

use crate::Job;
/// The unit for measuring logical time. `idealloc` does not care about
/// semantics, as long as the liveness invariant (see [`Job`]) is preserved.
///
/// That said, one might object against our decision to use the same
/// type for both job sizes and their lifetimes. Many such objections
/// may hold merit! We will publish a trait-based version in the future.
///
/// TODO: Make the fact that we're designing with 64bit arch in mind explicit.
pub type ByteSteps = usize;
