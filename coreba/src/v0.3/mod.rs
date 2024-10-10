//! Welcome to `idealloc`!

mod space;
mod job;
mod instance;
pub mod algo;
pub mod utils;
pub mod jobset;

/// Imports, type aliases, traits ... in general
/// useful stuff that shall be needed in many places.
use crate::utils::*;

/// Our fundamental unit of interest. A [`Job`] is a complete description
/// of the events triggered by the need for some memory:
///
/// 1. [`size`](Job::size) bytes were *allocated* at logical time [`birth`](Job::birth).
///     The originally *requested* size, plus some further info, is
///     stored in [`home`](Job::home).
/// 2. The memory was *deallocated* at logical time [`death`](Job::death).
///
/// > ***ATTENTION:*** One must at all times be cognizant of their
/// jobs' *liveness semantics*, that is, the boundary conditions w.r.t.
/// how memory behaves at [Job::birth] and [Job::death]. Assume the sorted
/// range of natural numbers [birth, death]: it is easy to reason about
/// values *between* the two extremes (memory is obviously live), but it's
/// equally easy to change your assumptions about either one or both tips
/// of the range.
/// >
/// > In `idealloc`, memory is **not live** at the extremes. If a job is born
/// > at the same time that another job dies, they could share the
/// > same offset.
#[derive(Eq, Debug)]
pub struct Job {
    // The bigger the type, the wider a variety of workloads is allowable.
    // This is the *CURRENT* size of the job! It will vary as it moves
    // along the algorithm's pipeline!
    pub size:           Cell<ByteSteps>,
    pub birth:          ByteSteps,
    pub death:          ByteSteps,
    pub home:           Cell<space::Placement>,
    /// They user may not care, but `idealloc`'s core operation is boxing
    /// jobs together recursively. A very common interface is (i) consuming
    /// a set of jobs and (ii) producing a *new* set, its elements containing
    /// subgroups of the input set.
    ///
    /// The boxing algorithm does not differentiate between "original" jobs
    /// that contain nothing and "spawned" jobs that contain at least one
    /// job. "Everything is a [`Job`]."
    contents:           Option<JobSet>,
    // Used during calibrating epsilon.
    originals_boxed:    u32,
}

/// The entity consumed and produced by the majority of
/// `idealloc`'s components. On the highest level, `idealloc`
/// creates an input [`Instance`] comprising *unplaced* jobs,
/// puts it through the boxing pipeline, culminating into a
/// transformed [`Instance`] of *still unplaced* jobs, and then
/// unboxes and performs the actual placement.
#[derive(Clone)]
pub struct Instance {
    // To avoid expensive allocations when cloning at the
    // beginning of each next iteration.
    jobs: Rc<JobSet>,
    info: instance::Info,
}