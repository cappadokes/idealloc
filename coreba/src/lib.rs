//! Welcome to `idealloc`!

mod job;
mod instance;
mod analyze;

pub mod algo;
pub mod jobset;
pub mod helpe;

pub use crate::helpe::*;

/// Our fundamental unit of interest. A [`Job`] is a complete description
/// of the events triggered by the need for some memory:
///
/// 1. [`size`](Job::size) bytes were *allocated* at logical time [`birth`](Job::birth).
///     The originally *requested* size, plus alignment info, is stored in private fields.
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
#[derive(Debug, Clone)]
pub struct Job {
    // The bigger the type, the wider a variety of workloads is allowable.
    // This is the *ALLOCATED* size of the job! As it moves along the boxing
    // pipeline, it will sometimes be viewed as having a different height--
    // but the absolute truth is this.
    //
    // Earlier implementation had a "current" size field which had to be
    // somehow mutable--in turn, this ruined our chances for parallelism.
    pub size:           ByteSteps,
    pub birth:          ByteSteps,
    pub death:          ByteSteps,
    pub req_size:       ByteSteps,
    pub alignment:      Option<ByteSteps>,
    /// They user may not care, but `idealloc`'s core operation is boxing
    /// jobs together recursively. A very common interface is (i) consuming
    /// a set of jobs and (ii) producing a *new* set, its elements containing
    /// subgroups of the input set.
    ///
    /// The boxing algorithm does not differentiate between "original" jobs
    /// that contain nothing and "spawned" jobs that contain at least one
    /// job. "Everything is a [`Job`]."
    pub contents:       Option<JobSet>,
    // Used for debugging mostly.
    pub originals_boxed:u32,
    pub id:             u32,
}

/// The entity consumed and produced by the majority of
/// `idealloc`'s components. On the highest level, `idealloc`
/// creates an input [`Instance`] comprising *unplaced* jobs,
/// puts it through the boxing pipeline, culminating into a
/// transformed [`Instance`] of *still unplaced* jobs, and then
/// unboxes and performs the actual placement.
#[derive(Clone)]
pub struct Instance {
    jobs: JobSet,
    info: instance::Info,
}