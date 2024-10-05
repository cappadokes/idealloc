//! Welcome to `idealloc`!

/// Helpful abbreviations in here.
mod types;
use types::*;

/// Functionality related to managing space
/// (that is, memory).
mod space;
use space::Placement;

use std::cell::Cell;
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
#[derive(Eq)]
pub struct Job {
    // The bigger the type, the wider a variety of workloads is allowable.
    pub size:   ByteSteps,
    pub birth:  ByteSteps,
    pub death:  ByteSteps,
    pub home:   Cell<Placement>,
    /// They user may not care, but `idealloc`'s core operation is boxing
    /// jobs together recursively. A very common interface is (i) consuming
    /// a set of jobs and (ii) producing a *new* set, its elements containing
    /// subgroups of the input set.
    /// 
    /// The boxing algorithm does not differentiate between "original" jobs
    /// that contain nothing and "spawned" jobs that contain at least one
    /// job. "Everything is a [`Job`]."
    contents:   Option<JobSet>,
    /// TODO: Not sure whether I need this field.
    id:         u32,
}

/// Houses [`Job`]-related implementation.
mod job;

use std::rc::Rc;
/// A group of jobs, sorted in order of increasing birth.
/// 
/// This is arguably the most commonly occuring abstraction in
/// `idealloc`.
#[derive(PartialEq, Eq)]
pub struct JobSet (Vec<Rc<Job>>);

/// Houses [`JobSet`]-related implementation.
mod jobset;

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
    jobs:               Rc<JobSet>,
    // Used during calibrating epsilon.
    originals_boxed:    u32,
    load:               Option<ByteSteps>,
}

mod instance;

mod algo;
pub use algo::main_loop;

mod utils;