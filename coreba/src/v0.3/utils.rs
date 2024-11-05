pub use std::{
    rc::Rc,
    sync::{Arc, Mutex},
    io::{BufRead, BufReader},
    collections::{HashMap, BinaryHeap, BTreeSet, HashSet},
    path::PathBuf,
    iter::Peekable,
    hash::Hash,
    backtrace::Backtrace,
    cell::Cell,
    time::Instant,
};
pub use thiserror::Error;
pub use itertools::Itertools;
pub use rayon::prelude::*;

pub use crate::{Instance, Job,
    jobset::*,
};

/// The unit for measuring logical time. `idealloc` does not care about
/// semantics, as long as the liveness invariant (see [`Job`]) is preserved.
///
/// That said, one might object against our decision to use the same
/// type for both job sizes and their lifetimes. Many such objections
/// may hold merit! We may publish a trait-based version in the future.
///
/// TODO: Make the fact that we're designing with 64bit arch in mind explicit.
pub type ByteSteps = usize;

/// A group of jobs, sorted in order of increasing birth.
///
/// This is arguably the most commonly occuring abstraction in
/// `idealloc`.
// TODO: Now that I've introduced IDs, isn't it smarter to switch from
// `Vec` to `BTreeSet`? Investigate!
pub type JobSet = Vec<Arc<Job>>;
// `Arc` is needed for parallelism.

/// Defines the interface for reading jobs.
///
/// For example: we will write a type that implements [JobGen]
/// and reads a `minimalloc`-style CSV. We will write another
/// type that reads from a Linux-born `.trc` binary file.
///
/// The user can implement their own types as needed.
pub trait JobGen<T> {
    /// Either a set of jobs is successfully returned, or some
    /// arbitrary type that implements [std::error::Error].
    fn read_jobs(&self) -> Result<Vec<Job>, Box<dyn std::error::Error>>;
    /// Uses some available data to spawn one [Job]. We do not put
    /// any limitations on what that data may look like.
    fn gen_single(&self, d: T, id: u32) -> Job;
}

#[derive(Error, Debug)]
#[error("{message}\n{:?}", culprit)]
/// Appears while constructing the [JobSet] of *original*
/// jobs to be dealt with.
pub struct JobError {
    pub message: String,
    pub culprit: Job,
}

//---START EXTERNAL INTERFACES
// The types listed below implement interfaces to several
// data sources for `idealloc`.
//
// To write your own interface, simply make sure that it
// satisfies the `JobGen` trait.

/// We adopt [`minimalloc`'s CSV](https://github.com/google/minimalloc)
/// as the most standard format.
pub struct MinimalloCSVParser {
    pub path: PathBuf,
}

impl MinimalloCSVParser {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
        }
    }
}

impl JobGen<&[ByteSteps; 3]> for MinimalloCSVParser {
    fn read_jobs(&self) -> Result<Vec<Job>, Box<dyn std::error::Error>> {
        let mut res = vec![];
        let mut data_buf: [ByteSteps; 3] = [0; 3];
        let mut next_id = 0;

        let path = self.path
            .as_path();

        match std::fs::metadata(path) {
            Ok(_)   => {
                let fd = std::fs::File::open(path)?;
                let reader = BufReader::new(fd);
                for line in reader.lines()
                    // First line is the header!
                    .skip(1) {
                    for (idx, data) in line?.split(',')
                        // First column is the id!
                        .skip(1)
                        .take(3)
                        .map(|x| {
                            if let Ok(v) = usize::from_str_radix(x, 10) { v }
                            else { panic!("Error while parsing CSV."); }
                        }).enumerate() {
                            data_buf[idx] = data;
                    }
                    res.push(self.gen_single(&data_buf, next_id));
                    next_id += 1;
                }
            },
            Err(e)  => { return Err(Box::new(e)); }
        };

        Ok(res)
    }

    fn gen_single(&self, d: &[ByteSteps; 3], id: u32) -> Job {
        Job {
            size:               d[2],
            birth:              d[0],
            death:              d[1],
            req_size:           d[2],
            alignment:          None,
            contents:           None,
            originals_boxed:    0,
            id
        }        
    }
}

//---END EXTERNAL INTERFACES

//---START PLACEMENT PRIMITIVES
/// A [Job] which has been assigned an offset in
/// some contiguous address space.
pub struct PlacedJob {
    pub descr:          Arc<Job>,
    pub offset:         Cell<ByteSteps>,
    // The `times_placed` field is owed to `idealloc`'s
    // iterative nature as well as the requirement for
    // high-performance squeezing: by keeping track of
    // how many times a [PlacedJob] has been squeezed,
    // we can quickly filter the interference graph during
    // best-fit.
    pub times_squeezed: Cell<u32>,
}

impl PlacedJob {
    pub fn new(descr: Arc<Job>) -> Self {
        Self {
            descr,
            offset:         Cell::new(0),
            times_squeezed: Cell::new(0),
        }
    }

    pub fn next_avail_addr(&self) -> ByteSteps {
        self.offset.get() + self.descr.size
    }
}

// The INTERMEDIATE result of unboxing, that is, a first
// loose placement, will be a min-heap on the jobs' offsets.
impl Ord for PlacedJob {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.offset.cmp(&self.offset)
    }
}

impl PartialOrd for PlacedJob {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for PlacedJob {
    fn eq(&self, other: &Self) -> bool {
        *self.descr == *other.descr
    }
}

impl Eq for PlacedJob {}

impl Hash for PlacedJob {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // A `PlacedJob` is hashed according to the
        // underlying `Job` ID.
        self.descr.hash(state);
    }
}

// No `Arc` needed here, since we shall
// work single-threadedly.
pub type PlacedJobSet = Vec<Rc<PlacedJob>>;

/// A map which holds, for each [PlacedJob], the subset of
/// jobs which are temporally overlapping with it.
pub type InterferenceGraph = HashMap<Rc<PlacedJob>, PlacedJobSet>;
pub type PlacedJobRegistry = HashMap<u32, Rc<PlacedJob>>;

/// A min-heap on the offsets of jobs, to be passed for squeezing.
pub type LoosePlacement = BinaryHeap<Rc<PlacedJob>>;

pub enum UnboxCtrl {
    SameSizes(ByteSteps),
    NonOverlapping,
    Unknown,
}
//---END PLACEMENT PRIMITIVES