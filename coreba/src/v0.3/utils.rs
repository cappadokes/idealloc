pub use std::{
    sync::Arc,
    io::{BufRead, BufReader},
    collections::{HashMap, BinaryHeap},
    path::PathBuf,
};
pub use thiserror::Error;
pub use itertools::Itertools;

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
pub type JobSet = Vec<Arc<Job>>;
// `Arc` is needed for parallelism.

/// The boxing procedure is governed by
/// this floating-point value.
///
/// It is the ε in the "2+ε" bound provided
/// by Buchsbaum et al.
pub struct Epsilon {
    pub val:    f32,
    last:       u32,
    limit:      f32,
}

impl Epsilon {
    pub fn new(jobs: &mut Instance) -> Self {
        Self {
            val:    jobs.init_e(),
            last:   0,
            limit:  0.0,
        }
    }

    pub fn update(&mut self, ns: u32) {
        if ns > self.last {
            self.limit = self.val;
        }
        // Previous implementation was incrementing ε
        // by 1%. We care about faster convergence, so
        // we 10x the increase rate.
        self.val *= 1.1;
        self.last = ns;

        if self.limit > 0.0 && self.val > 2.0 * self.limit {
            self.val = self.limit;
        }
    }
}

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
    fn gen_single(&self, d: T) -> Job;
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
                    res.push(self.gen_single(&data_buf));
                }
            },
            Err(e)  => { return Err(Box::new(e)); }
        };

        Ok(res)
    }

    fn gen_single(&self, d: &[ByteSteps; 3]) -> Job {
        Job {
            size:               d[2],
            birth:              d[0],
            death:              d[1],
            req_size:           d[2],
            alignment:          None,
            contents:           None,
            originals_boxed:    0,
        }        
    }
}

//---END EXTERNAL INTERFACES