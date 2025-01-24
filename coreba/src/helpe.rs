pub use std::{
    rc::Rc,
    sync::{Arc, Mutex},
    io::{BufRead, BufReader, Read},
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
pub use indexmap::IndexMap;
pub use clap::{Parser, ValueEnum};

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

/// Defines the interface for reading jobs.
///
/// For example: we will write a type that implements [JobGen]
/// and reads a `minimalloc`-style CSV. We will write another
/// type that reads from a Linux-born `.trc` binary file.
///
/// The user can implement their own types as needed.
pub trait JobGen<T> {
    fn new(path: PathBuf) -> Self;
    /// Either a set of jobs is successfully returned, or some
    /// arbitrary type that implements [std::error::Error].
    fn read_jobs(&self, shift: ByteSteps) -> Result<Vec<Job>, Box<dyn std::error::Error>>;
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

pub struct PLCParser {
    pub path: PathBuf,
}

pub const PLC_FIELDS_NUM: usize = 8;

impl JobGen<&[u8; 8 * PLC_FIELDS_NUM]> for PLCParser {
    fn new(path: PathBuf) -> Self {
        Self {
            path
        }
    }
    fn gen_single(&self, d: &[u8; 8 * PLC_FIELDS_NUM], _: u32) -> Job {
        let mut words_read = 0;
        let mut baby_job = Job::new();
        while words_read < PLC_FIELDS_NUM {
            let mut word_buffer: [u8; 8] = [0; 8];
            for byte_count in 0..8 {
                word_buffer[byte_count] = d[words_read * 8 + byte_count];
            }
            words_read += 1;
            let data = usize::from_be_bytes(word_buffer);
            match words_read {
                1   => { baby_job.id = data.try_into().unwrap(); },
                2   => { baby_job.birth = data; },
                3   => { baby_job.death = data; },
                4   => { baby_job.size = data; },
                5   => {},
                6   => {},
                7   => { if data != 0 { baby_job.alignment = Some(data); }},
                8   => { baby_job.req_size = data; },
                _   => { panic!("Unreachable state while parsing PLC."); }
            }
        }

        baby_job
    }
    fn read_jobs(&self, _: ByteSteps) -> Result<Vec<Job>, Box<dyn std::error::Error>> {
        let path = self.path.as_path();
        let mut res = vec![];
        match std::fs::metadata(path) {
            Ok(_)   => {
                let fd = std::fs::File::open(path)?;
                let mut reader = BufReader::new(fd);
                let mut buffer: [u8; 8 * PLC_FIELDS_NUM] = [0; 8 * PLC_FIELDS_NUM];
                while let Ok(_) = reader.read_exact(&mut buffer) {
                    res.push(self.gen_single(&buffer, 62));
                }
            },
            Err(e)  => { return Err(Box::new(e)); }
        }

        Ok(res)
    }
}

/// We adopt [`minimalloc`'s CSV](https://github.com/google/minimalloc)
/// as the most standard format.
pub struct MinimalloCSVParser {
    pub path: PathBuf,
}

impl JobGen<&[ByteSteps; 3]> for MinimalloCSVParser {
    fn new(path: PathBuf) -> Self {
        Self {
            path,
        }
    }
  
    fn read_jobs(&self, _: ByteSteps) -> Result<Vec<Job>, Box<dyn std::error::Error>> {
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

/// A helper type for parsing IREE-born buffers stored into
/// the standard minimalloc CSV format.
/// 
/// We introduce this additional type because IREE adopts
/// *inclusive* semantics on both ends of a buffer's lifetime.
/// Thus conversion is needed.
pub struct IREECSVParser {
    pub dirty:  PathBuf,
}

impl JobGen<Job> for IREECSVParser {
    fn new(dirty: PathBuf) -> Self {
        Self {
            dirty,
        }
    }

    fn read_jobs(&self, shift: ByteSteps) -> Result<Vec<Job>, Box<dyn std::error::Error>> {
        let helper = MinimalloCSVParser::new(self.dirty.clone());
        let dirty_jobs: JobSet = helper.read_jobs(0)
            .unwrap()
            .into_iter()
            .map(|j| Arc::new(j))
            .collect();
        let mut evts = get_events(&dirty_jobs);
        // Increased by 1 at every first death after a birth.
        let mut num_generations = 0;
        // Helper var for increasing generations.
        let mut last_evt_was_birth = true;
        // Collects "transformed" jobs.
        let mut res = vec![];
        // Keeps live jobs, indexed by ID.
        let mut live: HashMap<u32, Job> = HashMap::new();
        while let Some(e) = evts.pop() {
            match e.evt_t {
                EventKind::Birth    => {
                    if !last_evt_was_birth {
                        last_evt_was_birth = true;
                    }
                    live.insert(
                        e.job.id,
                        Job {
                            size:               e.job.size,
                            birth:              e.job.birth + num_generations,
                            death:              e.job.death + num_generations + shift,
                            req_size:           e.job.size,
                            alignment:          None,
                            contents:           None,
                            originals_boxed:    0,
                            id:                 e.job.id,
                        }
                    );
                },
                EventKind::Death    => {
                    if last_evt_was_birth {
                        num_generations += 1;
                        last_evt_was_birth = false;
                    };
                    res.push(live.remove(&e.job.id).unwrap());
                },
            }
        };

        Ok(res)
    }
    fn gen_single(&self, d: Job, _id: u32) -> Job {
        d
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
    #[inline(always)]
    pub fn overlaps_with(&self, other: &Self) -> bool {
        self.descr.birth < other.descr.death &&
        other.descr.birth < self.descr.death
    }

    #[inline(always)]
    pub fn new(descr: Arc<Job>) -> Self {
        Self {
            descr,
            offset:         Cell::new(0),
            times_squeezed: Cell::new(0),
        }
    }

    #[inline(always)]
    pub fn next_avail_offset(&self) -> ByteSteps {
        self.offset.get() + self.descr.size
    }

    /// Returns an appropriately aligned
    /// offset for the job.
    #[inline(always)]
    pub fn get_corrected_offset(
        &self, 
        start_addr: ByteSteps,
        cand:       ByteSteps
    ) -> ByteSteps {
        if let Some(a) = self.descr.alignment {
            let cand_addr = start_addr + cand;
            if cand_addr == 0 || cand_addr % a == 0 { cand }
            else if cand_addr < a {
                a - start_addr
            } else {
                (cand_addr / a + 1) * a - start_addr
            }
        } else { cand }
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
pub type InterferenceGraph = HashMap<u32, PlacedJobSet>;
pub type PlacedJobRegistry = HashMap<u32, Rc<PlacedJob>>;

/// A min-heap on the offsets of jobs, to be passed for squeezing.
pub type LoosePlacement = BinaryHeap<Rc<PlacedJob>>;

pub enum UnboxCtrl {
    SameSizes(ByteSteps),
    NonOverlapping,
    Unknown,
}
//---END PLACEMENT PRIMITIVES

pub enum AnalysisResult {
    NoOverlap(JobSet),
    SameSizes(JobSet, InterferenceGraph, PlacedJobRegistry),
    NeedsBA(BACtrl),
}

pub struct BACtrl {
    pub input:      Rc<Instance>,
    pub pre_boxed:  Rc<Instance>,
    pub epsilon:    f64,
    pub to_box:     usize,
    pub real_load:  ByteSteps,
    pub dummy:      Option<Arc<Job>>,
    pub ig:         InterferenceGraph,
    pub reg:        PlacedJobRegistry,
    pub mu_lim:     f64,
    pub best_opt:   ByteSteps,
    pub hardness:   (f64, f64, f64),
}

/// Helper structure for Theorem 2.
pub struct T2Control {
    pub bounding_interval:  (ByteSteps, ByteSteps),
    pub critical_points:    BTreeSet<ByteSteps>,
}

impl T2Control {
    pub fn new(jobs: &Instance) -> Self {
        let (start, end) = jobs.get_horizon();
        debug_assert!(start < end, "Same-ends horizon met.");
        let mid = Self::gen_crit(jobs, start, end);

        Self {
            bounding_interval:  (start, end),
            critical_points:    BTreeSet::from([start, end, mid]),
        }
    }

    /// Generates a random number within (left, right) at which
    /// at least one piece in `jobs` is live.
    #[inline(always)]
    pub fn gen_crit(
        jobs:   &Instance, 
        left:   ByteSteps, 
        right:  ByteSteps
    ) -> ByteSteps {
        // What follows is the simplest, most naive, but also
        // most safe implementation of `gen_crit`.
        use rand::{Rng, thread_rng};

        debug_assert!(left + 1 < right, "Bad range found.");
        let mut pts: Vec<ByteSteps> = vec![];
        let mut evts = get_events(&jobs.jobs);
        while let Some(evt) = evts.pop() {
            let cand = match evt.evt_t {
                // All jobs have lifetimes at least 2,
                // so this is safe.
                //
                // At least one job must be live in each
                // candidate point, so we add/subtract 1
                // in case of birth/death.
                EventKind::Birth    => { evt.time + 1 },
                EventKind::Death    => { evt.time - 1 }
            };
            if cand > left && cand < right {
                pts.push(cand);
            }
        };

        // Rust ranges (x..y) are low-inclusive, upper-exclusive.
        pts[thread_rng().gen_range(0..pts.len())]
    }
}

#[derive(PartialEq, Eq, Clone)]
/// An [Event] is either a birth or a death.
pub enum EventKind {
    Birth,
    Death,
}

#[derive(Eq, Clone)]
pub struct Event {
    pub job:    Arc<Job>,
    pub evt_t:  EventKind,
    // Copy time here to elude pattern matching during
    // comparison.
    pub time:   ByteSteps,
}

/// Traversal of a [JobSet] can be thought as an ordered stream
/// of events, with increasing time of occurence. Each [Job] generates
/// two events, corresponding to the start/end of its lifetime
/// respectively.
/// 
/// We use these events to calculate things such as maximum load,
/// interference graphs, fragmentation, critical points, etc.
pub type Events = BinaryHeap<Event>;

impl Ord for Event {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We're using a BinaryHeap, which is
        // a max-priority queue. We want a min-one
        // and so we're reversing the order of `cmp`.
        other.time.cmp(&self.time)
            .then(
                if self.evt_t == other.evt_t {
                    std::cmp::Ordering::Equal
                } else {
                    match self.evt_t {
                        // Prioritize deaths over births.
                        EventKind::Birth    => { std::cmp::Ordering::Less },
                        EventKind::Death    => { std::cmp::Ordering::Greater },
                    }
                })
    }
}
impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time
    }
}

/// Helper struct for Lemma 1. "Vertical strips" are going
/// to be stored into [BinaryHeap]s. Since [BinaryHeap] is
/// a max-heap, we compare 2 jobs' deaths to order them.
#[derive(Eq)]
pub struct VertStripJob {
    pub job:    Arc<Job>,
}

impl New for VertStripJob {
    fn new(job: Arc<Job>) -> Self {
        Self { job }
    }
    #[inline(always)]
    fn get_inner_job(self) -> Arc<Job> {
        self.job
    }
}

impl Ord for VertStripJob {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.job
            .death
            .cmp(&other.job.death)
    }
}

impl PartialOrd for VertStripJob {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for VertStripJob {
    fn eq(&self, other: &Self) -> bool {
        self.job == other.job
    }
}

pub trait New {
    fn new(src: Arc<Job>) -> Self;
    fn get_inner_job(self) -> Arc<Job>;
}

/// Helper struct for Lemma 1. "Vertical strips" are going
/// to be stored into [BinaryHeap]s. Since [BinaryHeap] is
/// a max-heap, we compare 2 jobs in reverse to order them.
#[derive(Eq)]
pub struct HorStripJob {
    pub job:    Arc<Job>,
}

impl New for HorStripJob {
    fn new(job: Arc<Job>) -> Self {
        Self { job }
    }
    #[inline(always)]
    fn get_inner_job(self) -> Arc<Job> {
        self.job
    }
}

impl Ord for HorStripJob {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.job
            .cmp(&self.job)
    }
}

impl PartialOrd for HorStripJob {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for HorStripJob {
    fn eq(&self, other: &Self) -> bool {
        self.job == other.job
    }
}

/// Helper function for Lemma 1. Splits all inner
/// strips into boxes containing `group_size` jobs each.
/// 
/// The real size of each box is `box_size`.
#[inline(always)]
pub fn strip_boxin(
    verticals:      Vec<BinaryHeap<VertStripJob>>,
    horizontals:    Vec<BinaryHeap<HorStripJob>>,
    group_size:     ByteSteps,
    box_size:       ByteSteps,
) -> JobSet {
    let mut res_set = strip_box_core(verticals, group_size, box_size);
    res_set.append(&mut strip_box_core(horizontals, group_size, box_size));

    res_set
}

/// Helper function for Lemma 1. Splits the jobs
/// of a single strip into boxes containing `group_size` jobs each.
/// 
/// The real size of each box is `box_size`.
#[inline(always)]
fn strip_box_core<T>(
    strips:         Vec<BinaryHeap<T>>,
    group_size:     ByteSteps,
    box_size:       ByteSteps,
) -> JobSet where T: Ord + New {
    let mut res: JobSet = vec![];
    let mut buf: JobSet = vec![];
    for mut strip in strips {
        // We must repeatedly divide each strip in groups
        // of size `group_size` and box them.
        let mut stripped = 0;
        while !strip.is_empty() {
            buf.push(strip.pop().unwrap().get_inner_job());
            stripped += 1;
            if stripped == group_size || strip.is_empty() {
                res.push(Arc::new(Job::new_box(buf, box_size)));
                buf = vec![];
                stripped = 0;
            }
        }
    };

    res
}

/// Helper function for Lemma 1. Selects `to_take`
/// jobs from a given iterator.
#[inline(always)]
pub fn strip_cuttin<T>(
    source:     &mut IndexMap<u32, Arc<Job>>,
    mirror:     &mut IndexMap<u32, Arc<Job>>,
    to_take:    ByteSteps,
) -> BinaryHeap<T> where T: Ord + New {
    let mut res: BinaryHeap<T> = BinaryHeap::new();
    let mut stripped = 0;
    while stripped < to_take && !source.is_empty() {
        let (id, cut_job) = source.pop().unwrap();
        mirror.shift_remove(&id).unwrap();
        res.push(New::new(cut_job));
        stripped += 1;
    }

    res
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum InpuType {
    /// A CSV file using the minimalloc benchmarks format (exclusive endpoints)
    ExCSV,
    /// A CSV file using the minimalloc benchmarks format (start-inclusive, end-exclusive endpoints)
    InExCSV,
    /// A CSV file using the minimalloc benchmarks format (inclusive endpoints)
    InCSV,
    /// An `idealloc`-native, binary-encoded file, produced by the `adapt` tool
    PLC,
    /// An `idealloc`-native, binary-encoded file produced by tracing Linux programs
    TRC,
}

pub fn read_from_path<T, B>(file_path: PathBuf, shift: ByteSteps) -> Result<JobSet, Box<dyn std::error::Error>> 
where T: JobGen<B> {
    let parser = T::new(file_path);
    let jobs = parser.read_jobs(shift)?;
    assert!(jobs.len() > 0);
    let set = crate::jobset::init(jobs)?;

    Ok(set)
}