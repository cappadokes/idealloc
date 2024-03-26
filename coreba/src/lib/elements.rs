use std::{cell::{Cell, RefCell}, cmp::Ordering, hash::BuildHasherDefault, rc::Rc};
use indexmap::IndexMap;
use ahash::AHasher;
pub use crate::io::{BufReader, File, Read, Result, Error, BufWriter};
use crate::algo::{do_placement, LilEGen, PlacedSet};

type Event = (Request, Option<(Rc<Job>, Option<Rc<Job>>)>);

/// A request may have one of 7 types, accompanied by
/// the type's arguments--all of which have been stored
/// as `usize`-length in the trace file.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ReqType {
    Malloc          (usize),
    Free            (usize),
    Calloc          (usize, usize),
    ReAlloc         (usize, usize),
    AlignedAlloc    (usize, usize),
    MemAlign        (usize, usize),
    PosixMemAlign   (usize, usize, usize),
    Done
}

impl ReqType {
    fn is_free(&self) -> bool {
        if let ReqType::Free(_) = self {
            true
        } else { 
            false 
        }
    }

    pub fn get_requested_size(&self) -> usize {
        match self {
            ReqType::Malloc(s)  => { *s },
            ReqType::Calloc(nobj, osize)    => {*nobj * *osize },
            ReqType::ReAlloc(_, s)  => { *s },
            ReqType::AlignedAlloc(_, s) => { * s},
            ReqType::MemAlign(_, s) => { *s },
            ReqType::PosixMemAlign(_, _, s) => { *s },
            _   => { panic!("Requested size of non-allocation request."); }
        }
    }

    pub fn get_sentinel(&self) -> u8 {
        match self {
            ReqType::Malloc(_)  => { 0x05 },
            ReqType::Free(_)    => { 0x12 },
            ReqType::Calloc(_, _)   => { 0x26 },
            ReqType::ReAlloc(_, _)  => { 0x36 },
            ReqType::AlignedAlloc(_, _) => { 0x46 },
            ReqType::MemAlign(_, _) => { 0x56 },
            ReqType::PosixMemAlign(_, _, _) => { 0x67 },
            ReqType::Done   => { panic!("Attempted to write Done req."); }
        }
    }
}

// Exploit trace packet's first sentinel byte to swiftly index into
// the correct request type.
const CODES: [ReqType; 7] = [
    ReqType::Malloc(0),
    ReqType::Free(0),
    ReqType::Calloc(0, 0),
    ReqType::ReAlloc(0, 0),
    ReqType::AlignedAlloc(0, 0),
    ReqType::MemAlign(0, 0),
    ReqType::PosixMemAlign(0, 0, 0),
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Placement {
    pub heap:       usize,
    // A job may be placed or unplaced.
    // It certainly belongs to some heap,
    // and has some alignment.
    pub offset:     Option<usize>,
    pub alignment:  usize,
    pub req_size:   usize
}

impl Placement {
    pub fn new(heap: usize, o: usize, alignment: usize, req_size: usize) -> Self {
        Self { 
            heap, 
            offset: Some(o), 
            alignment,
            req_size
        }
    }
}

fn compute_proper_size(cand: usize) -> usize {
    // Experiments with the `--diss` flag exposed
    // segfaults when completely abiding to a program's
    // requested sizes. This is an attempt to investigate
    // whether allowing only size classes of multiples of
    // 8 or 16 is what's missing.
    const MIN_SIZE_CLASS: usize = 8;

    if cand % MIN_SIZE_CLASS != 0 {
        MIN_SIZE_CLASS * (cand / MIN_SIZE_CLASS + 1)
    } else {
        cand
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Request {
    // Info held matches the TRACED data!
    pub rtype:  ReqType,
    pub tid:    usize
}

impl Request {
    fn new_from_plc(world: &mut World, plc_source: &mut BufReader<File>, diss: bool) -> Option<()> {
        const PLC_FIELDS_NUM: usize = 8;
        let mut buffer: [u8; 8 * PLC_FIELDS_NUM] = [0; 8 * PLC_FIELDS_NUM];
        let mut baby_job = Job::new();
        match plc_source.read_exact(&mut buffer) {
            Ok(_)   => {
                let mut words_read = 0;
                while words_read < PLC_FIELDS_NUM {
                    let mut word_buffer: [u8; 8] = [0; 8];
                    for byte_count in 0..8 {
                        word_buffer[byte_count] = buffer[words_read * 8 + byte_count];
                    }
                    words_read += 1;
                    let data = usize::from_be_bytes(word_buffer);
                    match words_read {
                        1   => { baby_job.id = Cell::new(data); },
                        2   => { baby_job.birth = Cell::new(data); },
                        3   => { baby_job.death = Cell::new(data); },
                        4   => { baby_job.height = Cell::new((data, data)); },
                        5   => { baby_job.home.get_mut().heap = data; },
                        6   => { baby_job.home.get_mut().offset = Some(data); },
                        7   => { baby_job.home.get_mut().alignment = data; },
                        8   => { baby_job.home.get_mut().req_size = data; },
                        _   => { panic!("Reached unreachable state!"); }
                    }
                }
                baby_job.origin.set(baby_job.addr().unwrap());
                let j = Rc::new(baby_job);
                if world.all.contains(&j) {
                    panic!("Two jobs with same ID found.");
                } else {
                    if diss {
                        j.initialize_size(compute_proper_size(j.home.get().req_size));
                    }
                    world.all.insert_job(j.clone());
                }

                Some(())
            },
            Err(_)  => { None }
        }
    }
    fn _is_useless(&self) -> bool {
        match self.rtype {
            ReqType::Free(0)    => { true },
            _                   => { false }
        }
    }

    fn new_event(world: &mut World, trace: &mut BufReader<File>, running_id: &mut usize) -> Result<Event> {
        //! Spawns a new event from a trace file.
        let mut sentinel: [u8; 1] = [0];
        let mut baby_req = Self { rtype: ReqType::Done, tid: 0 };
        let mut baby_job = None;
        if let Ok(_) = trace.read_exact(&mut sentinel) {
            if  sentinel[0] != 0x05 &&
                sentinel[0] != 0x12 &&
                sentinel[0] != 0x26 &&
                sentinel[0] != 0x36 &&
                sentinel[0] != 0x46 &&
                sentinel[0] != 0x56 &&
                sentinel[0] != 0x67 {
                    return Err(Error::msg("Corrupted trace file."));
            }
            let req_type = CODES[((sentinel[0] & 0b11110000u8) >> 4) as usize];
            baby_req.rtype = req_type;
            let words_left = sentinel[0] & 0b00001111u8;
            // Buffer to get filled repeatedly.
            let mut word_buf: [u8; 8] = [0; 8];
            let mut i: u8 = 0;
            let mut tid: usize = 0;
            while i < words_left {
                trace.read_exact(&mut word_buf).expect("Unexpected trace EOF");
                let data = usize::from_be_bytes(word_buf);
                if i == words_left - 2 && !req_type.is_free() {
                    tid = data;
                }
                match baby_req.rtype {
                    ReqType::Malloc(ref mut size)  => {
                        if i == 0 { 
                            *size = data; 
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                        }
                    },
                    ReqType::Calloc(ref mut nobj, ref mut size)  => {
                        if i == 0 { 
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                            *nobj = data; 
                        }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::ReAlloc(ref mut p, ref mut size)  => {
                        if i == 0 { 
                            *p = data;
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some((_, ref mut inner)) = baby_job {
                                if *p != 0 {
                                    *inner = Some(world.live
                                    .remove(p)
                                    .unwrap());
                                }
                            };
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                        }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::AlignedAlloc(ref mut a, ref mut size) => {
                        if i == 0 { 
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                            *a = data; 
                        }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::MemAlign(ref mut a, ref mut size) => {
                        if i == 0 { 
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                            *a = data; 
                        }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::PosixMemAlign(ref mut p, ref mut a, ref mut size) => {
                        if i == 0 { 
                            *p = data; 
                            let got_alive = world.all
                                .contents
                                .get(running_id)
                                .unwrap()
                                .clone();
                            *running_id += 1;
                            baby_job = Some((got_alive.clone(), None));
                            if let Some(_) = world.live.insert(got_alive.addr().unwrap(), got_alive) {
                                panic!("bababa");
                            }
                            if let Some((_, ref mut inner)) = baby_job {
                                *inner = Some(world.live
                                .get(p)
                                .unwrap()
                                .clone());
                            };
                        }
                        else if i == 1 { *a = data; }
                        else if i == 2 { *size = data; }
                    },
                    ReqType::Free(ref mut p)   => {
                        if i == 0 { 
                            *p = data; 
                            if *p != 0 {
                                baby_job = Some((world.live
                                    .remove(p)
                                    .unwrap(), None));
                            } else {
                                baby_job = Some((Rc::new(Job::new()), None));
                            }
                        }
                        else { tid = data; }
                    },
                    ReqType::Done   => {
                        return Err(Error::msg("Unreachable block reached."));
                    },
                };
                i += 1;
            };
            baby_req.tid = tid;
        };

        Ok((baby_req, baby_job))
    }

    pub fn is_done(&self) -> bool {
        if let ReqType::Done = self.rtype { true }
        else { false }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Job {
    pub birth:      Cell<usize>,
    pub death:      Cell<usize>,
    // BA often downscales Jobs, and then upscales them to
    // boxes with different size than the initial one. We 
    // use 2 fields track which is the original, and which
    // the running size.
    pub height:     Cell<(usize, usize)>,
    // BA boxes jobs in larger boxes. These new
    // boxes share birth/death times w/ their
    // contents. Use the id field to differentiate.
    pub id:         Cell<usize>,
    pub home:       Cell<Placement>,
    pub contents:   RefCell<Option<LiveSet>>,
    pub origin:     Cell<usize>,
    pub just_boxes: Cell<bool>,
    pub just_jobs:  Cell<bool>,
    pub overlappin: Cell<bool>
}

// Used during tightening placements.
impl Ord for Job {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        other.offset()
            .cmp(&self.offset())
            .then(other.next_avail_addr()
                .cmp(&self.next_avail_addr()))
            // Avoid "equal" jobs in traversed BTreeSet.
            .then(other.id().cmp(&self.id()))
    }
}

impl PartialOrd for Job {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Job {
    #[inline]
    pub fn new() -> Self {
        Self {
            birth:      Cell::new(0),
            death:      Cell::new(0),
            height:     Cell::new((0, 0)),
            id:         Cell::new(0),
            home:       Cell::new(Placement::new(0, 0, 0, 0)),
            contents:   RefCell::new(None),
            origin:     Cell::new(0),
            just_boxes: Cell::new(true),
            just_jobs:  Cell::new(true),
            overlappin: Cell::new(false)
        }
    }

    #[inline]
    pub fn addr(&self) -> Result<usize> {
        if let Some(o) = self.home.get().offset {
            Ok(self.home.get().heap + o)
        } else { Err(Error::msg("Tried to retrieve address of unplaced job.")) }
    }

    pub fn unplace(&self) {
        let mut current_placement = self.home.get();
        current_placement.offset = None;
        self.home.set(current_placement);
    }

    #[inline]
    pub fn id(&self) -> usize { self.id.get() }
}

use crate::utils::HeapStats;

#[derive(Clone, Debug)]
pub struct JobSet {
    pub contents:   LiveSet,
    pub stats:      HeapStats,
    // JobSets come and go through BA's components.
    // In the process, new "Jobs" (boxes) are created.
    // To deal with this, we store the following fields.
    pub next_id:    usize,
    pub jobs_boxed: usize,
    pub just_boxes: bool,
    pub just_jobs:  bool,
    pub overlappin: bool
}

impl JobSet {
    #[inline]
    pub fn new() -> Self {
        Self {
            contents: IndexMap::default(),
            stats: HeapStats::new(),
            next_id: 0,
            jobs_boxed: 0,
            just_boxes: true,
            just_jobs:  true,
            overlappin: false
         }
    }

    #[inline]
    pub fn get_len(&self) -> usize {
        self.contents.len()
    }

    #[inline]
    pub fn insert_job(&mut self, j: Rc<Job>) {
        if let Some(oj) = self.contents.insert(j.id(), j.clone()) {
            panic!("{oj:?} and {j:?} have same id!");
        };
    }

    #[inline]
    pub fn contains(&self, candidate: &Rc<Job>) -> bool {
        self.contents.contains_key(&candidate.id())
    }
}

pub type LiveSet    = IndexMap<usize, Rc<Job>, BuildHasherDefault<AHasher>>;
pub type HeapSet    = IndexMap<usize, JobSet, BuildHasherDefault<AHasher>>;
pub type AddrSet    = IndexMap<usize, usize, BuildHasherDefault<AHasher>>;

pub struct World {
    pub next_id:    usize,
    // Indexed by address, used during trace parsing.
    live:           LiveSet,
    events:         Vec<Event>,
    // Indexed by ID, used in core part.
    all:            JobSet,
    best_addresses: AddrSet,
    magic:          RandTameLogic
}

impl World {
    pub fn new() -> Self {
        Self {
            next_id:        0,
            live:           IndexMap::default(),
            events:         vec![],
            all:            JobSet::new(),
            best_addresses: IndexMap::default(),
            magic:          RandTameLogic::new()
        }
    }

    pub fn give_back(mut self) -> Result<()> {
        let fd = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("optimized.plc")?;

        let mut writer = BufWriter::new(fd);

        for j in self.all.contents.values() {
            crate::io::update_plc(&mut writer, j, &self.best_addresses)?;
        }

        let fd = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open("optimized.trc")?;

        let mut writer = BufWriter::new(fd);

        self.live.clear();
        for (r, j) in self.events {
            crate::io::update_trace(&mut writer, r, j,&self.best_addresses)?;
        }

        Ok(())
    }

    pub fn populate(&mut self, mut plc: BufReader<File>, diss: bool, maybe_trc: Option<BufReader<File>>) -> Result<()> {
        // We want to tie PLC jobs to requests.
        while let Some(_) = Request::new_from_plc(self, &mut plc, diss) {
            self.next_id += 1;
        }
        if let Some(mut trace) = maybe_trc {
            let mut running_id = 0;
            loop {
                let (req, job) = Request::new_event(self, &mut trace, &mut running_id)?;
                if req.is_done() { 
                    break;
                }
                self.events.push((req, job));
            }
        }

        Ok(())
    }

    fn prepare_heaps(&self) -> HeapSet {
        let mut heaps: HeapSet = IndexMap::default();
        for (_, j) in &self.all.contents {
            // We needed the offsets while creating the jobs.
            // Now it's all about making new offsets via BA.
            // So we dispense the old ones.
            j.unplace();
            let key = j.home.get().heap;
            match heaps.get_mut(&key) {
                Some(set)   => { set.insert_job(j.clone()); },
                None    => {
                    let mut to_insert: JobSet = JobSet::new();
                    to_insert.insert_job(j.clone());
                    heaps.insert(key, to_insert);
                }
            }
        }

        heaps
    }

    pub fn optimize(mut self) {
        // First thing to do is to determine how many
        // heaps we're operating on.
        for (base_address, mut contents) in self.prepare_heaps() {
            contents.next_id = self.next_id;
            // In case BA fails.
            let backup = contents.clone();
            println!("{} ({} jobs): (t_m, max_load) = {:?}, (h_min, h_max) = {:?}", base_address, contents.get_len(), contents.max_load(), contents.min_max_height());
            let mut best_makespan = usize::MAX;
            const NUM_TRIALS: usize = 5;
            let mut done = 0;
            while done < NUM_TRIALS {
                contents = backup.clone();
                contents.restore_heights();
                // Error parameter initialized via Corollary 17.
                let mut e = LilEGen::new(&mut contents);
                let ready = loop {
                    // 2 possible outcomes exist:
                    // (i) BA has boxed ALL jobs
                    // (ii)BA hasn't done (i)
                    self.magic.current.set(RandTensor::new());
                    self.magic.dims = (0, 0, 0, 0, 0);
                    match contents.t_16(e.val, &mut self.magic) {
                        Ok(mut all_boxed)   => { 
                            self.next_id = all_boxed.next_id;
                            all_boxed.check_overlap(false);
                            break all_boxed 
                        },
                        Err(info)   => {
                            e.update(info.jobs_boxed);
                            contents = backup.clone();
                            contents.restore_heights();
                        }
                    }
                };
                // At this point, jobs buried in _ready must
                // be placed and unpacked.
                let tight = do_placement(
                    ready.contents, 
                    0, 
                    true,
                    false,
                    ready.overlappin,
                    true,
                    best_makespan         
                );
                //let mut refer = PlacedSet::new();
                //tighten_placement(tight, &mut refer);
                if tight.is_empty() {
                    self.magic.update_history(None);
                } else if tight.max_addr < best_makespan {
                    best_makespan = tight.max_addr;
                    println!("Improved! Current best = {} ({} iters)", best_makespan / 4096 + 1, done + 1);
                    self.update_addresses(&tight);
                    self.magic.update_history(Some(best_makespan));
                }
                done += 1;
            }

            println!("Placement done = {}.", done);
        }
        self.give_back().expect("Shit happened.");
    }

    fn update_addresses(&mut self, winner: &PlacedSet) {
        winner.stuff
            .iter()
            .for_each(|j| {
                self.best_addresses.insert(j.id(), j.addr().unwrap());
            })
    }
}

/* 
    TAME RANDOMNESS START
    ----------------------

    Arguably the biggest problem of this algorithm is its random
    nature, as expressed by the consecutive "critical point injection"
    in Theorem 2. We want to go one step further and make this process
    less random. The first step towards this goal is *recording* the
    decisions taken.
*/

// We have identified five dimensions along which randomness
// unfolds: (i) Theorem 16 recursion depth, (ii) Corollary 15
// bucket index, (iii) Theorem 2 recursion depth, (iv) Theorem 2
// bucket index and (v) Theorem 2 entry point index, i.e., the bucket
// index of the previous layer which triggered this iteration.
// All dimensions' numbering starts from zero, and
// it is hard to know in advance the precise sizes; this, along
// with the need to allow for easy indexing, make a Vector of tuples
// a handy choice.
type RandMoment = (usize, usize, usize, usize, usize);

// A "random" (for now) moment, coupled to the jobs
// that are then live. There may be other jobs live
// as well--we limit ourselves to the jobs included
// in the partition that triggered said moment selection.
pub type RandPoint = (Vec<Rc<Job>>, usize);

// A "tensor" (named so because of the many dimensions identified)
// is a collection of random moments along the unfolding of a SINGLE
// BA run. As a whole, it can be considered an "action".
struct RandTensor {
    data:       Vec<(RandMoment, RandPoint)>
}

impl RandTensor {
    fn new() -> Self {
        Self {
            data:   vec![]
        }
    }
}

// This is the main structure passed back & forth between
// the several stages of the algorithm. Running dims are
// updated accordingly at each stage.
pub struct RandTameLogic {
    // All of the "actions" taken in previous
    // iterations of the algorithm.
    history:    Cell<Vec<(RandTensor, Option<usize>)>>,
    // The action currently being built.
    current:    Cell<RandTensor>,
    // Index for the current action.
    pub dims:   RandMoment,
}

impl RandTameLogic {
    fn new() -> Self {
        Self {
            history:    Cell::new(vec![]),
            current:    Cell::new(RandTensor::new()),
            dims:       (0, 0, 0, 0, 0)
        }
    }

    fn update_history(&mut self, score: Option<usize>) {
        let temp = self.current.replace(RandTensor::new());
        self.history.get_mut().push((temp, score));
    }

    pub fn update_current_points(&mut self, to_insert: RandPoint) {
        self.current
            .get_mut()
            .data
            .push((self.dims, to_insert))
    }
}