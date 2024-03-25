pub use std::env;
use std::io::{BufRead, BufWriter};
pub use std::path::Path;
pub use std::io::{BufReader, Read};
pub use std::fs::File;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Placement {
    heap:   usize,
    offset: usize,
    size:   usize,
    align:  usize,
}

impl Placement {
    fn new(heap: usize, offset: usize, size: usize, align: usize) -> Self {
        Self { heap, offset, size, align }
    }

    fn address(&self) -> usize {
        self.heap + self.offset
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Request {
    // Info held matches the TRACED data!
    pub rtype:  ReqType,
    tid:        usize
}

impl Request {
    fn get_alignment(&self) -> usize {
        match self.rtype {
            ReqType::Free(_)    => { panic!("Unreachable reached!"); },
            ReqType::AlignedAlloc(a, _) => { a },
            ReqType::MemAlign(a, _) => { a },
            ReqType::PosixMemAlign(_, a, _) => { a },
            _   => { 0 }
        }
    }

    fn get_req_size(&self) -> usize {
        match self.rtype {
            ReqType::Free(_)    => { panic!("Unreachable reached!"); },
            ReqType::Malloc(s)  => { s },
            ReqType::Calloc(n, s)   => { n * s},
            ReqType::ReAlloc(_, s)  => { s },
            ReqType::AlignedAlloc(_, s) => { s },
            ReqType::MemAlign(_, s) => { s },
            ReqType::PosixMemAlign(_, _, s) => { s },
            ReqType::Done    => { panic!("Unreachable reached!"); },
        }
    }

    pub fn new_trc_event(world: &mut SimWorld) -> Option<()> {
        //! Spawns a new event from a trace file.
        let trace = &mut world.input;
        let mut sentinel: [u8; 1] = [0];
        let mut baby_req = Self { rtype: ReqType::Done, tid: 0 };
        let mut baby_place = None;
        if let Ok(_) = trace.read_exact(&mut sentinel) {
            if  sentinel[0] != 0x05 &&
                sentinel[0] != 0x12 &&
                sentinel[0] != 0x26 &&
                sentinel[0] != 0x36 &&
                sentinel[0] != 0x46 &&
                sentinel[0] != 0x56 &&
                sentinel[0] != 0x67 {
                    panic!("Corrupt trace file."); 
            }
            let req_type = CODES[((sentinel[0] & 0b11110000u8) >> 4) as usize];
            baby_req.rtype = req_type;
            let words_left = sentinel[0] & 0b00001111u8;
            // Buffer to get filled repeatedly.
            let mut word_buf: [u8; 8] = [0; 8];
            let mut i: u8 = 0;
            let mut block_start: usize = 0;
            let mut map_base: usize = 0;
            let mut tid: usize = 0;
            let mut block_size: usize = 0;
            while i < words_left {
                trace.read_exact(&mut word_buf).expect("Unexpected trace EOF");
                let data = usize::from_be_bytes(word_buf);
                if i == words_left - 1 && !req_type.is_free() {
                    map_base = data;
                } else if i == words_left - 2 && !req_type.is_free() {
                    tid = data;
                } else if !req_type.is_free() && i == words_left - 3 {
                    block_size = data;
                } else if !req_type.is_free() && i == words_left - 4 {
                    block_start = data;
                }
                match baby_req.rtype {
                    ReqType::Malloc(ref mut size)  => {
                        if i == 0 { *size = data; }
                    },
                    ReqType::Calloc(ref mut nobj, ref mut size)  => {
                        if i == 0 { *nobj = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::ReAlloc(ref mut p, ref mut size)  => {
                        if i == 0 { *p = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::AlignedAlloc(ref mut a, ref mut size) => {
                        if i == 0 { *a = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::MemAlign(ref mut a, ref mut size) => {
                        if i == 0 { *a = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::PosixMemAlign(ref mut p, ref mut a, ref mut size) => {
                        if i == 0 { *p = data; }
                        else if i == 1 { *a = data; }
                        else if i == 2 { *size = data; }
                    },
                    ReqType::Free(ref mut p)   => {
                        if i == 0 { *p = data; }
                        else { tid = data; }
                    },
                    ReqType::Done   => {
                        panic!("Unreachable block reached.");
                    },
                };
                i += 1;
            };
            if !baby_req.rtype.is_free() {
                baby_place = Some(Placement::new(map_base, block_start - map_base, block_size, baby_req.get_alignment()));
            }
            baby_req.tid = tid;
        };

        if !baby_req.is_done() {
            Object::new(world, baby_req, baby_place, world.objects_num);
            Some(())
        } else {
            None
        }
        //(baby_req, baby_place)
    }

    pub fn new_minimalloc_event(world: &mut SimWorld) -> Option<()> {
        let maybe_dirty = &mut world.input;
        if let Some(l) = maybe_dirty.lines()
            .skip(if world.objects_num == 0 { 1 } else { 0 })
            .next() {
                match l {
                    Ok(txt) => {
                        world.objects_num += 1;
                        let data: Vec<usize> = txt.split(',')
                            .map(|chrs| usize::from_str_radix(chrs, 10).unwrap())
                            .collect();
                        assert_eq!(data.len(), 5, "Expected CSV has 5 columns.");
                        let obj_id = data[0];
                        match world.all_objects.insert(obj_id, Object::new_from_csv(&data)) {
                            None    => {},
                            Some(j) => {
                                panic!("Tried to overwrite {:?}", j);
                            }
                        }
                        return Some(());
                    },
                    Err(_)  => { panic!("Error parsing mimalloc output."); }
                }
        };

        None
    }

    fn is_done(&self) -> bool {
        if let ReqType::Done = self.rtype { true }
        else { false }
    }

    pub fn new_from_plc(world: &mut SimWorld) -> Option<()> {
        const PLC_FIELDS_NUM: usize = 8;
        let plc_source = &mut world.input;
        let mut buffer: [u8; 8 * PLC_FIELDS_NUM] = [0; 8 * PLC_FIELDS_NUM];
        let mut baby_job = Object {
            birth:  0,
            death:  0,
            height: 0,
            home:   Placement { heap: 0, offset: 0, size: 0, align: 0 },
            id:     0
        };
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
                        1   => { baby_job.id = data; },
                        2   => { baby_job.birth = data; },
                        3   => { baby_job.death = data; },
                        4   => { baby_job.home.size = data; },
                        5   => { baby_job.home.heap = data; },
                        6   => { baby_job.home.offset = data; },
                        7   => { baby_job.home.align = data; },
                        8   => { baby_job.height = data; },
                        _   => { panic!("Reached unreachable state!"); }
                    }
                }
                match world.all_objects.insert(baby_job.id, baby_job) {
                    Some(j) => {
                        panic!("Tried to overwrite {j:?}!");
                    },
                    None    => {}
                };

                Some(())
            },
            Err(_)  => { None }
        }
    }
}

#[derive(Debug, Eq, Serialize, Deserialize)]
pub struct Object {
    pub birth:      usize,
    pub death:      usize,
    // CAUTION: This is the REQUESTED, not the ALLOCATED size!
    pub height:     usize,
    pub home:       Placement,
    id:             usize
}

// In the context of fragmentation calculation, we are
// going to need a min, address-ordered, BinaryHeap.
impl Ord for Object {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.home.offset.cmp(&self.home.offset)
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// Time always progresses after every allocation. Birth
// times are thus unique.
impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.birth == other.birth
    }
}

impl Object {
    fn id(&self) -> usize {
        self.id
    }

    fn size(&self) -> usize {
        self.home.size
    }

    fn new_from_csv(data: &Vec<usize>) -> Self {
        Self {
            birth:  data[1],
            death:  data[2],
            height: data[3],
            home:   Placement { heap: 62, offset: data[4], size: data[3], align: 0 },
            id:     data[0]
        }
    }

    pub fn new(world: &mut SimWorld, inciting_req: Request, data: Option<Placement>, id: usize) {
        // Object is now created and the only thing left is to insert it
        // to the world's books. However, if the request served was a
        // ReAlloc, we also need to retire the job at the old address!
        if let ReqType::Done = inciting_req.rtype {
            return;
        }

        if let ReqType::ReAlloc(old_add, _) = inciting_req.rtype {
            if old_add != 0 {
                let mut to_retire = world.jobs
                    .remove(&old_add)
                    .unwrap();
                to_retire.death = world.time;
                assert!(world.all_objects.insert(to_retire.id(), to_retire).is_none());
            }
        }
        
        if let ReqType::Free(old_add) = inciting_req.rtype {
            if old_add != 0 {
                let mut to_retire = world.jobs.remove(&old_add).unwrap();
                to_retire.death = world.time;
                assert!(world.all_objects.insert(to_retire.id(), to_retire).is_none());
            }
        }
        else { 
            let data = data.unwrap();
            let res = Object { 
                birth: world.time, 
                // To be filled upon retirement.
                death: 0, 
                height: inciting_req.get_req_size(), 
                // Mapping to be filled later in this function.
                home: data, 
                id
            };
            world.update_time(data.size);
            if let Some(_culprit) = world.jobs.insert(data.address(), res) {
                panic!("Tried to insert job w/ existing ID!");
            };
        }
    }

    // Some helper functions for address arithmetic.
    fn offset(&self) -> usize { self.home.offset }
    fn page_local_offset(&self) -> usize { self.home.offset % utils::PAGE_SIZE }
    fn page_start_bottom(&self) -> usize { self.home.offset / utils::PAGE_SIZE * utils::PAGE_SIZE }
    fn page_end_top(&self) -> usize { (1 + self.top_add() / utils::PAGE_SIZE) * utils::PAGE_SIZE - 1 }
    fn next_avail_add(&self) -> usize { self.home.offset + self.size() }
    pub fn top_add(&self) -> usize { self.next_avail_add() - 1 }
    fn gap_same_page(&self, other: &Self) -> bool {
        if self.top_add() < other.home.offset {
                self.next_avail_add() / utils::PAGE_SIZE == other.home.offset / utils::PAGE_SIZE
                // Limit case!
            &&  self.next_avail_add() < self.page_end_top()
        } else {
            other.gap_same_page(self)
        }
    }

    // Gaps between jobs make sense only within pages.
    fn gap(&self, other: &Self) -> usize {
        if self.home.offset < other.home.offset {
            if self.gap_same_page(other) {
                other.home.offset - self.next_avail_add()
            } else {
                // Jobs are in different pages, with nothing else in between.
                0
            }
        } else { other.gap(self) }
    }

    fn runner_gap(&self, runner: usize) -> Option<usize> {
        if runner > self.home.offset { panic!("Runner can't exceed offset."); }
        let cand = self.home.offset - runner;
        if cand < utils::PAGE_SIZE {
            Some(cand)
        } else {
            None
        }
    }
}

use indexmap::IndexMap;
use ahash::AHasher;

type ObSet = IndexMap<usize, Object, std::hash::BuildHasherDefault<AHasher>>;

pub struct SimWorld {
    // Live jobs.
    jobs:               ObSet,
    time:               usize,
    // Trace file.
    input:              BufReader<File>,
    pub objects_num:    usize,
    // All jobs.
    all_objects:        ObSet
}

impl SimWorld {
    #[inline(always)]
    fn update_time(&mut self, increment: usize) {
        // Time is incremented only when new objects are made.
        self.objects_num += 1;
        self.time = self.time.checked_add(increment).expect("Time overflowed!");
    }

    pub fn new(input: BufReader<File>) -> Self {
        Self { 
            jobs: IndexMap::default(),
            time: 0,
            input,
            objects_num: 0,
            all_objects: IndexMap::default()
        }
    }

    pub fn retire_remnants(&mut self) {
        for (_k, mut v) in self.jobs.drain(..) {
            v.death = self.time;
            assert!(self.all_objects.insert(v.id(), v).is_none());
        }
    }

    pub fn give_back(&self, input_csv_name: &Path) {
        let no_suffix_name = String::from(
            input_csv_name.file_stem()
                .unwrap()
                .to_str()
                .unwrap()
        );
        let fd = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(format!("{no_suffix_name}.plc"))
            .unwrap();
        let mut writer = BufWriter::new(fd);
        for j in self.all_objects.values() {
            update_trace(&mut writer, j);
        }
    }
}

fn update_trace(w: &mut BufWriter<File>, j: &Object) {
    use std::io::Write;

    let mut buff: Vec<u8> = vec![];

    let args = get_args(j);
    for arg in args {
        let arg_bytes = arg.to_be_bytes();
        for b in &arg_bytes {
            buff.push(*b);
        }
    }

    w.write(buff.as_slice()).unwrap();
}

fn get_args(j: &Object) -> Vec<usize> {
    vec![
        j.id,
        j.birth,
        j.death,
        j.home.size,
        j.home.heap,
        j.home.offset,
        j.home.align,
        j.height
    ]
}

use std::collections::{BTreeSet, BTreeMap, BinaryHeap};
use serde::{Serialize, Deserialize};

// As we traverse our workload, we store
// information w.r.t. the load and the
// waste of the placement. These are what
// we need to calculate fragmentation, and
// other useful things.
#[derive(Serialize, Deserialize)]
struct WorkloadStats {
    running_load:   BTreeMap<usize, usize>,
    running_waste:  BTreeMap<usize, usize>,
    agg_data:       BTreeMap<usize, (usize, usize)>,
    time:           BTreeSet<usize>,
    max_load_t:     usize,
    makespan:       usize,
    max_size:       usize,
    objects_num:    usize,
}

impl WorkloadStats {
    fn new() -> Self {
        Self {  running_load:   BTreeMap::new(),
                running_waste:  BTreeMap::new(),
                agg_data:       BTreeMap::new(),
                time:           BTreeSet::new(),
                max_load_t:     0,
                makespan:       0,
                max_size:       0, 
                objects_num:    0
        }
    }

    fn inject_points(&mut self, steps: &BTreeSet<usize>) {
        let mut sum = 0;
        for t in steps {
            if let Some(v) = self.running_load.get(t) {
                sum = *v;
            }
            else {
                self.running_load.insert(*t, sum); 
            }
        }
        sum = 0;
        for t in steps {
            if let Some(v) = self.running_waste.get(t) {
                sum = *v;
            }
            else {
                self.running_waste.insert(*t, sum); 
            }
        }
    }

    fn max_load(&self) -> (usize, usize) {
        (self.max_load_t, *self.running_load.get(&self.max_load_t).unwrap())
    }

    fn load_at_t(&self, t: usize) -> (usize, usize) {
        (
            *self.running_load.get(&t).unwrap(),
            self.agg_data.get(&t).unwrap().0
        )
    }

    fn frag_at_t(&self, t: usize) -> (f64, f64) {
        let (rl, al) = self.load_at_t(t);
        let mut res: (f64, f64) = (0.0, 0.0);
        if !(rl == 0) {
            res.0 = *self.running_waste.get(&t).unwrap() as f64 / (rl as f64);
        }
        if !(al == 0) {
            res.1 = self.agg_data.get(&t).unwrap().1 as f64 / (al as f64);
        }

        res
    }
}

#[derive(Serialize, Deserialize)]
pub struct Mapping {
    pub objects:        Vec<Object>,
    pub address_space:  (usize, usize),
    pub time_endpoints: (usize, usize)
}

impl Mapping {
    fn new(o: Object) -> Self {
        Self { 
            address_space: (o.page_start_bottom(), o.page_end_top()), 
            time_endpoints: (o.birth, o.death),
            objects: vec![o],
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct TDBP {
    pub mappings:   IndexMap<usize, Mapping, std::hash::BuildHasherDefault<AHasher>>,
    stats:          WorkloadStats
}

impl TDBP {
    pub fn new(world: SimWorld) -> Self {
        let mut res = TDBP {
            mappings: IndexMap::default(),
            stats: WorkloadStats::new(),
        };

        for test in world.all_objects.into_values() {
            if test.size() > res.stats.max_size {
                res.stats.max_size = test.size();
            }
            res.stats.objects_num += 1;
            if !res.stats.time.contains(&test.birth) {
                res.stats.time.insert(test.birth);
                res.stats.time.insert(test.birth + 1);
            }
            if !res.stats.time.contains(&test.death) {
                res.stats.time.insert(test.death);
            }
            let test_map = test.home.heap;
            match res.mappings.get_mut(&test_map) {
                Some(jobs)  => {
                    if test.page_start_bottom() < jobs.address_space.0 {
                        jobs.address_space.0 = test.page_start_bottom();
                    }
                    if test.page_end_top() > jobs.address_space.1 {
                        jobs.address_space.1 = test.page_end_top();
                    }
                    if test.birth < jobs.time_endpoints.0 {
                        jobs.time_endpoints.0 = test.birth;
                    }
                    if test.death > jobs.time_endpoints.1 {
                        jobs.time_endpoints.1 = test.death;
                    }
                    jobs.objects.push(test);
                },
                None    => { res.mappings.insert(test_map, Mapping::new(test)); }
            }
        };
        println!("Commencing processing of {} objects...", res.stats.objects_num);

        res
    }

    pub fn init_mappings(&mut self) {
        for mapping in self.mappings.values_mut() {
            // Sort objects w.r.t. birth date. Unstable sorting is faster
            // since it doesn't allocate. No equal elements are expected.
            mapping.objects.sort_unstable_by(|this, other| {
                if this.birth < other.birth {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            });
        }
    }

    pub fn measure_frag(&mut self) {
        // Process is expensive. If it has already run,
        // no need to do it again. We make the assumption
        // that if the load vector is non-empty, both itself
        // AND the waste vector contain valid, complete data.
        if self.stats.running_load.is_empty() {
            use std::sync::Mutex;
            use rayon::prelude::*;

            let all_map_stats: Mutex<Vec<WorkloadStats>> = Mutex::new(vec![]);

            self.mappings.par_iter().for_each(|(_m, jobs)| {
                let map_stats = Area::traverse(&jobs.objects[..]);

                let mut guard = all_map_stats.lock().unwrap();
                guard.push(map_stats);
            });
            println!("Mapping processing done. Consolidating into workload-wide data...");
            let mut all_map_stats = all_map_stats.into_inner().unwrap();

            // We now make sure that all vectors in all mappings address all time points.
            for elt in &mut all_map_stats {
                // Also an opportunity to calculate total makespan.
                self.stats.makespan += elt.makespan;
                elt.inject_points(&self.stats.time);
                for (t, ld) in &elt.running_load {
                    match self.stats.running_load.get_mut(t) {
                        None    => { self.stats.running_load.insert(*t, *ld); },
                        Some(old)   => { *old += *ld; }
                    };
                }
                for (t, wst) in &elt.running_waste {
                    match self.stats.running_waste.get_mut(t) {
                        None    => { self.stats.running_waste.insert(*t, *wst); },
                        Some(old)   => { *old += *wst; }
                    };
                }
            }
        }
    }

    pub fn get_acc(&mut self) {
        let mut prev_point = (0, (0, 0));
        let mut max_load = 0;
        let mut prev_els = (0, 0);
        let mut idx = 0;

        for (t, ld) in &self.stats.running_load {
            if idx == 0 {
                self.stats.agg_data.insert(0, (0, 0));
            } else {
                if *ld > max_load {
                    max_load = *ld;
                    self.stats.max_load_t = *t;
                }
                let wst = self.stats.running_waste.get(t).unwrap();
                let ld_incr: usize;
                let wst_incr: usize;
                if idx < self.stats.running_load.len() - 1 {
                    if *ld > prev_els.0 {
                        ld_incr = (t - prev_point.0) * *ld;
                    } else {
                        ld_incr = (t - prev_point.0) * prev_els.0;
                    }
                    wst_incr = (t - prev_point.0) * *wst;
                } else {
                    ld_incr = (t - prev_point.0) * prev_els.0;
                    wst_incr = (t - prev_point.0) * prev_els.1;
                }
                self.stats.agg_data.insert(*t, (prev_point.1.0 + ld_incr, prev_point.1.1 + wst_incr));
                prev_point = (*t, (prev_point.1.0 + ld_incr, prev_point.1.1 + wst_incr));
                prev_els = (*ld, *wst);
            }
            idx += 1;
        }
    }

    pub fn report(&self) {
        println!("Makespan (in pages): {}", self.stats.makespan / utils::PAGE_SIZE);
        println!("Max object size: {} bytes", self.stats.max_size);
        println!("Max load: {} bytes at t = {}", self.stats.max_load().1, self.stats.max_load().0);
        println!("Overall agg frag: {:.3}", self.stats.frag_at_t(*self.stats.time.last().unwrap()).1);
        println!("Agg frag at max load: {:.3}", self.stats.frag_at_t(self.stats.max_load_t).1);
        println!("Running frag at max load: {:.3}", self.stats.frag_at_t(self.stats.max_load_t).0);
        println!("Makespan vs. LOAD: {:.3}%", self.stats.makespan as f64 / self.stats.max_load().1 as f64 * 100.0);
    }
}

// Used in the context of fragmentation. See below.
#[derive(Debug, Clone, Copy)]
enum JobRole {
    Init,
    Add,
    Retire
}

// Used in the context of fragmentation. See below.
#[derive(Debug, Clone, Copy)]
enum Position {
    Top,
    Mid,
    Bottom
}

// Here's how we compute fragmentation: we traverse all of
// a mapping's objects in birth-increasing order. By "traverse"
// we refer both to the CREATION and RETIREMENT of objects. Those
// moments create events which affect the running state of the
// placement in a specific way. We keep that state, in order to
// save extra work--which proves very very valuable.
// Each event either adds or retires a job, either from the top,
// the middle or the bottom of our running state.
#[derive(Debug, Clone, Copy)]
struct Event<'o> {
    job:    &'o Object,
    role:   JobRole,
    pos:    Option<Position>,
    time:   usize
}

impl<'o> Event<'o> {
    fn new(job: &'o Object, state: &Area<'o>, is_birth: bool) -> Self {
        let mut res = Self {
            job,
            role: JobRole::Init,
            pos: None,
            time: 0
        };

        if !(state.live.is_empty() && is_birth) {
            // Else the result is ready.
            if state.live.is_empty() { panic!("Job wanted to be retired from Empty Area."); }
            if is_birth { res.role = JobRole::Add; res.time = job.birth; } 
            else { res.role = JobRole::Retire; res.time = job.death }
            let h = state.live.last_key_value().unwrap().1;
            if job.home.offset >= h.home.offset {
                res.pos = Some(Position::Top);
            } else if job.home.offset <= state.live.first_key_value().unwrap().1.home.offset {
                res.pos = Some(Position::Bottom);
            } else {
                res.pos = Some(Position::Mid);
            }
        }

        res
    }
}

// This is the running state of fragmentation calculation.
struct Area<'o> {
    live:               BTreeMap<usize, &'o Object>,
    add_space:          (usize, usize),
    prev_point:         usize,
    run_waste:          usize,
    run_load:           usize,
}

impl<'o> Area<'o> {
    fn new() -> Self {
        Self { 
            add_space: (0, 0),
            live: BTreeMap::new(),
            prev_point: 0,
            // Amoung of live memory.
            run_load: 0,
            // Amount of live gaps.
            run_waste: 0,
        }
    }

    fn makespan(&self) -> usize {
        self.add_space.1 - self.add_space.0 + 1
    }

    fn update(&mut self, stats: &mut WorkloadStats, evt: Event<'o>) {
        match evt.role {
            JobRole::Init   => {
                // Jobs are considered dead at their limits.
                stats.running_load.insert(evt.time, 0);
                stats.running_load.insert(evt.time + 1, evt.job.size());
                self.run_load = evt.job.size();
                // CAUTION: A mapping consists of many pages. Frag calculation makes
                // sense only WITHIN page boundaries. Thus in the case of a single job,
                // it contributes as waste the distance from its closest lower page boundary.
                self.run_waste = evt.job.page_local_offset();
                // Gaps considered vacuum @ their limits as well.
                stats.running_waste.insert(evt.time, 0);
                stats.running_waste.insert(evt.time + 1, self.run_waste);
                self.add_space.0 = evt.job.page_start_bottom();
                self.add_space.1 = evt.job.page_end_top();
                if let Some(existing) = self.live.insert(evt.job.offset(), evt.job) {
                    panic!("Already processed object {:?} temporally AND spatially overlaps with currently processed object {:?}!", existing, evt.job);
                }
            },
            JobRole::Add    => {
                stats.running_load.insert(evt.time, self.run_load);
                self.run_load += evt.job.size();
                stats.running_load.insert(evt.time + 1, self.run_load);
                match evt.pos {
                    Some(Position::Bottom)    => {
                        let curr_bottom = self.live.first_key_value().unwrap().1;
                        let mut waste_incr = evt.job.page_local_offset();
                        if evt.job.gap_same_page(*curr_bottom) {
                            self.run_waste = self.run_waste.checked_sub(curr_bottom.page_local_offset()).unwrap();
                            waste_incr += evt.job.gap(*curr_bottom);
                        }
                        stats.running_waste.insert(evt.time, self.run_waste);
                        self.run_waste += waste_incr;
                        stats.running_waste.insert(evt.time + 1, self.run_waste);
                        self.add_space.0 = evt.job.page_start_bottom();
                        if let Some(existing) = self.live.insert(evt.job.offset(), evt.job) {
                            panic!("Already processed object {:?} temporally AND spatially overlaps with currently processed object {:?}!", existing, evt.job);
                        }
                    },
                    Some(Position::Mid) => {
                        if let Some(existing) = self.live.insert(evt.job.offset(), evt.job) {
                            panic!("Already processed object {:?} temporally AND spatially overlaps with currently processed object {:?}!", existing, evt.job);
                        }
                        stats.running_waste.insert(evt.time, self.run_waste);
                        let waste_incr = self.get_waste_core();
                        self.run_waste = waste_incr;
                        stats.running_waste.insert(evt.time + 1, self.run_waste);
                    },
                    Some(Position::Top) => {
                        stats.running_waste.insert(evt.time, self.run_waste);
                        let highest_job = self.live.last_key_value().unwrap().1;
                        if evt.job.gap_same_page(*highest_job) {
                            let waste_incr: usize = evt.job.gap(highest_job);
                            self.run_waste += waste_incr;
                        } else {
                            self.run_waste += evt.job.page_local_offset();
                        }
                        stats.running_waste.insert(evt.time + 1, self.run_waste);
                        self.add_space.1 = evt.job.page_end_top();
                        if let Some(existing) = self.live.insert(evt.job.offset(), evt.job) {
                            panic!("Already processed object {:?} temporally AND spatially overlaps with currently processed object {:?}!", existing, evt.job);
                        }
                    },
                    None    => { panic!("Cannot add None pos-job"); }
                };
            },
            JobRole::Retire => {
                self.run_load -= evt.job.size();
                match stats.running_load.get_mut(&evt.time) {
                    None    => { stats.running_load.insert(evt.time, self.run_load); },
                    Some(existing_v) => { *existing_v = (*existing_v).min(self.run_load); }
                }
                match evt.pos {
                    Some(Position::Bottom)  => {
                        let evict = self.live.pop_first().unwrap().1;
                        assert_eq!(evt.job, evict, "{:?} came dressed as retired bottom, but real bottom is {:?}", evt.job, evict);
                        if self.live.len() == 0 {
                            self.run_waste = 0;
                        } else {
                            let new_guy = self.live.first_key_value().unwrap().1;
                            // First, subtract the bottom's contributions--both ways.
                            self.run_waste = self.run_waste.checked_sub(evict.page_local_offset()).unwrap();
                            if evict.gap_same_page(*new_guy) {
                                self.run_waste = self.run_waste.checked_sub(evict.gap(new_guy)).unwrap();
                                self.run_waste += new_guy.page_local_offset();
                            }
                            self.add_space.0 = new_guy.page_start_bottom();
                        }
                        match stats.running_waste.get_mut(&evt.time) {
                            None    => { stats.running_waste.insert(evt.time, self.run_waste); },
                            Some(existing_v) => { *existing_v = (*existing_v).min(self.run_waste); }
                        }
                    },
                    Some(Position::Mid) => {
                        if let None = self.live.remove(&evt.job.home.offset) {
                            panic!("Retirement faild!");
                        }
                        let waste_incr = self.get_waste_core();
                        self.run_waste = waste_incr;
                        match stats.running_waste.get_mut(&evt.time) {
                            None    => { stats.running_waste.insert(evt.time, self.run_waste); },
                            Some(existing_v) => { *existing_v = (*existing_v).min(self.run_waste); }
                        }
                    },
                    Some(Position::Top) => {
                        let evict = self.live.pop_last().unwrap().1;
                        assert_eq!(evt.job, evict, "{:?} came dressed as retired top, but real bottom is {:?}", evt.job, evict);
                        if self.live.len() == 0 {
                            self.run_waste = 0;
                        } else {
                            let new_guy = self.live.last_key_value().unwrap().1;
                            if new_guy.gap_same_page(evict) {
                                self.run_waste = self.run_waste.checked_sub(evict.gap(new_guy)).unwrap();
                            } else {
                                self.run_waste = self.run_waste.checked_sub(evict.page_local_offset()).unwrap();
                            }
                            self.add_space.1 = new_guy.page_end_top();
                        }
                        match stats.running_waste.get_mut(&evt.time) {
                            None    => { stats.running_waste.insert(evt.time, self.run_waste); },
                            Some(existing_v) => { *existing_v = (*existing_v).min(self.run_waste); }
                        }
                    },
                    None    => { panic!("Cannot retire None pos-job"); }
                }
            },
        }
        self.prev_point = evt.time;
        if self.makespan() > stats.makespan { stats.makespan = self.makespan(); }
    }

    fn get_waste_core(&mut self) -> usize {
        let mut res: usize = 0;
        let mut runner = self.add_space.0;
        
        for (_, j) in &self.live {
            let test = j.runner_gap(runner);
            match test {
                Some(same_page_diff)    => { res += same_page_diff; },
                None    => { res += j.page_local_offset(); }
            };
            runner = j.next_avail_add();
        }

        res
    }

    // Traversal is arguably the most crucial operation
    // when processing a simulated placement. Doing it
    // efficiently is the difference between a PhD and
    // no PhD. So let's do it well.
    fn traverse(jobs: &[Object]) -> WorkloadStats {
        let max_capacity = jobs.len();
        let mut res = WorkloadStats::new();
        let mut area = Area::new();
        // Efficient live job book-keeping is key. We want to add
        // and remove live jobs fast. We use a HashMap indexed by
        // the reference points that we encounter along the way.
        let mut future_refs: BinaryHeap<RefPoint> = BinaryHeap::with_capacity(max_capacity / 2);

        for j in jobs {
            // We first must account for any events (i.e. deaths) that happen earlier.
            while let Some(p) = future_refs.peek() {
                if j.birth < p.1 { break; }
                let key = future_refs.pop().unwrap();
                let evt = Event::new(area.live.get(&key.2).unwrap(), &area, false);
                if !res.time.contains(&evt.job.death) {
                    res.time.insert(evt.job.death);
                }
                area.update(&mut res, evt);
            }
            // THEN we account for the job itself.
            let evt = Event::new(j, &area, true);
            if !res.time.contains(&evt.job.birth) {
                res.time.insert(evt.job.birth);
                res.time.insert(evt.job.birth + 1);
            }
            area.update(&mut res, evt);
            future_refs.push(RefPoint::new(j));
        }
        // Finally, we account for remaining deaths.
        while let Some(key) = future_refs.pop() {
            let evt = Event::new(area.live.get(&key.2).unwrap(), &area, false);
            if !res.time.contains(&evt.job.death) {
                res.time.insert(evt.job.death);
            }
            area.update(&mut res, evt);
        }

        res
    }
}

// RefPoints are nothing fancier than the
// birth and death data of objects. On top of this
// type, however, we intend to build a crucial helper
// structure for our traversal algorithm.
#[derive(Eq, PartialEq, Hash, Clone, Copy)]
struct RefPoint(usize, usize, usize);

impl RefPoint {
    fn new(j: &Object) -> Self {
        Self(j.birth, j.death, j.home.offset)
    }
}

// The "crucial helper structure" mentioned above is
// a BinaryHeap. We want it to be a min-heap on the deaths
// of jobs, with tie breaks solved by job births (where there
// are no ties).
impl Ord for RefPoint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.1.cmp(&self.1).then_with(|| { self.2.cmp(&other.2) })
    }
}

impl PartialOrd for RefPoint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

mod utils {
    pub const PAGE_SIZE: usize = 4096;
}

pub mod plot {
    use plotters::prelude::*;
    use super::{TDBP, Object};
    use std::path::Path;
    use std::fs::create_dir;

    pub fn plot_mappings(m: &TDBP, f: &Path) {
        let file_name = f.file_stem().unwrap();
        let f = f.parent().unwrap();
        for (idx, mapn) in &m.mappings {
            let map_path = f.join(format!("{file_name:?}_m_{}", *idx));
            create_dir(&map_path).unwrap();
            let img = map_path.join("placement.png");
            let backend = BitMapBackend::new(&img, (1920, 1080)).into_drawing_area();
            backend.fill(&WHITE).unwrap();
            let backend = backend.margin(10u32, 10u32, 10u32, 10u32);

            let mut chart = ChartBuilder::on(&backend)
                                .x_label_area_size(20u32)
                                .y_label_area_size(60u32)
                                .build_cartesian_2d(mapn.time_endpoints.0..mapn.time_endpoints.1 + 1,
                                            mapn.address_space.0..mapn.address_space.1 + 1).unwrap();

            chart
                .configure_mesh()
                .x_labels(10)
                .y_labels(10)
                .draw().unwrap();

            chart.draw_series(create_series(&mapn.objects)).unwrap();
        }
    }

    fn create_series(m: &Vec<Object>) -> Vec<Rectangle<(usize, usize)>> {
        let mut jobs_result: Vec<Rectangle<(usize, usize)>> = Vec::new();

        for job in m {
            let left_x = job.birth;
            let right_x = job.death;
            let lower_y = job.home.offset;
            let upper_y = job.next_avail_add();
            jobs_result.push(Rectangle::new([
                (left_x, upper_y),
                (right_x, lower_y)],
                ShapeStyle {
                    color: BLACK.into(),
                    filled: false,
                    stroke_width: 1,
                }
            ));
        }

        jobs_result
    }
}