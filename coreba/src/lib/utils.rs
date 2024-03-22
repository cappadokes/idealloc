pub use anyhow::{Error, Result};
use indexmap::IndexMap;
pub use std::collections::{BTreeMap, BinaryHeap};
use crate::elements::{Job, JobSet, LiveSet, RandPoint};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct HeapStats {
    pub running_load:   Option<BTreeMap<usize, usize>>,
    pub max_load:   Option<(usize, usize)>,
    pub max_height: Option<usize>,
    pub min_height: Option<usize>,
    // For used w/ bounding intervals of T2. Contains
    // smallest birth, largest death time.
    pub horizon:    Option<(usize, usize)>
}

impl HeapStats {
    #[inline]
    pub fn new() -> Self {
        Self {  running_load:   None,
                max_load:       None,
                max_height:     None,
                min_height:     None,
                horizon:        None
        }
    }

    #[inline]
    fn push_load(&mut self, k: usize, val: usize) {
        match self.running_load {
            None    => { panic!("Bad traversal."); },
            Some(ref mut v) => {
                v.insert(k, val);
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum JobRole {
    Init,
    Add,
    Retire
}

pub struct Event {
    job:    Rc<Job>,
    role:   JobRole,
    time:   usize
}

impl Event {
    #[inline]
    fn new(job: Rc<Job>, state: &Area, is_birth: bool) -> Self {
        let mut res = Self {
            job: job.clone(),
            role: JobRole::Init,
            time: if is_birth { job.birth.get() } else { job.death.get() }
        };

        if !(state.live.is_empty() && is_birth) {
            // Else the result is ready.
            if state.live.is_empty() { panic!("Job wanted to be retired from Empty Area."); }
            if is_birth { res.role = JobRole::Add; } 
            else { res.role = JobRole::Retire; }
        }

        res
    }
}

pub struct Area {
    live:               BTreeMap<usize, Rc<Job>>,
    prev_point:         usize,
    run_load:           usize,
}

impl Area {
    #[inline]
    pub fn new() -> Self {
        Self { 
            live: BTreeMap::new(),
            prev_point: 0,
            // Amoung of live memory.
            run_load: 0,
        }
    }

    pub fn update(&mut self, stats: &mut HeapStats, evt: Event) {
        match evt.role {
            JobRole::Init   => {
                // Jobs are considered dead at their limits.
                stats.push_load(evt.time, 0);
                stats.push_load(evt.time + 1, evt.job.size());
                self.run_load = evt.job.size();
                if self.run_load > stats.max_load.unwrap().1 {
                    stats.max_load = Some((evt.time + 1, self.run_load));
                }
                self.live.insert(evt.job.id.get(), evt.job.clone());
            },
            JobRole::Add    => {
                stats.push_load(evt.time, self.run_load);
                self.run_load += evt.job.size();
                stats.push_load(evt.time + 1, self.run_load);
                if self.run_load > stats.max_load.unwrap().1 {
                    stats.max_load = Some((evt.time + 1, self.run_load));
                }
                self.live.insert(evt.job.id.get(), evt.job.clone());
            },
            JobRole::Retire => {
                self.run_load -= evt.job.size();
                match stats.running_load {
                    None    => { panic!("Bad traversal.") },
                    Some(ref mut v) => {
                        match v.get_mut(&evt.time) {
                            None    => { stats.push_load(evt.time, self.run_load); },
                            Some(existing_v) => { *existing_v = (*existing_v).min(self.run_load); }
                        }
                    }
                };
                self.live.remove(&evt.job.id.get()).unwrap();
            },
        }
        self.prev_point = evt.time;
    }

    pub fn igc_update(&mut self, state: &mut IGCCtrl, evt: Event) {
        match evt.role {
            JobRole::Init   => {
                let c: usize;
                if !state.a.is_empty() {
                    c = state.a.pop().expect("Bad IGC.").0;
                } else {
                    state.rows.push(IndexMap::default());
                    state.d += 1;
                    c = state.d - 1;
                };
                state.rows[c].insert(evt.job.id(), evt.job.clone());
                self.live.insert(evt.job.id.get(), evt.job.clone());
            },
            JobRole::Add    => {
                let c: usize;
                if !state.a.is_empty() {
                    c = state.a.pop().expect("Bad IGC.").0;
                } else {
                    state.rows.push(IndexMap::default());
                    state.d += 1;
                    c = state.d - 1;
                };
                state.rows[c].insert(evt.job.id(), evt.job.clone());
                self.live.insert(evt.job.id.get(), evt.job.clone());
            },
            JobRole::Retire => {
                let mut rows_iter = state.rows.iter();
                let mut row_num: usize = 0;
                let c: usize = loop {
                    if let Some(m) = rows_iter.next() {
                        if m.contains_key(&evt.job.id()) { break row_num; }
                    };
                    row_num += 1;
                };
                state.a.push(RowNum::new(c));
                self.live.remove(&evt.job.id.get()).unwrap();
            }
        }
        self.prev_point = evt.time;
    }

    pub fn rand_update(&mut self, points: &mut (Vec<RandPoint>, (usize, usize)), evt: Event) {
        match evt.role {
            JobRole::Init   => {
                self.live.insert(evt.job.id.get(), evt.job.clone());
                if evt.time + 1 <= points.1.0 || evt.time + 1 >= points.1.1 { return; }
                points.0.push(self.derive_point(evt.time + 1));
            },
            JobRole::Add    => {
                self.live.insert(evt.job.id.get(), evt.job.clone());
                if evt.time + 1 <= points.1.0 || evt.time + 1 >= points.1.1 { return; }
                points.0.push(self.derive_point(evt.time + 1));
            },
            JobRole::Retire => {
                self.live.remove(&evt.job.id.get()).unwrap();
            }
        }
        self.prev_point = evt.time;
    }

    #[inline]
    fn derive_point(&self, moment: usize) -> RandPoint {
        let mut new_set = Vec::new();
        for j in self.live.values() {
            new_set.push(j.clone());
        }

        (new_set, moment)
    }

    // Traversal is arguably the most crucial operation
    // when processing a simulated placement. Doing it
    // efficiently is the difference between a PhD and
    // no PhD. So let's do it well.
    pub fn traverse<T, F>(jobs: &JobSet, res: &mut T, func: F)
        where F: Fn(&mut Self, &mut T, Event) {
        let max_capacity = jobs.get_len();
        let mut area = Area::new();
        // Efficient live job book-keeping is key. We want to add
        // and remove live jobs fast. We use a HashMap indexed by
        // the reference points that we encounter along the way.
        let mut future_refs: BinaryHeap<RefPoint> = BinaryHeap::with_capacity(max_capacity / 2);

        for (_, j) in &jobs.contents {
            // We first must account for any events (i.e. deaths) that happen earlier.
            while let Some(p) = future_refs.peek() {
                if j.birth.get() < p.1 { break; }
                let key = future_refs.pop().unwrap();
                let evt = Event::new(area.live.get(&key.2).unwrap().clone(), &area, false);
                func(&mut area, res, evt);
            }
            // THEN we account for the job itself.
            let evt = Event::new(j.clone(), &area, true);
            func(&mut area, res, evt);
            future_refs.push(RefPoint::new(j.clone()));
        }
        // Finally, we account for remaining deaths.
        while let Some(key) = future_refs.pop() {
            let evt = Event::new(area.live.get(&key.2).unwrap().clone(), &area, false);
            func(&mut area, res, evt);
        }
    }
}

// RefPoints are nothing fancier than the
// birth and death data of objects. On top of this
// type, however, we intend to build a crucial helper
// structure for our traversal algorithm.
#[derive(Eq, PartialEq, Hash, Clone, Copy)]
struct RefPoint(usize, usize, usize);

impl RefPoint {
    #[inline]
    fn new(j: Rc<Job>) -> Self {
        Self(j.birth.get(), j.death.get(), j.id.get())
    }
}

// The "crucial helper structure" mentioned above is
// a BinaryHeap. We want it to be a min-heap on the deaths
// of jobs, with tie breaks solved by job births (where there
// are no ties).
impl Ord for RefPoint {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.1.cmp(&self.1).then_with(|| { other.0.cmp(&self.0) })
    }
}

impl PartialOrd for RefPoint {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq, PartialEq)]
struct RowNum(pub usize);

impl RowNum {
    #[inline]
    fn new(idx: usize) -> Self {
        Self(idx)
    }
}

impl Ord for RowNum {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0.cmp(&self.0)
    }
}

impl PartialOrd for RowNum {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct IGCCtrl {
    pub rows:   Vec<LiveSet>,
    d:      usize,
    a:      BinaryHeap<RowNum>
}

impl IGCCtrl {
    #[inline]
    pub fn new() -> Self {
        Self {
            rows:   vec![],
            d:      0,
            a:      BinaryHeap::new()
        }
    }
}

pub mod myerrors {
    use thiserror::Error;

    #[derive(Error, Debug)]
    #[error("Jobs boxed: {}", jobs_boxed)]
    pub struct T16Error {
        pub jobs_boxed: usize,
    }

    impl T16Error {
        pub fn new(jobs_boxed: usize) -> Self {
            Self { jobs_boxed }
        }
    }
}