use std::collections::BTreeSet;
use std::rc::Rc;
use crate::elements::Job;
use crate::elements::{JobSet, LiveSet};
use crate::algo::sort_inc_birth;

pub struct PlacedSet {
    // Help structure for final placement.
    // This is a min-heap of offset-ordered jobs,
    // with tie breaks made by next_avail_addr().
    pub stuff: BTreeSet<Rc<Job>>,
    pub max_addr: usize,
    //pub horizon: (usize, usize),
}

impl PlacedSet {
    #[inline]
    pub fn new() -> Self {
        PlacedSet {
            stuff: BTreeSet::new(),
            max_addr: 0,
            //horizon: (usize::MAX, usize::MIN),
        }
    }

    #[inline]
    fn append(&mut self, mut other: Self) {
        self.max_addr = self.max_addr.max(other.max_addr);
        /*
        self.horizon = (
            self.horizon.0.min(other.horizon.0),
            self.horizon.1.max(other.horizon.1),
        );
        */
        self.stuff.append(&mut (other.stuff));
    }

    #[inline]
    fn push(&mut self, entry: Rc<Job>) {
        self.max_addr = self.max_addr.max(entry.next_avail_addr());
        //let (start, end) = entry.endpoints();
        //self.horizon = (self.horizon.0.min(start), self.horizon.1.max(end));
        self.stuff.insert(entry);
    }

    fn _valid_placement(&self) -> bool {
        for j in &self.stuff {
            for k in &self.stuff {
                if j.id() != k.id() && j.overlaps_with(&k) {
                    if j.space_overlaps_with(&k) {
                        println!("{:?} overlaps with\n {:?}", j, k);
                        return false;
                    }
                }
            }
        }

        true
    }

    pub fn is_empty(&self) -> bool {
        self.stuff.is_empty()
    }
/*
    pub fn filter_relevant(&self, other: &Self) -> BTreeSet<Rc<Job>> {
        //let hor = other.horizon;
        self.stuff
            .iter()
            .filter(|j| j.is_live_within(&hor))
            .cloned()
            .collect()
    }*/
}

pub fn do_placement(
    mut inp: LiveSet,
    mut watermark: usize,
    just_boxes: bool,
    just_jobs: bool,
    overlappin: bool,
    tighten: bool,
) -> PlacedSet {
    let mut res = PlacedSet::new();

    if just_boxes {
        if overlappin {
            let mut helper = JobSet::new();
            helper.contents = inp;
            sort_inc_birth(&mut helper.contents);
            for row in helper.igc() {
                let placed = do_placement(row, watermark, true, false, false, false);
                if tighten {
                    tighten_placement(placed, &mut res);
                } else {
                    res.append(placed);
                }
                watermark = res.max_addr;
            }
        } else {
            //for j in inp.into_values() {
            for j in inp.values() {
                //let innards = j.contents.replace(None).unwrap();
                let innards = j.contents.borrow().clone().unwrap();
                let placed = do_placement(
                    innards,
                    watermark,
                    j.just_boxes.get(),
                    j.just_jobs.get(),
                    j.overlappin.get(),
                    false,
                );
                res.append(placed);
            }
        }
    } else if just_jobs {
        if !overlappin {
            for j in inp.into_values() {
                j.restore_size();
                j.place(watermark);
                res.push(j);
            }
        } else {
            let probe = inp[0].original_size();
            if !inp.iter().any(|(_, j)| j.original_size() != probe) {
                // All jobs are same-sized and overlappin,
                // thus we can IGC!
                let mut helper = JobSet::new();
                helper.contents = inp;
                sort_inc_birth(&mut helper.contents);
                for row in helper.igc() {
                    let placed =
                        do_placement(row, watermark, false, true, false, false);
                    res.append(placed);
                    watermark = res.max_addr;
                }
            } else {
                // Jobs are overlappin and of various sizes.
                // Put big rocks first and pray.
                //
                // Locally, the order of the jobs doesn't matter. There
                // are global dependencies though. The best ordering we've
                // experimented with is area (space * time load).
                //
                // We tried other smart-sounding stuff, like an exhaustive
                // search on the permutations of the jobs--didn't work.
                inp.sort_unstable_by(|_, a, _, b| b.area().cmp(&a.area()));
                for j in inp.into_values() {
                    j.restore_size();
                    j.place(watermark);
                    watermark = j.next_avail_addr();
                    res.push(j);
                }
            }
        }
    } else {
        let mut helper = JobSet::new();
        helper.contents = inp;
        let (mut jobs, mut boxes) = helper.partition_by(|j| j.is_original(), false);
        jobs.check_overlap(true);
        boxes.check_overlap(false);
        let placed = do_placement(
            jobs.contents,
            watermark,
            false,
            true,
            jobs.overlappin,
            false,
        );
        res.append(placed);
        if overlappin {
            watermark = res.max_addr;
        }
        let placed = do_placement(
            boxes.contents,
            watermark,
            true,
            false,
            boxes.overlappin,
            false,
        );
        res.append(placed);
    }

    debug_assert!(res._valid_placement(), "Fucked up!");

    res
}

pub fn tighten_placement(inp: PlacedSet, reference: &mut PlacedSet) {
    let inp = inp.stuff;

    for j in inp {
        let mut target = j.size();
        let a = j.alignment();
        if a != 0 {
            target += a - 1;
        }
        let mut next_avail = 0;
        let mut runner_vec = reference
            .stuff
            .iter()
            .filter(|nj| nj.overlaps_with(&j))
            .peekable();
        while let Some(next_job) = runner_vec.peek() {
            //if next_job.offset() > next_avail {
            let njo = next_job.offset();
            if njo > next_avail && njo - next_avail >= target {
                break;
            }
            //}
            next_avail = next_avail.max(next_job.next_avail_addr());
            runner_vec.next();
        }
        j.place(next_avail);
        reference.push(j);
    }
}