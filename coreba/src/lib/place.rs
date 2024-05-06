use std::collections::BTreeSet;
use std::rc::Rc;
use std::time::Instant;

use indexmap::IndexMap;

use crate::elements::{InterferenceGraph, Job};
use crate::elements::{JobSet, LiveSet};
use crate::algo::sort_inc_birth;
use crate::utils::Area;

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
                    tighten_placement(placed, &mut helper);
                } else {
                    res.append(placed);
                }
                watermark = res.max_addr;
            }
        } else {
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
                j.place(watermark, false);
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
                    j.place(watermark, false);
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

pub fn tighten_placement(
    inp:    PlacedSet, 
    jobs:   &mut JobSet
) -> PlacedSet {
    let now = Instant::now();
    let mut reference = PlacedSet::new();

    let inp = inp.stuff;
    let mut interference: InterferenceGraph = IndexMap::default();
    jobs.sort_inc_birth();
    Area::traverse(jobs, &mut interference, Area::interference_update);
    let _interf_time = now.elapsed();
    //println!("Interference graph building took {} seconds", interf_time.as_secs());

    let now = Instant::now();
    for j in inp {
        let target = j.size();
        let mut next_avail = 0;
        // Squeezing potentially changes Job ordering--sets of the
        // interference graph can't be trusted!
        let runner_set: BTreeSet<Rc<Job>> = interference.get(&j.id())
            .unwrap()
            .into_iter()
            .filter(|j| {
                j.is_squeezed()
            })
            .cloned()
            .collect();
        let mut runner_vec = runner_set.iter()
            .peekable();
        let mut smallest_gap = usize::MAX;
        let mut best_offset = None;
        while let Some(next_job) = runner_vec.peek() {
            let njo = next_job.offset();
            if njo > next_avail {
                let a = j.alignment();
                let test_addr = if a == 0 {
                    next_avail
                } else {
                    if next_avail < a {
                        a
                    } else if next_avail % a != 0 {
                        (next_avail / a + 1) * a
                    } else {
                        next_avail
                    }
                };
                if njo > test_addr && njo - test_addr >= target {
                    let gap = njo - test_addr;
                    if gap < smallest_gap {
                        smallest_gap = gap;
                        best_offset = Some(test_addr);
                    }
                }
                next_avail = test_addr.max(next_job.next_avail_addr());
            } else {
                next_avail = next_avail.max(next_job.next_avail_addr());
            }
            runner_vec.next();
        }
        if let Some(o) = best_offset {
            j.place(o, true);
        } else { j.place(next_avail, true); }
        reference.push(j);
    }
    let _loop_time = now.elapsed();

    reference
}