use crate::helpe::*;

impl Instance {
    // Unbox and tighten. Probably needs to be
    // implemented for another type or YIELD
    // another type.
    pub fn place(
        self, 
        ig:             &(InterferenceGraph, PlacedJobRegistry), 
        iters_done:     u32,
        makespan_lim:   ByteSteps,
        dumb_id:        u32,
    ) -> ByteSteps {
        // Measure unboxing time.
        let row_size = self.jobs[0].size;
        let loose = get_loose_placement(Arc::into_inner(self.jobs).unwrap(), 0, UnboxCtrl::SameSizes(row_size), &ig.1, dumb_id);
        do_best_fit(loose, &ig.0, iters_done, makespan_lim, false)
    }
}

pub fn get_loose_placement(
    mut jobs:           JobSet,
    mut start_offset:   ByteSteps,
    control_state:      UnboxCtrl,
    ig:                 &PlacedJobRegistry,
    dumb_id:            u32,
) -> LoosePlacement {
    let mut res = BinaryHeap::new();
    match control_state {
        UnboxCtrl::SameSizes(row_height)    => {
            // If jobs are same-sized, do IGC!
            // The jobs in each row will be non-overlapping.
            jobs.sort_unstable();
            for row in interval_graph_coloring(jobs) {
                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                start_offset += row_height;
            }
        },
        UnboxCtrl::NonOverlapping   => {
            // If jobs are non-overlapping, they can all be put
            // at the same offset.
            for j in jobs {
                if j.is_original() {
                    if j.id != dumb_id {
                        let to_put = ig.get(&j.id).unwrap().clone();
                        to_put.offset.set(start_offset);
                        res.push(to_put.clone());
                    }
                } else {
                    res.append(&mut get_loose_placement(Arc::unwrap_or_clone(j).contents.unwrap(), start_offset, UnboxCtrl::Unknown, ig, dumb_id));
                }
            }
        },
        UnboxCtrl::Unknown  => {
            // We must find out on our own the jobs' characteristics.
            // First check if they're all of the same size.
            let size_probe = jobs[0].size;
            if jobs.iter()
                .skip(1)
                .all(|j| { j.size == size_probe }) {
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::SameSizes(size_probe), ig, dumb_id));
            } else {
                // Then check if they're non-overlapping. We can do that
                // by demanding that the corresponding events are always
                // alternating between births and deaths.
                jobs.sort_unstable();
                let mut evts = get_events(&jobs);
                let mut last_was_birth = false;
                let mut non_overlapping = true;
                while let Some(e) = evts.pop() {
                    match e.evt_t {
                        EventKind::Birth    => {
                            if last_was_birth {
                                non_overlapping = false;
                                break;
                            }
                            last_was_birth = true;
                        },
                        EventKind::Death    => {
                            last_was_birth = false;
                        }
                    }
                }
                if non_overlapping {
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                } else {
                    // Here we know for a fact that the jobs are of multiple sizes, and they're also
                    // overlapping. One idea is to use "big rocks first". This can be combined with
                    // clustering (maybe more than one jobs are of the same size and can thus be
                    // put in the same cluster).
                    let mut size_buckets: HashMap<ByteSteps, JobSet> = HashMap::new();
                    for j in jobs {
                        size_buckets.entry(j.size)
                            .and_modify(|e| e.push(j.clone()))
                            .or_insert(vec![j]);
                    }
                    for (row_height, mut size_class) in size_buckets.into_iter()
                        .sorted_unstable_by(|a, b| { b.0.cmp(&a.0)}) {
                            size_class.sort_unstable();
                            let igc_rows = interval_graph_coloring(size_class);
                            let num_rows = igc_rows.len();
                            for row in igc_rows {
                                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                                start_offset += row_height * num_rows;
                            }
                    }
                }
            }
        }
    };

    res
}

/// Performs best/first-fit placement of an already-ordered collection
/// of jobs (by some symbolic offset). Returns the resulting makespan.
/// 
/// Stops early if the running makespan exceeds a pre-defined limit.
/// Prints the total time of the operation.
pub fn do_best_fit(
    mut loose:      LoosePlacement,
    ig:             &InterferenceGraph,
    iters_done:     u32,
    makespan_lim:   ByteSteps,
    first_fit:      bool,
) -> ByteSteps {
    let mut max_address = 0;
    // Traverse loosely placed jobs in ascending offset.
    while let Some(to_squeeze) = loose.pop() {
        let min_gap_size = to_squeeze.descr.size;
        let mut offset_runner = 0;
        let mut smallest_gap = ByteSteps::MAX;
        let mut best_offset: Option<ByteSteps> = None;
        // Traverse already-squeezed jobs that overlap with
        // the current one in ascending offset. You're looking
        // for the smallest gap which fits the job, alignment
        // requirements included.
        let mut jobs_vec = ig.get(&to_squeeze.descr.id)
            .unwrap()
            .iter()
            .filter(|j| { j.times_squeezed.get() == iters_done + 1 })
            .sorted_unstable()
            .rev()
            .peekable();

        while let Some(next_job) = jobs_vec.peek() {
            let njo = next_job.offset.get();
            if njo > offset_runner {
                let test_addr = if let Some(a) = to_squeeze.descr.alignment {
                    if offset_runner < a {
                        a
                    } else if offset_runner % a != 0 {
                        (offset_runner / a + 1) * a
                    } else {
                        offset_runner
                    }
                } else {
                    offset_runner
                };
                if njo > test_addr && njo - test_addr >= min_gap_size {
                    if !first_fit {
                        let gap = njo - test_addr;
                        if gap < smallest_gap {
                            smallest_gap = gap;
                            best_offset = Some(test_addr);
                        }
                    } else {
                        best_offset = Some(test_addr);
                        break;
                    }
                }
                offset_runner = test_addr.max(next_job.next_avail_addr());
            } else {
                offset_runner = offset_runner.max(next_job.next_avail_addr());
            }
            jobs_vec.next();
        }
        if let Some(o) = best_offset {
            to_squeeze.offset.set(o);
        } else { to_squeeze.offset.set(offset_runner); }
        to_squeeze.times_squeezed.set(iters_done + 1);
        let cand_makespan = to_squeeze.next_avail_addr();
        if cand_makespan > max_address {
            max_address = cand_makespan;
            if max_address > makespan_lim {
                return ByteSteps::MAX;
            }
        }
    };

    max_address
}