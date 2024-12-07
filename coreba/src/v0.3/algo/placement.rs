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
        start_addr:     ByteSteps
    ) -> ByteSteps {
        // Measure unboxing time.
        let row_size = self.jobs[0].size;
        let loose = get_loose_placement(Arc::into_inner(self.jobs).unwrap(), 0, UnboxCtrl::SameSizes(row_size), &ig.1, dumb_id);
        do_best_fit(loose, &ig.0, iters_done, makespan_lim, false, start_addr)
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
                            for row in igc_rows {
                                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                                start_offset += row_height;
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
    start_addr:     ByteSteps,
) -> ByteSteps {
    // TEMP PLOT BOILERPLATE START
    //use plotters::prelude::*;
    //let mut series: Vec<Rc<PlacedJob>> = vec![];
    // TEMP PLOT BOILERPLATE END

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
                let test_off = to_squeeze.get_corrected_offset(start_addr, offset_runner);
                if njo > test_off && njo - test_off >= min_gap_size {
                    if !first_fit {
                        let gap = njo - test_off;
                        if gap < smallest_gap {
                            smallest_gap = gap;
                            best_offset = Some(test_off);
                        }
                    } else {
                        best_offset = Some(test_off);
                        break;
                    }
                }
                offset_runner = test_off.max(next_job.next_avail_offset());
            } else {
                offset_runner = offset_runner.max(next_job.next_avail_offset());
            }
            jobs_vec.next();
        }
        if let Some(o) = best_offset {
            to_squeeze.offset.set(o);
        } else { to_squeeze.offset.set(offset_runner); }
        to_squeeze.times_squeezed.set(iters_done + 1);
        let cand_makespan = to_squeeze.next_avail_offset();
        if cand_makespan > max_address {
            max_address = cand_makespan;
            if max_address > makespan_lim {
                return ByteSteps::MAX;
            }
        }
        /*
        // What follows is a temporary, quick-and-dirty
        // visual debugging aid.
        //
        // After squeezing each next job, we plot it onto
        // a chart.
        series.push(to_squeeze);
        let backend = SVGBackend::new("/home/cappadokes/Desktop/idealloc/coreba/debug.svg", (1920, 1080))
            .into_drawing_area();
        backend.fill(&WHITE).unwrap();
        let backend = backend.margin(10, 10, 10 , 10);
        let max_death = loose.iter()
            .map(|j| j.descr.death)
            .max()
            .unwrap();
        let jobs: JobSet = loose.iter()
            .map(|j| j.descr.clone())
            .sorted_unstable()
            .collect();
        let y_end = (1.5 * (get_load(&jobs) as f64)).ceil() as ByteSteps;
        let mut chart = ChartBuilder::on(&backend)
            .build_cartesian_2d(0..max_death, 0..y_end)
            .unwrap();
        chart.configure_mesh()
            .draw()
            .unwrap();
        chart.draw_series(
            series.iter()
                .fold(vec![], |mut acc, pj| {
                    let left_x = pj.descr.birth;
                    let upper_y = pj.next_avail_offset();
                    let right_x = pj.descr.death;
                    let lower_y = pj.offset.get();
                    acc.push(
                        Rectangle::new(
                            [(left_x, upper_y), (right_x, lower_y)],
                            ShapeStyle {
                                color: BLACK.into(),
                                filled: false,
                                stroke_width: 1,
                            }
                        )
                    );

                    acc
                })
        ).unwrap();
        backend.present().unwrap();
        */
    };

    max_address
}