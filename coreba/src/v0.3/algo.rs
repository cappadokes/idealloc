use crate::utils::*;

/// Executes the core `idealloc` loop for a `max_iters`
/// number of times. At the end the jobs in `input` have
/// received placement offsets.
pub fn main_loop(mut original_input: JobSet, max_iters: u32) {
    // Measure total allocation time.
    let total_start = Instant::now();

    // Initializations...
    let mut input = Instance::new(original_input.clone());
    let (h_min, mut h_max) = input.min_max_height();
    let mut iters_done = 0;
    let mut best_opt = ByteSteps::MAX;
    let mut jobs_num_to_box = input.jobs.len() as u32;

    // The first thing to do is stabilize ε.
    let (mu_lim, small_end, big_end, _lg2r) = input.ctrl_prelude();

    let mut dummy_job = None;
    let (epsilon, mut pre_boxed) = if small_end < big_end {
        init_rogue(input.clone(), small_end, big_end)
    } else {
        jobs_num_to_box += 1;
        // Demanding that small < end leads to the condition:
        // r > lg2r * mu_lim.powi(-6)
        // Via WolframAlpha, an approximate solution to that
        // is any r > 2216.53...
        //
        // We thus plant such a "dummy" job in the instance.
        h_max = (2216.54_f64 * h_min as f64).ceil() as ByteSteps;
        let dummy = Arc::new(Job {
            size:               h_max,
            req_size:           h_max,
            birth:              0,
            death:              original_input.iter().map(|j| j.death).max().unwrap(),
            originals_boxed:    0,
            alignment:          None,
            contents:           None,
            id:                 original_input.iter().map(|j| j.id).max().unwrap() + 1,
        });
        original_input.push(dummy.clone());
        original_input.sort_unstable();
        dummy_job = Some(dummy);
        input = Instance::new(original_input);
        let (_mu_lim, small_end, big_end, _lg2r) = input.ctrl_prelude();

        init_rogue(input.clone(), small_end, big_end)
    };
    let (dumb_id, target_load) = if let Some(j) = dummy_job {
        (j.id, input.load() - j.size)
    } else { (u32::MAX, input.load()) };

    // Initializations related to the last
    // invocation of C15.
    let (_, mut mu, _, _) = pre_boxed.get_safety_info(epsilon);
    if mu > mu_lim {
        mu = 0.99 * mu_lim;
    }
    let final_h = h_max as f64 / mu;

    // Last but not least, we'll need an interference graph
    // for fast placement.
    let interference_graph = input.build_interference_graph();

    loop {
        let boxed = c_15(pre_boxed.clone(), final_h, mu);
        debug_assert!(boxed.check_boxed_originals(jobs_num_to_box), "Invalid boxing!");
        let current_opt = boxed.place(&interference_graph, iters_done, best_opt, dumb_id);
        debug_assert!(current_opt == ByteSteps::MAX || current_opt >= target_load, "Bad placement");
        if current_opt < best_opt {
            best_opt = current_opt;
        }
        iters_done += 1;
        if iters_done < max_iters && best_opt > target_load {
            pre_boxed = rogue(input.clone(), epsilon);
        } else { break; }
    };

    println!(
        "Total allocation time: {} μs",
        total_start.elapsed().as_micros()
    );
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", best_opt, target_load, (best_opt - target_load) as f64 / target_load as f64 * 100.0);

    unimplemented!()
}

/// Calls [rogue] for a variety of ε-values, returning the one
/// which results in the smallest min/max height ratio.
/// 
/// Also returns the winning value's almost-converged instance.
fn init_rogue(input: Instance, small: f64, big: f64) -> (f64, Instance) {
    let mut e = small;
    let mut min_r = f64::MAX;
    let mut best_e = e;
    let mut tries_left = 3;
    let mut best: Instance = input.clone();
    loop {
        if tries_left > 0 {
            let mut test = rogue(input.clone(), e);
            let (r, _, _, _) = test.get_safety_info(e);
            if r < min_r {
                min_r = r;
                best_e = e;
                best = test.clone();
                tries_left = 3;
            } else {
                tries_left -= 1;
            }
            e += (big - e) * 0.01;
        } else { 
            break (best_e, best); 
        }
    }
}

// Does the lower branch of T16 until its conditions don't hold.
// Returns a same-sized Instance at all cases.
fn rogue(mut input: Instance, epsilon: f64) -> Instance {
    let (_r, mu, h, is_safe) = input.get_safety_info(epsilon);
    let target_size = (mu * h).floor() as ByteSteps;

    if is_safe {
        let (x_s, x_l) = input.split_by_height(target_size);
        let small_boxed = c_15(x_s, h, mu);
        rogue(x_l.merge_with(small_boxed), epsilon)
    } else {
        // Done.
        input
    }
}

fn c_15(
    input:      Instance,
    h:          f64,
    epsilon:    f64,
) -> Instance {
    // Each bucket can be treated independently.
    // Embarassingly parallel operation. Consolidate
    // a Mutex-protected Instance.
    let res = Arc::new(Mutex::new(Instance::new(vec![])));
    input.make_buckets(epsilon)
        .into_par_iter()
        .for_each(|(h_i, unit_jobs)| {
            debug_assert!(h_i as f64 <= h, "T2 fed with zero H! (ε = {:.2})", epsilon);
            let h_param = (h / h_i as f64).floor() as ByteSteps;
            let boxed = t_2(unit_jobs, h_param, h as ByteSteps, epsilon, None);
            let mut guard = res.lock().unwrap();                
            guard.merge_via_ref(boxed);
    });

    match Arc::into_inner(res) {
        Some(v) => {
            v.into_inner().unwrap()
        },
        None    => {
            // This shouldn't happen because all threads
            // should have finished by now, and hence `res`
            // should only have one strong reference.
            panic!("Could not unwrap Arc!");
        }
    }
}

/// Helper structure for Theorem 2.
struct T2Control {
    bounding_interval:  (ByteSteps, ByteSteps),
    critical_points:    BTreeSet<ByteSteps>,
}

impl T2Control {
    fn new(jobs: &Instance) -> Self {
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
    fn gen_crit(
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

/// Buchsbaum's Theorem 2.
fn t_2(
    input:      Instance,
    h:          ByteSteps,
    // Needed because we have discarded scaling operations.
    h_real:     ByteSteps,
    epsilon:    f64,
    ctrl:       Option<T2Control>,
) -> Instance {
    let mut res_jobs: JobSet = vec![];
    let mut all_unresolved: JobSet = vec![];

    // This is a recursive function. It always has `ctrl` filled
    // with something when it calls itself.
    let ctrl = if let Some(v) = ctrl { v }
    else { T2Control::new(&input) };

    // Help vector to be used below.
    let pts_vec = ctrl.critical_points
        .iter()
        .copied()
        .collect::<Vec<ByteSteps>>();

    // We split, in as efficient a way as possible, the input's jobs
    // into groups formed by their liveness in the critical points.
    let (r_coarse, x_is) = input.split_by_liveness(&ctrl.critical_points);
    debug_assert!(!r_coarse.is_empty(), "Theorem 2 entered infinite loop");

    // X_is are going to be passed in future iterations and it makes sense
    // to make Instances out of them. R_is, however, will be immediately
    // boxed. So we remain at the JobSet abstraction.
    let r_is: Vec<JobSet> = split_ris(
            r_coarse,
            &pts_vec[..],
    );

    for r_i in r_is {
        let (boxed, mut unresolved) = lemma_1(r_i, h, h_real, epsilon);
        all_unresolved.append(&mut unresolved);
        if let Some(mut boxed) = boxed {
            res_jobs.append(&mut boxed);
        }
    }

    // We want to apply IGC to `all_unresolved`. We're going to
    // use traversal, so the jobs must be sorted.
    all_unresolved.sort_unstable();
    let igc_rows = interval_graph_coloring(all_unresolved);

    // The produced rows implicitly generate "gaps", which will be used
    // to generate each X_i's control structures. Let's find those gaps.
    let mut points_to_allocate: BTreeSet<ByteSteps> = BTreeSet::new();
    let mut row_count = 0;
    let mut jobs_buf: JobSet = vec![];
    for mut row in igc_rows {
        points_to_allocate.append(&mut gap_finder(&row, ctrl.bounding_interval));
        // The only remaining thing is to box the row and add it to the result.
        // We do not immediately box it though; we need to box together
        // as many rows as designated by the `h` argument!
        row_count += 1;
        jobs_buf.append(&mut row);
        if row_count % h == 0 {
            jobs_buf.sort_unstable();
            res_jobs.push(Arc::new(Job::new_box(jobs_buf, h_real)));
            jobs_buf = vec![];
        }
    }
    if !jobs_buf.is_empty() {
        jobs_buf.sort_unstable();
        res_jobs.push(Arc::new(Job::new_box(jobs_buf, h_real)));
    }

    // T2 is going to be called for all X_is in parallel.
    let res = Arc::new(Mutex::new(Instance::new(res_jobs)));

    // Missing tasks: (i) set X_i control structures up, do recursion for each
    // (ii) consolidate Arc-Mutex-protected res.
    x_is.into_par_iter()
        .for_each(|(i, x_i)| {
        // We shall be pulling points from this iterator.
        let mut pts_alloc_iter = points_to_allocate.iter().copied().peekable();

        // Where the X_i's bounding interval starts, ends.
        // The points to allocate must include AT LEAST one value which:
        //  1. bi_start < v < bi_end
        //  2. at least one job in X_i is live @ v
        // We know for a fact that this is not always the case--we then
        // inject a point of our own.
        let (bi_start, bi_end) = (pts_vec[i], pts_vec[i + 1]);
        let mut crit_pts = BTreeSet::from([bi_start, bi_end]);

        // Let's check first if there's any suitable point in alloc.
        let mut pts_ready = false;
        loop {
            // We need a loop because there may be more
            // than one points that must be inserted.
            if let Some(v) = pts_alloc_iter.peek() {
                if *v <= bi_start {
                    pts_alloc_iter.next();
                } else if *v >= bi_end {
                    break;
                } else {
                    // This is a suitable point w.r.t. Req. #1.
                    // ...but what about Req. #2 ?
                    if !pts_ready &&
                        x_i.jobs
                        .iter()
                        .any(|j| j.is_live_at(*v) ) {
                            pts_ready = true;
                    }
                    // In any case we insert the point.
                    crit_pts.insert(pts_alloc_iter.next().unwrap());
                }
            } else {
                break;
            }
        }
        // We've exhausted all valid points to allocate to this X_i.
        if !pts_ready {
            // Injection if no liveness has been found.
            while !crit_pts.insert(
                T2Control::gen_crit(&x_i, bi_start, bi_end)
            ) {};
        }

        let x_i_res = t_2(x_i, h, h_real, epsilon, Some(T2Control {
            bounding_interval:  (bi_start, bi_end),
            critical_points:    crit_pts
        }));

        let mut guard = res.lock().unwrap();
        guard.merge_via_ref(x_i_res);
    });

    match Arc::into_inner(res) {
        Some(i)   => {
            i.into_inner().unwrap()
        },
        None  => { panic!("Bad multithreading @ T2!"); }
    }
}

/// Finds gaps in between jobs of an IGC row, and adds
/// their endpoints to an ordered set, eventually returned.
/// 
/// Used in the context of Theorem 2.
fn gap_finder(row_jobs: &JobSet, (alpha, omega): (ByteSteps, ByteSteps)) -> BTreeSet<ByteSteps> {
    let mut res = BTreeSet::new();
    // Again we use event traversal. Row jobs are already sorted
    // since IGC itself is a product of event traversal.
    let mut evts = get_events(&row_jobs);
    // We either have found the next gap's start, or we haven't.
    // Initialize it optimistically to the left extreme of our
    // horizon.
    let mut gap_start = Some(alpha);

    while let Some(evt) = evts.pop() {
        match evt.evt_t {
            EventKind::Birth    => {
                if let Some(v) = gap_start {
                    if v < evt.time {
                        res.insert(v);
                        res.insert(evt.time);
                    }
                    gap_start = None;
                }
            },
            EventKind::Death    => { gap_start = Some(evt.time); }
        }
    }
    let last_gap_start = gap_start.unwrap();
    if last_gap_start < omega { res.insert(last_gap_start); }

    res
}

/// Implements Buchsbaum et al's Lemma 1.
fn lemma_1(
    input:  JobSet,
    h:      ByteSteps,
    h_real: ByteSteps,
    e: f64,
) -> (Option<JobSet>, JobSet) {
    // First we cut two strips, each having `outer_num` jobs
    // (if enough jobs exist)
    let outer_num = h * (1.0 / e.powi(2)).ceil() as ByteSteps;
    let mut total_jobs = input.len();
    if total_jobs > outer_num {
        let mut iter = input.into_iter().peekable();
        let mut outer = strip_cuttin(&mut iter, true, outer_num);
        // We know for a fact that there are more jobs to carve.
        let mut outer_2 = strip_cuttin(&mut iter, false, outer_num);

        if  total_jobs > 2 * outer_num {
            // The inner strips will contain that many
            // jobs in total.
            total_jobs -= 2 * outer_num;
            // Counter of inner-stripped jobs.
            let mut inner_jobs = 0;
            let mut inner_vert: Vec<JobSet> = vec![];
            let mut inner_hor: Vec<JobSet> = vec![];
            // Max size of each inner strip.
            let inner_num = h * (1.0 / e).ceil() as ByteSteps;
            while inner_jobs < total_jobs {
                iter = iter.sorted_unstable().peekable();
                let inner = strip_cuttin(&mut iter, true, inner_num);
                inner_jobs += inner.len();
                inner_vert.push(inner);
                if inner_jobs == total_jobs { break; }
                let inner_2 = strip_cuttin(&mut iter, false, inner_num);
                inner_jobs += inner_2.len();
                inner_hor.push(inner_2);
            }

            outer.append(&mut outer_2);
            (Some(strip_boxin(inner_vert, inner_hor, h, h_real)), outer)
        } else {
            outer.append(&mut outer_2);

            (None, outer)
        }
    } else {
        (None, input)
    }
}

/// Helper function for Lemma 1. Splits all inner
/// strips into boxes containing `group_size` jobs each.
/// 
/// The real size of each box is `box_size`.
fn strip_boxin(
    verticals:      Vec<JobSet>,
    horizontals:    Vec<JobSet>,
    group_size:     ByteSteps,
    box_size:       ByteSteps,
) -> JobSet {
    let mut res_set = strip_box_core(verticals, group_size, box_size, true);
    res_set.append(&mut strip_box_core(horizontals, group_size, box_size, false));

    res_set
}

/// Helper function for Lemma 1. Splits the jobs
/// of a single strip into boxes containing `group_size` jobs each.
/// 
/// The real size of each box is `box_size`.
fn strip_box_core(
    strips:         Vec<JobSet>,
    group_size:     ByteSteps,
    box_size:       ByteSteps,
    vertical:       bool,
) -> JobSet {
    let mut res: JobSet = vec![];
    for strip in strips {
        // We must repeatedly divide each strip in groups
        // of size `group_size` and box them.
        //
        // We shall make reuse of `strip_cuttin`.
        let mut iter = if vertical {
            strip.into_iter()
                // Whether the strip is a vertical or horizontal one
                // designates the sorting before selection.
                .sorted_unstable_by(|a, b| { b.death.cmp(&a.death) })
                .peekable()
        } else {
            strip.into_iter()
                .sorted_unstable()
                .peekable()
        };
        while iter.peek().is_some() {
            let mut to_cut = strip_cuttin(&mut iter, true, group_size);
            to_cut.sort_unstable();
            res.push(Arc::new(
                Job::new_box(
                    to_cut,
                     box_size)
                    )
                );
        }
    };

    res
}

/// Helper function for Lemma 1. Selects `to_take`
/// jobs from a given iterator.
fn strip_cuttin(
    iter:       &mut Peekable<std::vec::IntoIter<Arc<Job>>>,
    is_sorted:  bool,
    to_take:    ByteSteps,
) -> JobSet {
    if !is_sorted {
        // Sort remaining jobs by decreasing
        // death.
        *iter = iter.sorted_unstable_by(|a, b| {
            b.death.cmp(&a.death)
        }).peekable();
    }
    let mut stripped = 0;
    // This vector collects the outer vertical/horizontal strip jobs.
    let mut res = vec![];

    // This condition helps us check if we've run
    // out of jobs.
    while let Some(j) = iter.next() {
        stripped += 1;
        res.push(j);
        // This condition helps check if we're
        // done with the outer vert. strip.
        if stripped == to_take { break; }
    };
    
    res
}

// The following operations are considered
// parts of the algorithm, so they're put here instead of
// the file hosting the rest of the impls.
impl Instance {
    /// Records all jobs that are concurrently live with each job.
    pub fn build_interference_graph(&self) -> (InterferenceGraph, PlacedJobRegistry) {
        let mut res = HashMap::new();
        let mut reg_res = HashMap::new();
        // We will use traversal. As we process events, we will be updating
        // a set of live jobs.
        let mut live: HashMap<u32, Rc<PlacedJob>> = HashMap::new();
        let mut evts = get_events(&self.jobs);

        while let Some(e) = evts.pop() {
            match e.evt_t {
                EventKind::Birth    => {
                    let init_vec: PlacedJobSet = live.values()
                        .cloned()
                        .collect();
                    let new_entry = Rc::new(PlacedJob::new(e.job.clone()));
                    // First, add a new entry, initialized to the currently live jobs.
                    res.insert(e.job.id, init_vec);
                    reg_res.insert(e.job.id, new_entry.clone());
                    for (_, j) in &live {
                        // Update currently live jobs' vectors with the new entry.
                        let vec_handle = res.get_mut(&j.descr.id).unwrap();
                        vec_handle.push(new_entry.clone());
                    }
                    // Add new entry to currently live jobs.
                    live.insert(e.job.id, new_entry);
                },
                EventKind::Death    => {
                    // Remove job from currently live.
                    live.remove(&e.job.id);
                },
            }
        }

        (res, reg_res)
    }

    // Unbox and tighten. Probably needs to be
    // implemented for another type or YIELD
    // another type.
    fn place(
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

    /// Splits instance to unit-height buckets, in the
    /// context of Corollary 15. Each bucket is indexed
    /// by the height to be given to Theorem 2.
    fn make_buckets(self, epsilon: f64) -> HashMap<ByteSteps, Instance> {
        let mut res = HashMap::new();
        let mut prev_floor = 1.0 / (1.0 + epsilon);
        let mut i = 0;
        let mut source = self;
        while source.jobs.len() > 0 {
            let h = (1.0 + epsilon).powi(i);
            if source.jobs.iter().any(|j| j.size as f64 > prev_floor && j.size as f64 <= h) {
                let h_split = h.floor() as ByteSteps;
                let (toward_bucket, rem) = source.split_by_height(h_split);
                res.insert(h_split, toward_bucket);
                source = rem;
            }
            prev_floor = h;
            i += 1;
        }

        res
    }
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