use crate::helpe::*;

/// Variant of Theorem 16 (p. 561-562), owed to the empirical
/// realization that *ε must be greater than 1 in order for
/// boxing's invariants to be preserved*.
/// 
/// Theorem 16 comprises two phases of computation. In the first
/// phase, input jobs are partitioned into "small" and "big" ones.
/// Corollary 15 (p. 561) is used to box the small ones, and the
/// resulting boxes are merged along with the big ones into a new
/// instance--for which Theorem 16 is again recursively called.
/// 
/// This function implements the aforementioned first phase
/// until boxing's invariants are broken.
pub fn rogue(input: Rc<Instance>, epsilon: f64) -> Rc<Instance> {
    let (r, mu, h, is_safe) = input.get_safety_info(epsilon);
    let target_size = (mu * h).floor() as ByteSteps;

    if is_safe {
        // p. 562: "Assume first that lg^2r >= 1 / ε [...]"
        debug_assert!(r.log2().powi(2) >= 1.0 / epsilon);
        let (x_s, x_l) = input.split_by_height(target_size);
        let small_boxed = c_15(Rc::new(x_s), h, mu);
        rogue(x_l.merge_with(small_boxed), epsilon)
    } else {
        // Done.
        input
    }
}

#[inline(always)]
pub fn c_15(
    input:      Rc<Instance>,
    h:          f64,
    epsilon:    f64,
) -> Instance {
    // Each bucket can be treated independently.
    // Embarassingly parallel operation. Consolidate
    // a Mutex-protected Instance.
    let res = Arc::new(Mutex::new(Instance::new(vec![])));
    Instance::make_buckets(input, epsilon)
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
            res_jobs.push(Arc::new(Job::new_box(jobs_buf, h_real)));
            jobs_buf = vec![];
        }
    }
    if !jobs_buf.is_empty() {
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

/// Implements Buchsbaum et al's Lemma 1.
#[inline(always)]
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
    if total_jobs > 2 * outer_num {
        // Copy jobs into two ordered sets: one for cutting verticals
        // and another for horizontals.
        let mut hor_source: IndexMap<u32, Arc<Job>> = input
            .iter()
            .cloned()
            .map(|j| (j.id, j))
            // Largest deaths go to the end (again, we'll be popping).
            .sorted_unstable_by(|(_, a), (_, b)| {
                a.death.cmp(&b.death)
            })
            .collect();
        let mut vert_source: IndexMap<u32, Arc<Job>> = input
            // Normally sorted by increasing birth.
            .into_iter()
            // Reverse, since we'll be popping from the IndexMap.
            .rev()
            .map(|j| (j.id, j))
            .collect();
        let outer_vert: BinaryHeap<VertStripJob> = strip_cuttin(&mut vert_source, &mut hor_source, outer_num);
        // We know for a fact that there are more jobs to carve.
        let outer_hor: BinaryHeap<HorStripJob> = strip_cuttin(&mut hor_source, &mut vert_source, outer_num);
        // The inner strips will contain that many
        // jobs in total.
        total_jobs -= 2 * outer_num;
        // Counter of inner-stripped jobs.
        let mut inner_jobs = 0;
        // Max size of each inner strip.
        let inner_num = h * (1.0 / e).ceil() as ByteSteps;
        let mut inner_vert: Vec<BinaryHeap<VertStripJob>> = vec![];
        let mut inner_hor: Vec<BinaryHeap<HorStripJob>> = vec![];
        while inner_jobs < total_jobs {
            let vert_strip: BinaryHeap<VertStripJob> = strip_cuttin(&mut vert_source, &mut hor_source, inner_num);
            inner_jobs += vert_strip.len();
            inner_vert.push(vert_strip);
            if inner_jobs == total_jobs { break; }
            let hor_strip: BinaryHeap<HorStripJob> = strip_cuttin(&mut hor_source, &mut vert_source, inner_num);
            inner_jobs += hor_strip.len();
            inner_hor.push(hor_strip);
        }
        debug_assert!(vert_source.len() == 0 && hor_source.len() == 0);

        (
            Some(strip_boxin(inner_vert, inner_hor, h, h_real)),
            outer_vert.into_iter()
                .map(|vj| vj.job)
                .chain(outer_hor.into_iter()
                        .map(|hj| hj.job))
                .collect()
        )
    } else {
        (None, input)
    }
}