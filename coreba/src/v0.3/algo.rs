use crate::utils::*;

/// Executes the core `idealloc` loop for a `max_iters`
/// number of times. At the end the jobs in `input` have
/// received placement offsets.
pub fn main_loop(input: JobSet, max_iters: u32) {
    use std::time::Instant;

    // Initializations...
    let mut input = Instance::new(input);
    let mut iters_done = 0;
    let mut best_opt = ByteSteps::MAX;
    let jobs_num_to_box = input.jobs.len() as u32;

    // Measure total allocation time.
    let total_start = Instant::now();
    let epsilon = Epsilon::new(&mut input);

    // Early stop in case of stumbling on perfect solution
    while iters_done < max_iters && best_opt > input.load() {
        let boxed = t_16(input.clone(), epsilon.val);
        debug_assert!(boxed.total_originals_boxed() == jobs_num_to_box, "Invalid boxing!");
        let placed = boxed.place();
        let current_opt = placed.opt();
        if current_opt < best_opt {
            best_opt = current_opt;
            input.update_offsets();
        }
        iters_done += 1;
    }

    println!(
        "Total allocation time: {} μs",
        total_start.elapsed().as_micros()
    );
}

/// Implements Theorem 16 from Buchsbaum et al. Returns
/// a modified [`Instance`] in case of convergence, and
/// the number of original jobs that this run managed
/// to box otherwise.
fn t_16(mut input: Instance, epsilon: f64) -> Instance {
    match t_16_cond(&mut input, epsilon) {
        (true, _, _)    => {
            let h_max = input.min_max_height().1 as f64;
            c_15(
                input,
                (h_max / epsilon).ceil(),
                epsilon,
            )
        },
        (false, mu, h)  => {
            let target_size = (mu * h).floor() as ByteSteps;
            assert!(target_size > input.min_max_height().0, "ε-convergence can't be avoided after all.");
            let (x_s, x_l) = input.split_by_height(target_size);
            let small_boxed = c_15(x_s, h, mu);
            // TODO: demystify old impl's check for jobs_boxed.
            t_16(x_l.merge_with(small_boxed), epsilon)
        }
    }
}

/// Checks if Theorem 16 has converged.
fn t_16_cond(input: &mut Instance, epsilon: f64) -> (bool, f64, f64) {
    let (h_min, h_max) = input.min_max_height();
    let r = h_max as f64 / h_min as f64;

    let lg2r = r.log2().powi(2);
    let mu = epsilon / lg2r;

    (
        lg2r < 1.0 / epsilon,
        mu,
        (mu.powi(5) * (h_max as f64) / lg2r).ceil(),
    )
}

fn c_15(
    input:      Instance,
    h:          f64,
    epsilon:    f64,
) -> Instance {
    // Core assumption of Corollary 15: heights of all jobs
    // are at most ε*h. Boxes returned are size h, that is, BIGGER
    // than their contents. ε must thus be < 1.
    assert!(epsilon < 1.0, "ε >= 1.0 @ C15!");

    use rayon::prelude::*;
    use std::sync::Mutex;

    // Each bucket can be treated independently.
    // Embarassingly parallel operation. Consolidate
    // a Mutex-protected Instance.
    let res = Arc::new(Mutex::new(Instance::new(vec![])));
    input.make_buckets(epsilon)
        .into_par_iter()
        .for_each(|(h_i, unit_jobs)| {
            assert!(h_i as f64 <= h, "T2 fed with zero H!");
            let h_param = (h / h_i as f64).floor() as ByteSteps;
            let boxed = t_2(unit_jobs, h_param, h as ByteSteps, epsilon, None);
            // TODO: how many hierarchy levels are present after T2?
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
        assert!(start < end, "Same-ends horizon met.");
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

        assert!(left + 1 < right, "Bad range found.");
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
    mut input:  Instance,
    h:          ByteSteps,
    // Needed because we have discarded scaling operations.
    h_real:     ByteSteps,
    epsilon:    f64,
    ctrl:       Option<T2Control>,
) -> Instance {
    let mut res = Instance::new(vec![]);
    let mut all_unresolved: JobSet = vec![];

    // This is a recursive function. It always has `ctrl` filled
    // with something when it calls itself.
    let ctrl = if let Some(v) = ctrl { v }
    else { T2Control::new(&input) };

    // We split, in as efficient a way as possible, the input's jobs
    // into groups formed by their liveness in the critical points.
    let (r_coarse, x_is) = input.split_by_liveness(&ctrl.critical_points);
    assert!(!r_coarse.is_empty(), "Theorem 2 entered infinite loop");
    // X_is are going to be passed in future iterations and it makes sense
    // to make Instances out of them. R_is, however, will be immediately
    // boxed. So we remain at the JobSet abstraction.
    let r_is: Vec<JobSet> = split_ris(
            r_coarse,
        &ctrl.critical_points
            .iter()
            .copied()
            .collect::<Vec<ByteSteps>>()[..]
    );

    for r_i in r_is {
        let (boxed, mut unresolved) = lemma_1(r_i, h, h_real, epsilon);
        all_unresolved.append(&mut unresolved);
        if let Some(boxed) = boxed {
            res = res.merge_with(boxed);
        }
    }

    // We want to apply IGC to `all_unresolved`. We're going to
    // use traversal, so the jobs must be sorted.
    all_unresolved.sort_unstable();
    let igc_rows = interval_graph_coloring(all_unresolved);

    unimplemented!()
}

/// Implements Buchsbaum et al's Lemma 1.
fn lemma_1(
    input:  JobSet,
    h:      ByteSteps,
    h_real: ByteSteps,
    e: f64,
) -> (Option<Instance>, JobSet) {
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
) -> Instance {
    let mut res_set = strip_box_core(verticals, group_size, box_size, true);
    res_set.append(&mut strip_box_core(horizontals, group_size, box_size, false));
    res_set.sort_unstable();
    Instance::new(res_set)
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
                .sorted()
                .peekable()
        };
        while iter.peek().is_some() {
            res.push(Arc::new(
                Job::new_box(
                    strip_cuttin(&mut iter, true, group_size),
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
    // Unbox and tighten. Probably needs to be
    // implemented for another type or YIELD
    // another type.
    fn place(self) -> Self {
        unimplemented!()
    }

    /// Splits instance to unit-height buckets, in the
    /// context of Corollary 15. Each bucket is indexed
    /// by the height to be given to Theorem 2.
    fn make_buckets(self, epsilon: f64) -> HashMap<ByteSteps, Instance> {
        let mut res = HashMap::new();
        let mut prev_floor = 1.0 / epsilon;
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
