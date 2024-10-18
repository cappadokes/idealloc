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
        // TODO: Time unboxing/tightening withing placement func
        // if you're still interested!
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
                (h_max / epsilon).ceil() as ByteSteps,
                epsilon,
            )
        },
        (false, mu, h)  => {
            let target_size = (mu * (h as f64)).floor() as usize;
            assert!(target_size > input.min_max_height().0, "ε-convergence can't be avoided after all.");
            let (x_s, x_l) = input.split_by_height(target_size);
            let small_boxed = c_15(x_s, h, mu);
            // TODO: demystify old impl's check for jobs_boxed.
            t_16(x_l.merge_with(small_boxed), epsilon)
        }
    }
}

/// Checks if Theorem 16 has converged.
fn t_16_cond(input: &mut Instance, epsilon: f64) -> (bool, f64, ByteSteps) {
    let (h_min, h_max) = input.min_max_height();
    let r = h_max as f64 / h_min as f64;

    let lg2r = r.log2().powi(2);
    let mu = epsilon / lg2r;

    (
        lg2r < 1.0 / epsilon,
        mu,
        (mu.powi(5) * (h_max as f64) / lg2r).ceil() as ByteSteps,
    )
}

fn c_15(
    input:      Instance,
    h:          ByteSteps,
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
            let h_param = (h as f32 / h_i as f32).floor() as ByteSteps;
            // The below assertion was mostly used out of fear. To be
            // reconsidered if the rest of the pipeline proves broken.
            //assert!(h_param > 1, "T2 fed with unit H!");
            let boxed = t_2(unit_jobs, h_param, epsilon, None);
            // TODO: how many hierarchy levels are present after T2?
            // betalloc assumes just one, because (as below) it changes
            // only the heights of the OUTER boxes. What's the truth?
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
        let mut critical_points = BTreeSet::new();
        critical_points.insert(start);
        assert!(critical_points.insert(end), "Same-ends horizon met.");
        critical_points.insert(Self::gen_crit(jobs, start, end));

        Self {
            bounding_interval:  (start, end),
            critical_points
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
        for evt in get_events(&jobs.jobs) {
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
        pts.remove(thread_rng().gen_range(0..pts.len()))
    }
}

/// Buchsbaum's Theorem 2.
fn t_2(
    mut input:  Instance,
    h:          ByteSteps,
    epsilon:    f64,
    ctrl:       Option<T2Control>,
) -> Instance {
    let mut res = Instance::new(vec![]);
    let mut all_unresolved = Instance::new(vec![]);

    // This is a recursive function.
    let ctrl = if let Some(v) = ctrl { v }
    else { T2Control::new(&input) };

    let (r_coarse, x_is) = input.split_by_liveness(&ctrl.critical_points);

    unimplemented!()
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
        let mut prev_floor = 1.0;
        let mut i = 1;
        let mut source = self;
        while source.jobs.len() > 0 {
            let h = (1.0 + epsilon).powi(i);
            if source.jobs.iter().any(|j| j.size as f64 > prev_floor && j.size as f64 <= h) {
                let h = h.floor() as ByteSteps;
                let (toward_bucket, rem) = source.split_by_height(h);
                res.insert(h, toward_bucket);
                source = rem;
            }
            prev_floor = h;
            i += 1;
        }

        res
    }
}
