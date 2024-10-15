use crate::utils::*;

/// Executes the core `idealloc` loop for a `max_iters`
/// number of times. At the end the jobs in `input` have
/// received placement offsets.
pub fn main_loop(input: JobSet, max_iters: u32) {
    let mut input = Instance::new(input);
    // We time the different phases, both for debugging
    // and because we find it interesting.
    use std::time::Instant;
    let total_start;
    let mut start = Instant::now();

    // First thing to do is stabilize epsilon.
    // As a by-product, ε-initialization fleshes out the
    // input's info (max load, min/max height).
    let mut epsilon = Epsilon::new(&mut input);
    let boxes = loop {
        match t_16(input.clone(), epsilon.val) {
            Ok(b) => {
                // We reach this point only when all
                // original jobs have been boxed.
                println!("ε-convergence: {}", start.elapsed().as_micros());
                input.restore_heights();
                break b;
            }
            Err(jobs_boxed) => {
                epsilon.update(jobs_boxed);
                input.restore_heights();
            }
        }
    };
    total_start = start;
    start = Instant::now();

    // Produce initial set of offsets. They are inscribed to
    // the jobs of the input, which are still available.
    boxes.unbox();
    println!("1 unboxing iteration: {} μs", start.elapsed().as_micros());
    start = Instant::now();
    input.tighten();
    println!("1 tightening iteration: {} μs", start.elapsed().as_micros());

    let mut iters_done = 1;
    let mut best_opt = input.opt();
    // Early stop in case of stumbling on perfect solution
    while iters_done < max_iters && best_opt > input.load() {
        t_16(input.clone(), epsilon.val)
            // No danger of panicking in here given
            // that ε has been stabilized.
            .unwrap()
            .unbox();
        input.restore_heights();
        input.tighten();
        let current_opt = input.opt();
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
fn t_16(mut input: Instance, epsilon: f32) -> Result<Instance, u32> {
    match t_16_cond(&mut input, epsilon) {
        (true, _, _)    => {
            let h_max = input.min_max_height().1 as f32;
            c_15(
                input,
                (h_max / epsilon).ceil() as ByteSteps,
                epsilon,
            )
        },
        (false, mu, h)  => {
            let target_size = (mu * (h as f32)).ceil() as usize;
            if target_size < input.min_max_height().0 || target_size == 1 {
                Err(input.total_originals_boxed())
            } else {
                let (x_s, mut x_l) = input.split_by_height(target_size);
                let small_boxed = c_15(x_s, h, mu)?;
                // TODO: demystify old impl's check for jobs_boxed.
                x_l.merge_with(small_boxed);
                t_16(x_l, epsilon)
            }
        }
    }
}

/// Checks if Theorem 16 has converged.
fn t_16_cond(input: &mut Instance, epsilon: f32) -> (bool, f32, ByteSteps) {
    let (h_min, h_max) = input.min_max_height();
    let r = h_max as f32 / h_min as f32;

    let lg2r = r.log2().powi(2);
    let mu = epsilon / lg2r;

    (
        lg2r < 1.0 / epsilon,
        mu,
        (mu.powi(5) * (h_max as f32) / lg2r).ceil() as ByteSteps,
    )
}

fn c_15(
    input:      Instance,
    h:          ByteSteps,
    epsilon:    f32,
) -> Result<Instance, u32> {
    use rayon::prelude::*;
    use std::sync::Mutex;

    let res = Arc::new(Mutex::new(Instance::new(vec![])));
    let buckets = input.make_buckets(epsilon);
    //let mut v: HashMap<usize, usize> = HashMap::new();
    buckets.into_par_iter()
        .for_each(|(h_i, unit_jobs)| {
        let h_param = 1.max((h as f32 / h_i as f32).floor() as ByteSteps);
        match t_2(unit_jobs, h_param, epsilon, None) {
            Ok(boxed)   => {
                // TODO: how many hierarchy levels are present after T2?
                // betalloc assumes just one, because (as below) it changes
                // only the heights of the OUTER boxes. What's the truth?
                let mut guard = res.lock().unwrap();                
                guard.merge_with(boxed);
            },
            Err(_j)  => {
                unimplemented!();
            }
        }
    });
    /*
        boxed.change_init_heights(h);
        res.merge_with(boxed);
    }
    */
    match Arc::into_inner(res) {
        Some(v) => {
            Ok(v.into_inner().unwrap())
        },
        None    => {
            // This shouldn't happen because all threads
            // should have finished by now, and hence `res`
            // should only have one strong reference.
            panic!("Could not unwrap Arc!");
        }
    }
}

fn t_2(
    mut input:  Instance,
    h:          ByteSteps,
    epsilon:    f32,
    ctrl:       Option<()>,
) -> Result<Instance, u32> {
    unimplemented!()
}

// The following operations are considered
// parts of the algorithm, so they're put here instead of
// the file hosting the rest of the impls.
impl Instance {
    /// Recursively unboxes and places elements.
    fn unbox(self) {
        unimplemented!()
    }

    /// Tightens already placed elements.
    fn tighten(&self) {
        unimplemented!()
    }

    /// Splits instance to unit-height buckets, in the
    /// context of Corollary 15. Each bucket is indexed
    /// by the height to be given to Theorem 2.
    fn make_buckets(self, epsilon: f32) -> HashMap<ByteSteps, Instance> {
        let mut res = HashMap::new();
        let mut i = 1;
        let mut source = self;
        while source.jobs.len() > 0 {
            let h = (1.0 + epsilon).powi(i).floor() as ByteSteps;
            if source.jobs.iter().any(|j| j.size <= h) {
                let (toward_bucket, rem) = source.split_by_height(h);
                toward_bucket.change_current_heights(1);
                res.insert(h, toward_bucket);
                source = rem;
            }
            i += 1;
        }

        res
    }
}
