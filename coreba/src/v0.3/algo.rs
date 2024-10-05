use crate::{
    utils::Epsilon,
    Instance
};

pub fn main_loop(
    mut input:  Instance,
    max_iters:  u32,
) {
    // We time the different phases, both for debugging
    // and because we find it interesting.
    use std::time::Instant;
    let total_start;
    let mut start = Instant::now();

    // First thing to do is stabilize epsilon.
    let mut epsilon = Epsilon::new(&mut input);
    let boxes = loop {
        match t_16(input.clone(), epsilon.val) {
            Ok(b)   => { 
                // We reach this point only when all
                // original jobs have been boxed.
                println!("ε-convergence: {}", start.elapsed().as_micros());
                break b 
            },
            Err(jobs_boxed)  => {
                epsilon.update(jobs_boxed);
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
        input.tighten();
        let current_opt = input.opt();
        if current_opt < best_opt {
            best_opt = current_opt;
            input.update_offsets();
        }
        iters_done += 1;
    }

    println!("Total allocation time: {} μs", total_start.elapsed().as_micros());
}

/// Implements Theorem 16 from Buchsbaum et al. Returns
/// a modified [`Instance`] in case of convergence, and
/// the number of original jobs that this run managed
/// to box otherwise.
fn t_16(
    input:      Instance,
    epsilon:    f32
)   -> Result<Instance, u32> {
    unimplemented!()
}

// Unboxing and tightening implementation are considered
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
}