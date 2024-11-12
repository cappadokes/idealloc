pub mod boxing;
pub mod placement;

use crate::{
    helpe::*,
    analyze::prelude_analysis,
};
use self::boxing::{
    c_15,
    rogue,
};

/// `idealloc` is, in its non-trivial case, probabilistic.
/// It tries different placements again and again in a loop
/// and picks the best one. This constant controls the
/// maximum allowable number of iterations.
/// 
/// To be replaced later with a more sophisticated value.
pub const MAX_ITERS: u32 = 100;

/// Assigns proper offsets to each buffer in `JobSet`,
/// so that the resulting memory fragmentation is at
/// most (`worst_case_frag` - 1.0) * 100.0 percent.
/// 
/// Returns the placement itself, and the corresponding
/// makespan. If worst-case-fragmentation was exceeded,
/// the immediately next best achieved placement is returned.
pub fn main_loop(
    original_input:     JobSet,
    worst_case_frag:    f64,
) -> (PlacedJobSet, ByteSteps) {
    // Measure total allocation time.
    let total_start = Instant::now();

    // There are some trivial cases in which the heavy-lifting
    // of the core algorithm is unnecessary. We conduct an analysis
    // first to see if any of said cases hold. Along the way we
    // set up the context of the aforementioned heavy lifting, so
    // as to avoid repeating computations if it ends up being needed.
    let (target_load, best_opt, placement) = match prelude_analysis(original_input) {
        AnalysisResult::NoOverlap(jobs) => {
            // Non-overlapping jobs can all be put in the
            // same offset.
            (
                get_load(&jobs), 
                get_max_size(&jobs),
                jobs.into_iter()
                    .map(|j| {
                        let placed = PlacedJob::new(j);
                        placed.offset.set(0);

                        Rc::new(placed)
                    })
                    .collect()
            )
        },
        AnalysisResult::SameSizes(jobs) => {
            // Overlapping jobs all sharing the same size can
            // be optimally placed with interval graph coloring.
            //
            // The resulting makespan equals their max load.
            let l = get_load(&jobs);
            let row_size = jobs[0].size;
            let mut res = vec![];
            for (row_idx, igc_row) in interval_graph_coloring(jobs).into_iter()
                                                                    .enumerate() {
                res.append(
                    &mut igc_row.into_iter()
                        .map(|j| {
                            let placed = PlacedJob::new(j);
                            placed.offset.set(row_idx * row_size);

                            Rc::new(placed)
                        })
                        .collect()
                );
            }

            (l, l, res)
        },
        AnalysisResult::NeedsBA(BACtrl {
            mut input,
            mut pre_boxed,
            to_box,
            epsilon,
            real_load,
            dummy,
            ig,
            reg,
            mu_lim
        }) => {
            // Initializations...
            let mut iters_done = 0;
            let mut best_opt = ByteSteps::MAX;
            let target_opt = (real_load as f64 * worst_case_frag).floor() as ByteSteps;
            let dumb_id = if let Some(ref dum) = dummy {
                dum.id
            } else {
                // Guaranteed never to be encountered, unless if
                // u32::MAX / 2 - jobs_num_to_box boxes are made.
                u32::MAX / 2 + 1
            };
            let ig_reg = (ig, reg);

            // Initializations related to the last
            // invocation of C15.
            let (_, mut mu, _, _) = pre_boxed.get_safety_info(epsilon);
            if mu > mu_lim {
                mu = 0.99 * mu_lim;
            }
            let (_h_min, h_max) = input.min_max_height();
            let final_h = h_max as f64 / mu;

            loop {
                let boxed = c_15(pre_boxed.clone(), final_h, mu);
                debug_assert!(boxed.check_boxed_originals(to_box as u32), "Invalid boxing!");
                let current_opt = boxed.place(&ig_reg, iters_done, best_opt, dumb_id);
                debug_assert!(current_opt == ByteSteps::MAX || current_opt >= real_load, "Bad placement");
                if current_opt < best_opt {
                    best_opt = current_opt;
                }
                iters_done += 1;
                if iters_done < MAX_ITERS && best_opt > target_opt {
                    pre_boxed = rogue(input.clone(), epsilon);
                } else { break; }
            };

            (
                real_load,
                best_opt,
                ig_reg.1
                    .into_values()
                    .collect()
            )
        }
    };

    println!(
        "Total allocation time: {} μs",
        total_start.elapsed().as_micros()
    );

    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", best_opt, target_load, (best_opt - target_load) as f64 / target_load as f64 * 100.0);

    (placement, best_opt)
}