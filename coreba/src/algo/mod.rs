pub mod boxing;
pub mod placement;

use placement::do_best_fit;

use crate::{
    helpe::*,
    analyze::{
        prelude_analysis,
        placement_is_valid,
    }
};
use self::boxing::{
    c_15,
    rogue,
};

/// Assigns proper offsets to each buffer in `JobSet`,
/// so that the resulting memory fragmentation is at
/// most (`worst_case_frag` - 1.0) * 100.0 percent.
/// Address space is assumed to start at `start_address`.
/// All offsets are relative to that one.
/// 
/// `idealloc` is, in its non-trivial case, probabilistic.
/// It tries different placements again and again in a loop
/// and picks the best one. This constant controls the maximum
/// number of iterations allowed to `idealloc` to outperform its
/// last best placement. The *total* number of iterations is
/// thus stochastic.
/// 
/// Returns the placement itself, and the corresponding
/// makespan. If worst-case-fragmentation was exceeded,
/// the immediately next best achieved placement is returned.
pub fn idealloc(
    original_input:     JobSet,
    worst_case_frag:    f64,
    start_address:      ByteSteps,
    max_lives:          u32,
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
                        // Don't forget alignment!
                        placed.offset.set(placed.get_corrected_offset(start_address, 0));

                        Rc::new(placed)
                    })
                    .collect()
            )
        },
        AnalysisResult::SameSizes(jobs, ig, reg) => {
            // Overlapping jobs all sharing the same size can
            // be optimally placed with interval graph coloring.
            //
            // The resulting makespan equals their max load.
            let l = get_load(&jobs);
            let row_size = jobs[0].size;
            let mut loose: LoosePlacement = BinaryHeap::new();
            for (row_idx, igc_row) in interval_graph_coloring(jobs).into_iter()
                                                                    .enumerate() {
                for j in igc_row {
                    let semi_placed = reg.get(&j.id).unwrap();
                    semi_placed.offset.set(row_idx * row_size);
                    loose.push(semi_placed.clone());
                }
            }

            (
                l, 
                do_best_fit(loose, &ig, 0, ByteSteps::MAX, false, start_address), 
                reg.into_values()
                    .collect()
            )
        },
        AnalysisResult::NeedsBA(BACtrl {
            input,
            mut pre_boxed,
            to_box,
            epsilon,
            real_load,
            dummy,
            ig,
            reg,
            mu_lim,
            mut best_opt,
            hardness,
        }) => {
            let heuristic_opt = best_opt;
            // Initializations...
            let mut lives_left = max_lives;
            let mut total_iters = 1;
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
                let current_opt = boxed.place(&ig_reg, total_iters, best_opt, dumb_id, start_address);
                debug_assert!(current_opt == ByteSteps::MAX || current_opt >= real_load, "Bad placement");
                if current_opt < best_opt {
                    debug_assert!(placement_is_valid(&ig_reg));
                    best_opt = current_opt;
                    lives_left = max_lives + 1;
                }
                total_iters += 1;
                lives_left -= 1;
                if lives_left > 0 && best_opt > target_opt {
                    pre_boxed = rogue(input.clone(), epsilon);
                } else { break; }
            };

            println!(
                "\nHeights hardness:\t{:.2}%\nConflicts hardness:\t{:.2}%\nLives hardness:\t\t{:.2}%\nHardness product:\t{:.2}%\n{:.2}% less fragmentation against heuristic.\n",
                hardness.0 * 100.0,
                hardness.1 * 100.0,
                hardness.2 * 100.0,
                hardness.0 * hardness.1 * hardness.2 * 100.0,
                (heuristic_opt - best_opt) as f64 / real_load as f64 * 100.0
            );

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

    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", 
        best_opt, 
        target_load, 
        (best_opt - target_load) as f64 / target_load as f64 * 100.0
    );

    (placement, best_opt)
}