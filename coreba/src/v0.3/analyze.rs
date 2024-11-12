use crate::{
    helpe::*,
    algo::boxing::rogue,
};

/// Realizes if:
/// 
/// (i)     there's any overlap between jobs. If none exists, they can all
///         get the same offset.
/// 
/// (ii)    job sizes are uniform. If that is the case, an optimal solution
///         can be found via IGC. This check runs only if overlap is found
///         in the context of (i).
/// 
/// If none of (i), (ii) have concluded, the stage is set for BA's heavy
/// lifting: a dummy [Job] is possibly inserted to ensure convergence,
/// the [InterferenceGraph] is built, max load is computed.
pub fn prelude_analysis(mut jobs: JobSet) -> AnalysisResult {
    // For detecting overlap.
    let mut last_evt_was_birth = false;
    let mut overlap_exists = false;
    // For calculating max load.
    let (mut running_load, mut max_load): (ByteSteps, ByteSteps) = (0, 0);
    // For detecting size uniformity.
    let mut sizes: HashSet<ByteSteps> = HashSet::new();
    // For building interference graph.
    let mut ig: InterferenceGraph = HashMap::new();
    let mut registry: PlacedJobRegistry = HashMap::new();
    let mut live: PlacedJobRegistry = HashMap::new();
    // For configuring BA in case it needs to run.
    let (mut h_min, mut h_max) = (ByteSteps::MAX, 0);
    let mut max_death = 0;
    let mut max_id = 0;

    let mut evts = get_events(&jobs);
    while let Some(e) = evts.pop() {
        match e.evt_t {
            EventKind::Birth    => {
                if e.job.size < h_min {
                    h_min = e.job.size;
                }
                if e.job.size > h_max {
                    h_max = e.job.size;
                }
                if e.job.id > max_id { max_id = e.job.id; }
                //---START MAX LOAD UPDATE---
                running_load += e.job.size;
                if running_load > max_load {
                    max_load = running_load;
                }
                //---END MAX LOAD UPDATE---

                sizes.insert(e.job.size);

                //---START IG BUILDING---
                let init_vec: PlacedJobSet = live.values()
                    .cloned()
                    .collect();
                let new_entry = Rc::new(PlacedJob::new(e.job.clone()));
                // First, add a new entry, initialized to the currently live jobs.
                ig.insert(e.job.id, init_vec);
                registry.insert(e.job.id, new_entry.clone());
                for (_, j) in &live {
                    // Update currently live jobs' vectors with the new entry.
                    let vec_handle = ig.get_mut(&j.descr.id).unwrap();
                    vec_handle.push(new_entry.clone());
                }
                // Add new entry to currently live jobs.
                live.insert(e.job.id, new_entry);
                //---END IG BUILDING---

                if last_evt_was_birth && !overlap_exists {
                    // Overlap detected!
                    overlap_exists = true;
                    if sizes.len() == 1 {
                        let size_probe = sizes.take(&e.job.size).unwrap();
                        if jobs.iter()
                            .all(|j| { j.size == size_probe }) {
                            return AnalysisResult::SameSizes(jobs);
                        }
                    }
                }
                last_evt_was_birth = true;
            },
            EventKind::Death    => {
                //---START MAX LOAD UPDATE
                if let Some(v) = running_load.checked_sub(e.job.size) {
                    running_load = v;
                } else {
                    panic!("Almost overflowed load!");
                }
                //---END MAX LOAD UPDATE
                if !overlap_exists { last_evt_was_birth = false; }
                live.remove(&e.job.id);
                if e.job.death > max_death { max_death = e.job.death; }
            },
        }
    };

    if !overlap_exists {
        // TODO: the above loop is TOO heavy an overhead
        // in case no actual overlap exists. We've currently
        // decided to do everything in one pass, but maybe
        // that should change in the future.
        AnalysisResult::NoOverlap(jobs)
    } else {
        // Interference graph has been built, max load has been computed.
        // BA needs to run, so we must compute epsilon, initialize rogue, etc.
        //
        // First thing to do is check if dummy job is needed.
        let r = h_max as f64 / h_min as f64;
        let lgr = r.log2();
        let lg2r = lgr.powi(2);
        let small_end = (lg2r.powi(7) / r).powf(1.0 / 6.0);
        let mu_lim = (5.0_f64.sqrt() - 1.0) / 2.0;
        let big_end = mu_lim * lg2r;
        let mut to_box = jobs.len();
        let mut dummy = None;
        let real_load = max_load;

        if small_end >= big_end {
            // Demanding that small < end leads to the condition:
            // r > lg2r * mu_lim.powi(-6)
            // Via WolframAlpha, an approximate solution to that
            // is any r > 2216.53...
            //
            // We thus plant such a "dummy" job in the instance.
            h_max = (2216.54_f64 * h_min as f64).ceil() as ByteSteps;
            let dummy_job = Arc::new(Job {
                size:               h_max,
                req_size:           h_max,
                birth:              0,
                death:              max_death,
                originals_boxed:    0,
                alignment:          None,
                contents:           None,
                id:                 max_id + 1,
            });
            jobs.push(dummy_job.clone());
            jobs.sort_unstable();
            to_box += 1;
            max_load += h_max;
            dummy = Some(dummy_job);
        }
        let mut instance = Instance::new(jobs);
        instance.info.set_load(max_load);
        instance.info.set_heights((h_min, h_max));
        let (_, small_end, big_end, _) = instance.ctrl_prelude();
        assert!(small_end < big_end);
        let (epsilon, pre_boxed) = init_rogue(instance.clone(), small_end, big_end);
        AnalysisResult::NeedsBA(BACtrl {
            input:      instance,
            pre_boxed,
            epsilon,
            to_box,
            real_load,
            dummy,
            ig,
            reg:        registry,
            mu_lim,
        })
    }
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