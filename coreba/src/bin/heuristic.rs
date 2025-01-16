use std::usize;

use algo::placement::do_best_fit;
use coreba::*;
use rand::prelude::*;

/// A heuristics generator for dynamic storage allocation
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to input
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:  PathBuf,

    /// Input format
    #[arg(value_enum)]
    format: InpuType,

    /// Start address
    #[arg(short, long, default_value_t = 0)]
    #[arg(value_parser = clap::value_parser!(ByteSteps))]
    start:  ByteSteps,

    /// Job ordering
    #[arg(value_enum)]
    order:  JobOrdering,

    /// Job fitting
    #[arg(value_enum)]
    fit:    JobFit,

    /// Whether to use an interference graph
    /// or not (speeds up things).
    #[arg(short, long, default_value_t = false)]
    #[arg(value_parser = clap::value_parser!(bool))]
    graph:  bool,

    /// Number of lives in the case of random ordering.
    #[arg(short, long, default_value_t = 1)]
    #[arg(value_parser = clap::value_parser!(ByteSteps))]
    lives:  ByteSteps,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum JobOrdering {
    /// Sort by ascending birth
    Birth,
    /// Sort by decreasing size
    Size,
    /// Sort by decreasing area (lifetime-size product)
    Area,
    /// A random permutation
    Random,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum JobFit {
    /// Best fit
    Best,
    /// First fit
    First,
}

fn main() {
    let cli = Args::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "Invalid input path");
    let set = match cli.format {
        InpuType::ExCSV => {
            read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>(input_path, 1)
        },
        InpuType::InExCSV => {
            read_from_path::<IREECSVParser, Job>(input_path, 1)
        },
        InpuType::InCSV => {
            read_from_path::<IREECSVParser, Job>(input_path, 2)
        },
        InpuType::PLC   => {
            read_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>(input_path, 0)
        },
        InpuType::TRC   => {
            panic!("TRC files must first pass through the adapter.");
        }
    }.unwrap(); 
    let load = get_load(&set);
    let (ig, registry): (Option<InterferenceGraph>, PlacedJobRegistry) = if cli.graph {
        let mut registry: PlacedJobRegistry = HashMap::new();
        let mut events = get_events(&set);
        let mut res: InterferenceGraph = HashMap::new();
        let mut live: PlacedJobRegistry = HashMap::new();
        while let Some(e) = events.pop() {
            match e.evt_t {
                EventKind::Birth    => {
                    let init_vec: PlacedJobSet = live.values()
                        .cloned()
                        .collect();
                    let new_entry = Rc::new(PlacedJob::new(e.job.clone()));
                    // First, add a new entry, initialized to the currently live jobs.
                    res.insert(e.job.id, init_vec);
                    registry.insert(e.job.id, new_entry.clone());
                    for (_, j) in &live {
                        // Update currently live jobs' vectors with the new entry.
                        let vec_handle = res.get_mut(&j.descr.id).unwrap();
                        vec_handle.push(new_entry.clone());
                    }
                    // Add new entry to currently live jobs.
                    live.insert(e.job.id, new_entry);
                },
                EventKind::Death    => {
                    assert!(live.remove(&e.job.id).is_some());
                },
            }
        }

        (Some(res), registry)
    } else { (None, set.iter()
        .cloned()
        .map(|j| (j.get_id(), Rc::new(PlacedJob::new(j))))
        .collect::<PlacedJobRegistry>()) };
    let total = Instant::now();
    let mut lives_left = cli.lives;
    let mut best_makespan = usize::MAX;
    let makespan = match cli.order {
        JobOrdering::Random => {
            let mut shuffled_ids: Vec<u32> = registry.values().map(|pj| pj.descr.id).collect();
            let mut rng = rand::thread_rng();
            let mut iters = 0;
            loop {
                shuffled_ids.shuffle(&mut rng);
                let ordered = shuffled_ids.iter().map(|id| registry.get(id).unwrap().clone()).collect();
                let test_makespan = gen_placement(ordered, &ig, cli.fit, cli.start, best_makespan, iters);
                if test_makespan == load { break test_makespan; }
                if test_makespan < best_makespan {
                    best_makespan = test_makespan;
                }
                lives_left -= 1;
                if lives_left > 0 { 
                    iters += 1;
                    continue; 
                }
                break best_makespan;
            }
        },
        _   => {
            let ordered: PlacedJobSet = match cli.order {
                JobOrdering::Birth  => {
                    registry.values()
                        .sorted_by(|a, b| a.descr.birth.cmp(&b.descr.birth))
                        .cloned()
                        .collect()
                },
                JobOrdering::Area   => {
                    registry.values()
                        .sorted_by(|a, b| b.descr.area().cmp(&a.descr.area()))
                        .cloned()
                        .collect()
                },
                JobOrdering::Size   => {
                    registry.values()
                        .sorted_by(|a, b| b.descr.size.cmp(&a.descr.size))
                        .cloned()
                        .collect()
                },
                JobOrdering::Random => { panic!("Unreachable branch reached."); }
            };
            gen_placement(ordered, &ig, cli.fit, cli.start, usize::MAX, 0)
        },
    };

    println!(
        "Total allocation time: {} μs",
        total.elapsed().as_micros()
    );
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", 
        makespan, 
        load, 
        (makespan - load) as f64 / load as f64 * 100.0
    );
}

fn gen_placement(
    ordered:    PlacedJobSet,
    ig:         &Option<InterferenceGraph>,
    fit:        JobFit,
    start:      ByteSteps,
    makesp_lim: ByteSteps,
    iters:      u32,
) -> ByteSteps {
    let mut symbolic_offset = 0;
    for pj in ordered.iter() {
        pj.offset.set(symbolic_offset);
        symbolic_offset += 1;
    }
    if let Some(ref g) = ig {
        let fit = if let JobFit::Best = fit { false } else { true };
        do_best_fit(
            ordered.into_iter().collect(),
            g,
            iters, 
            makesp_lim, 
            fit,
            start)
    } else {
        do_naive_fit(
            ordered.into_iter().collect(),
            fit,
            start) 
    }
}

/// A fitting function which does not use an
/// interference graph or early stopping.
fn do_naive_fit(
    mut loose:  LoosePlacement,
    fit:        JobFit,
    start_addr: ByteSteps
) -> ByteSteps {
    let mut max_address = 0;
    let mut squeezed: PlacedJobSet = vec![];
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
        let mut jobs_vec = squeezed.iter()
            .filter(|j| { j.overlaps_with(&to_squeeze) })
            .sorted_unstable()
            .rev()
            .peekable();

        while let Some(next_job) = jobs_vec.peek() {
            let njo = next_job.offset.get();
            if njo > offset_runner {
                let test_off = to_squeeze.get_corrected_offset(start_addr, offset_runner);
                if njo > test_off && njo - test_off >= min_gap_size {
                    if let JobFit::Best = fit {
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
        let cand_makespan = to_squeeze.next_avail_offset();
        if cand_makespan > max_address {
            max_address = cand_makespan;
        }
        squeezed.push(to_squeeze);
    };

    max_address
}
