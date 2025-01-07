use coreba::*;

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
            read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>(input_path)
        },
        InpuType::InCSV => {
            read_from_path::<IREECSVParser, Job>(input_path)
        },
        InpuType::PLC   => {
            read_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>(input_path)
        }
    }.unwrap();
    let total = Instant::now();
    let registry = set.iter()
        .cloned()
        .map(|j| (j.get_id(), Rc::new(PlacedJob::new(j))))
        .collect::<PlacedJobRegistry>();
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
        JobOrdering::Random => {
            registry.values()
                .cloned()
                .permutations(set.len())
                .take(1)
                .next()
                .unwrap()
        }
    };

    let mut symbolic_offset = 0;
    for pj in &ordered {
        pj.offset.set(symbolic_offset);
        symbolic_offset += 1;
    }
    let load = get_load(&set);
    let makespan = do_naive_fit(
        ordered.into_iter().collect(),
        cli.fit,
        cli.start,
    );

    println!(
        "Total allocation time: {:.2} seconds",
        total.elapsed().as_secs_f64()
    );
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", 
        makespan, 
        load, 
        (makespan - load) as f64 / load as f64 * 100.0
    );
}

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