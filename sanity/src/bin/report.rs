use sanity::*;
use coreba::*;

/// An utility for analyzing solved
/// dynamic storage allocation instances.
#[derive(Parser, Debug)]
struct Arg {
    /// Path to input (either PLC, or minimalloc-produced CSV)
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:          PathBuf,

    /// Input format
    #[arg(value_enum)]
    input_format:   InpuType,

    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    offsets:        PathBuf,

    /// Base address of the instance
    #[arg(short, long, value_parser = clap::value_parser!(ByteSteps))]
    start:          ByteSteps,
}

fn main() {
    let cli = Arg::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "File does not exist");
    let set: PlacedJobSet = read_placed_from_path::<MinimalloCSVParser, &[ByteSteps; 5]>(cli.offsets, 2).unwrap();
    let jobset = match cli.input_format {
        InpuType::ExCSV => {read_from_path::<MinimalloCSVParser, &[ByteSteps; 4]>(input_path.clone(),0)},
        InpuType::InCSV => {read_from_path::<IREECSVParser, Job>(input_path.clone(), 2)},
        InpuType::InExCSV   => {read_from_path::<IREECSVParser, Job>(input_path.clone(), 1)},
        _   => {unimplemented!()}
    }.unwrap();

    let mut evts = get_events(&jobset);
    let mut ig: InterferenceGraph = HashMap::new();
    let mut id_ground_truth: JobSet = read_from_path_reporter::<MinimalloCSVParser, &[ByteSteps; 4]>(input_path, 0).unwrap();
    let registry: PlacedJobRegistry = set.iter()
        .map(|pj| {
            let mut idx = 0;
            let correct_job = loop {
                let j = &id_ground_truth[idx];
                if j.birth == pj.descr.birth && j.death == pj.descr.death && j.size == pj.descr.size {
                    break id_ground_truth.remove(idx)
                }
                idx += 1;
            };
            let new_pj = Rc::new(PlacedJob::new(correct_job));
            new_pj.offset.set(pj.offset.get());
            (new_pj.descr.id, new_pj)
        })
        .collect();
    let mut live: PlacedJobRegistry = HashMap::new();

    let mut alarm_raised = false;
    while let Some(e) = evts.pop() {
        match e.evt_t {
            EventKind::Birth    => {
                let new_entry = registry.get(&e.job.id).unwrap().clone();
                //let new_entry = Rc::new(PlacedJob::new(e.job.clone()));
                //registry.insert(e.job.id, new_entry.clone());
                let init_vec: PlacedJobSet = live.values()
                    .cloned()
                    .filter(|j| new_entry.overlaps_with(&j))
                    .collect();
                let new_entry = registry.get(&e.job.id).unwrap().clone();
                // First, add a new entry, initialized to the currently live jobs.
                assert!(ig.insert(e.job.id, init_vec).is_none());
                for (_, j) in &live {
                    if e.job.overlaps_with(&j.descr) {
                        // Update currently live jobs' vectors with the new entry.
                        let vec_handle = ig.get_mut(&j.descr.id).unwrap();
                        vec_handle.push(new_entry.clone());
                    } else if !alarm_raised {
                        eprintln!("Culprit found!");
                        alarm_raised = true;
                    }
                }
                // Add new entry to currently live jobs.
                live.insert(e.job.id, new_entry);
            },
            EventKind::Death    => {
                assert!(live.remove(&e.job.id).is_some());
            }
        }        
    }

    let makespan = registry.values()
        .map(|pj| pj.next_avail_offset())
        .max()
        .unwrap();
    let load = get_load(&jobset);
    assert!(coreba::analyze::placement_is_valid(&(ig, registry)));
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", 
        makespan, 
        load, 
        (makespan - load) as f64 / load as f64 * 100.0
    );
    //plot::plot_mappings(&the_truth, target);
}

pub fn read_from_path_reporter<T, B>(file_path: PathBuf, shift: ByteSteps) -> Result<JobSet, Box<dyn std::error::Error>> 
where T: JobGen<B> {
    let parser = T::new(file_path);
    let jobs = parser.read_jobs(shift)?;
    assert!(jobs.len() > 0);

    Ok(jobs
        .into_iter()
        .map(|x| Arc::new(x))
        .collect())
}