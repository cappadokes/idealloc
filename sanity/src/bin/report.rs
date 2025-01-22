use sanity::*;
use coreba::*;

/// An utility for analyzing solved
/// dynamic storage allocation instances.
#[derive(Parser, Debug)]
struct Arg {
    /// Path to input (either PLC, or minimalloc-produced CSV)
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:  PathBuf,

    /// Base address of the instance
    #[arg(short, long, value_parser = clap::value_parser!(ByteSteps))]
    start:  ByteSteps,
}

fn main() {
    let cli = Arg::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "File does not exist");

    let set: PlacedJobSet = if input_path.extension().unwrap() == "plc" {
        read_placed_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>(input_path, 0).unwrap()
    } else if input_path.extension().unwrap() == "csv" {
        assert!(cli.start == 0, "minimalloc is always assumed to assign zero as base offset");
        read_placed_from_path::<IREECSVParser, Rc<PlacedJob>>(input_path, 1).unwrap()
    } else { panic!("Only file types accepted are (i) idealloc-produced PLCs and (ii) minimalloc-produced CSVs"); };
    let makespan = set.iter()
        .map(|pj| pj.next_avail_offset())
        .max()
        .unwrap();
    let load = get_load(
        &set
            .iter()
            .map(|pj| pj.descr.clone())
            .collect()
    );
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", 
        makespan, 
        load, 
        (makespan - load) as f64 / load as f64 * 100.0
    );
    //plot::plot_mappings(&the_truth, target);
}
