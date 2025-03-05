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

    /// Base address of the instance
    #[arg(short, long, value_parser = clap::value_parser!(ByteSteps))]
    start:          ByteSteps,

    /// The only tested case of setting this to false is when reading
    /// "In" placements derived from "InEx" inputs. Be careful!
    #[arg(short, long, value_parser = clap::value_parser!(bool))]
    generations:    bool,
}

fn main() {
    let cli = Arg::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "File does not exist");

    let set: PlacedJobSet = match cli.input_format {
        InpuType::PLC   => {
            read_placed_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>(input_path, 0, cli.generations).unwrap()
        },
        InpuType::InExCSV   => {
            assert!(cli.start == 0, "minimalloc is always assumed to assign zero as base offset");
            read_placed_from_path::<IREECSVParser, Rc<PlacedJob>>(input_path, 1, cli.generations).unwrap()
        },
        InpuType::InCSV => {
            read_placed_from_path::<IREECSVParser, Rc<PlacedJob>>(input_path, 2, cli.generations).unwrap()
        },
        InpuType::ExCSV => {
            read_placed_from_path::<MinimalloCSVParser, &[ByteSteps; 4]>(input_path, 2, cli.generations).unwrap()
        },
        InpuType::TRC   => {
            panic!("TRC not supported");
        }
    };
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
