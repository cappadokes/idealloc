use coreba::*;

/// A golden standard for dynamic storage allocation
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to input
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:      PathBuf,

    /// Input format
    #[arg(value_enum)]
    format:     InpuType,

    /// Maximum fragmentation allowed (e.g., 1.05 allows <= 5% memory waste)
    #[arg(short = 'f', long, default_value_t = 1.0)]
    #[arg(value_parser = clap::value_parser!(f64))]
    max_frag:   f64,

    /// Start address
    #[arg(short, long, default_value_t = 0)]
    #[arg(value_parser = clap::value_parser!(ByteSteps))]
    start:      ByteSteps,

    /// Maximum number of tries allowed to beat bootstrap heuristic
    #[arg(short = 'l', long, default_value_t = 1)]
    #[arg(value_parser = clap::value_parser!(u32))]
    max_lives:  u32
}

fn main() {
    let cli = Args::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "Invalid input path");
    assert!(cli.max_frag >= 1.0, "Maximum fragmentation must be at least 1.0");
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
        InpuType::TRC   => { panic!("TRC files must first be fed to the `adapt` binary!"); },
    }.unwrap();
    coreba::algo::idealloc(set, cli.max_frag, cli.start, cli.max_lives);
}