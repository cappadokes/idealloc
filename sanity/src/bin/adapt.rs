use sanity::{BufReader, File, Request, SimWorld};
use std::io::{Write, BufWriter};
use coreba::*;

/// A utility adapter for transforming input formats
/// according to the target allocator.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to input
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:          PathBuf,

    /// Input format
    #[arg(value_enum)]
    input_format:   InpuType,

    /// Path to output
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    output:         PathBuf,

    /// Output format
    #[arg(value_enum)]
    output_format:  InpuType,
}

fn main() {
    let cli = Args::parse();
    let input_path = cli.input;
    assert!(input_path.exists() && input_path.is_file(), "Invalid input path");
    if cli.input_format != cli.output_format {
        if cli.input_format != InpuType::TRC {
            assert!(cli.output_format != InpuType::TRC, "TRC output not allowed.");
            let input_set = match cli.input_format {
                InpuType::PLC   => {
                    read_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>(input_path, 0)
                },
                _   => {
                    read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>(input_path, 1)
                }
            }.unwrap();
            // The jobs have been now read.
            let retired = input_set
                .iter()
                .map(|job| {
                    let (birth, death): (ByteSteps, ByteSteps) = match cli.input_format {
                        InpuType::PLC | InpuType::ExCSV   => {
                            match cli.output_format {
                                InpuType::InCSV                 => {
                                    (job.birth + 2, job.death)
                                },
                                InpuType::InExCSV               => {
                                    (job.birth + 1, job.death)
                                },
                                InpuType::PLC | InpuType::ExCSV => { (job.birth, job.death) },
                                InpuType::TRC                   => { unimplemented!(); },
                            }
                        },
                        InpuType::InCSV => {
                            match cli.output_format {
                                InpuType::InCSV                 => { (job.birth, job.death) },
                                InpuType::InExCSV               => { (job.birth, job.death + 1) },
                                InpuType::PLC | InpuType::ExCSV => { (job.birth, job.death + 2) },
                                InpuType::TRC                   => { unimplemented!(); },
                            }
                        }
                        InpuType::InExCSV => {
                            match cli.output_format {
                                InpuType::InCSV                 => { (job.birth, job.death - 1) },
                                InpuType::InExCSV               => { (job.birth, job.death) },
                                InpuType::PLC | InpuType::ExCSV => { (job.birth, job.death + 1) },
                                InpuType::TRC                   => { unimplemented!(); },
                            }
                        },
                        InpuType::TRC   => { panic!("TRC not supported as output format"); }
                    };
                    Job {
                        size:               job.size,
                        birth,
                        death,
                        req_size:           job.size,
                        // Opponent allocators don't support alignment.
                        // We thus eliminate it in all cases.
                        alignment:          None,
                        contents:           None,
                        originals_boxed:    0,
                        id:                 job.id,
                    }
                })
                .collect::<Vec<Job>>();
            // The only remaining thing is for the jobs to be written in the appropriate
            // format.
            let fd = File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(cli.output)
                .unwrap();
            let mut writer = BufWriter::new(fd);
            let mut buff: Vec<u8> = vec![];
            if let InpuType::PLC = cli.output_format {}
            else { writeln!(&mut writer, "id,lower,upper,size").unwrap(); }
            for j in retired {
                let id = j.id;
                let birth = j.birth;
                let death = j.death;
                let size = j.size;
                match cli.output_format {
                    InpuType::PLC   => {
                        let args = [
                            j.id as usize,
                            j.birth,
                            j.death,
                            j.size,
                            0,
                            0,
                            0,
                            j.size,
                        ];
                        for arg in args {
                            let arg_bytes = arg.to_be_bytes();
                            for b in arg_bytes { buff.push(b); };
                        };
                        writer.write(&buff[..]).unwrap();
                        buff.clear();
                    },
                    _               => {
                        writeln!(&mut writer, "{id},{birth},{death},{size}")
                            .unwrap();
                    },
                }
            };
        } else {
            assert!(cli.output_format == InpuType::PLC, "Only TRC conversion supported is to PLC.");
            let f = BufReader::new(File::open(input_path).unwrap());
            let mut world = SimWorld::new(f);
            while let Some(_) = Request::new_trc_event(&mut world) {}
            world.retire_remnants();
            world.give_back(cli.output);
        }
    }
}