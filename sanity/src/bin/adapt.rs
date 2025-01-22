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
            let mut evts = get_events(&input_set);
            let mut retired: Vec<Job> = vec![];
            let mut to_retire: HashMap<u32, Job> = HashMap::new();
            let mut num_generations = 0;
            let mut last_was_birth = true;
            while let Some(e) = evts.pop() {
                match e.evt_t {
                    EventKind::Birth    => {
                        let (birth, death): (ByteSteps, ByteSteps) = match cli.input_format {
                            InpuType::PLC | InpuType::ExCSV   => {
                                match cli.output_format {
                                    InpuType::InCSV                 => {
                                        let new_birth = e.job.birth.checked_sub(num_generations).unwrap();
                                        let new_death = e.job.death.checked_sub(num_generations + 2).unwrap();

                                        (new_birth, new_death)
                                    },
                                    InpuType::InExCSV               => {
                                        let new_birth = e.job.birth.checked_sub(num_generations).unwrap();
                                        let new_death = e.job.death.checked_sub(num_generations + 1).unwrap();

                                        (new_birth, new_death)
                                    },
                                    InpuType::PLC | InpuType::ExCSV => { (e.job.birth, e.job.death) },
                                    InpuType::TRC                   => { unimplemented!(); },
                                }
                            },
                            InpuType::InCSV => {
                                match cli.output_format {
                                    InpuType::InCSV                 => { (e.job.birth, e.job.death) },
                                    InpuType::InExCSV               => { (e.job.birth, e.job.death + 1) },
                                    InpuType::PLC | InpuType::ExCSV => { (e.job.birth + num_generations, e.job.death + num_generations + 2) },
                                    InpuType::TRC                   => { unimplemented!(); },
                                }
                            }
                            InpuType::InExCSV => {
                                match cli.output_format {
                                    InpuType::InCSV                 => { (e.job.birth, e.job.death - 1) },
                                    InpuType::InExCSV               => { (e.job.birth, e.job.death) },
                                    InpuType::PLC | InpuType::ExCSV => { (e.job.birth + num_generations, e.job.death + num_generations + 1) },
                                    InpuType::TRC                   => { unimplemented!(); },
                                }
                            },
                            InpuType::TRC   => { panic!("TRC not supported as output format"); }
                        };
                        let template = Job {
                            size:               e.job.size,
                            birth,
                            death,
                            req_size:           e.job.size,
                            // Opponent allocators don't support alignment.
                            // We thus eliminate it in all cases.
                            alignment:          None,
                            contents:           None,
                            originals_boxed:    0,
                            id: e.job.id,
                        };
                        to_retire.insert(template.id, template);
                        last_was_birth = true;
                    },
                    EventKind::Death    => {
                        let to_insert = to_retire.remove(&e.job.id).unwrap();
                        retired.push(to_insert);
                        if last_was_birth {
                            num_generations += 1;
                            last_was_birth = false;
                        }
                    }
                }
            }
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