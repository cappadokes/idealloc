use sanity::{BufReader, File};
use serde::{Deserialize, Serialize};
use std::io::{Write, BufWriter};
use coreba::*;

/// A tool for creating DSA instances from the
/// released JSON files of the ASPLOS/EuroSys 2025
/// contest track.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to input
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    input:  PathBuf,

    /// Path to output
    #[arg(short, long, value_parser = clap::value_parser!(PathBuf))]
    output:  PathBuf,

    /// Policy for selecting usage size per node
    #[arg(value_enum)]
    policy: UsagePolicy         
}

/// The contest JSON files represent some multi-graphs, the
/// description of which is irrelevant for our discussion.
/// Each graph node spawns a Job, in idealloc lingo. Job lifetimes
/// are fixed, but many candidate job sizes exist. This enum
/// encodes the user's desire as per which size to pick.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum UsagePolicy {
    /// Pick a random usage size.
    Random,
    /// Pick the biggest usage size.
    Biggest,
    /// Pick the smallest usage size.
    Smallest,
}

/// JSON contents will be read into this.
#[derive(Serialize, Deserialize)]
struct ContestJSON {
    problem:    Problem,
}

#[derive(Serialize, Deserialize)]
struct Problem {
    name:           String,
    nodes:          NoDesc,
    edges:          EdgeDesc,
    usage_limit:    usize,
}

#[derive(Serialize, Deserialize)]
struct NoDesc {
    intervals:  Vec<(usize, usize)>,
    costs:      Vec<Vec<usize>>,
    usages:     Vec<Vec<usize>>,
}

#[derive(Serialize, Deserialize)]
struct EdgeDesc {
    nodes:      Vec<(usize, usize)>,
    costs:      Vec<Vec<usize>>,
}

struct IOPDDLParser {
    json_path:  PathBuf,
    size_pol:   UsagePolicy,
}

impl JobGen<Job> for IOPDDLParser {
    fn new(json_path: PathBuf) -> Self {
        Self { 
            json_path,
            size_pol:   UsagePolicy::Random,
        }
    }
    fn read_jobs(&self, _shift: ByteSteps) -> Result<Vec<Job>, Box<dyn std::error::Error>> {
        let path = self.json_path.as_path();
        match std::fs::metadata(path) {
            Ok(_)   => {
                let fd = std::fs::File::open(path)?;
                let reader = BufReader::new(fd);
                let prob = serde_json::from_reader::<BufReader<File>, ContestJSON>(reader)?;
                let mut jobset: JobSet = vec![];
                let node_info = prob.problem.nodes;
                let mut next_id = 0;
                for (interval, sizes) in node_info.intervals
                    .iter()
                    .zip(node_info.usages.iter()) {
                        let size = match self.size_pol {
                            UsagePolicy::Biggest    => { *sizes.iter().max().unwrap() + 1 },
                            UsagePolicy::Smallest   => { *sizes.iter().min().unwrap() + 1 },
                            UsagePolicy::Random     => {
                                use rand::{Rng, thread_rng};
                                sizes[
                                    thread_rng().gen_range(0..sizes.len())
                                ] + 1
                            }
                        };
                        let baby = Job {
                            // These intervals are wrong! Target JSONs
                            // have InExCSV semantics!
                            birth:          interval.0,
                            size,
                            death:          interval.1 + 1,
                            req_size:       size,
                            alignment:      None,
                            contents:       None,
                            originals_boxed:0,
                            id:             next_id,
                        };
                        next_id += 1;
                        jobset.push(Arc::new(baby));
                }

                // Sorry for the messy code. Adopted from `adapt`.
                let mut evts = get_events(&jobset);
                let mut num_generations = 0;
                let mut last_was_birth = true;
                let mut retired: Vec<Job> = vec![];
                let mut to_retire: HashMap<u32, Job> = HashMap::new();
                while let Some(e) = evts.pop() {
                    match e.evt_t {
                        EventKind::Birth    => {
                            let (birth, death) = (
                                e.job.birth + num_generations,
                                e.job.death + num_generations + 1
                            );
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
                        },
                    }
                }

                Ok(retired)
            },
            Err(e)  => { Err(Box::new(e)) }
        }
    }
    fn gen_single(&self, d: Job, _id: u32) -> Job {
        d        
    }
}

fn main() {
    let cli = Args::parse();
    let input_path = cli.input;
    // What follows is an almost-copy of coreba::read_from_path,
    // which can't be used due to the additional "policy" enum.
    let mut parser = IOPDDLParser::new(input_path);
    parser.size_pol = cli.policy;
    let jobs = parser.read_jobs(62).unwrap();
    match jobset::init(jobs) {
        Ok(set) => {
            let fd = File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(cli.output)
                .unwrap();
            let mut writer = BufWriter::new(fd);
            writeln!(&mut writer, "id,lower,upper,size").unwrap();
            for j in set {
                let id = j.id;
                let birth = j.birth;
                let death = j.death;
                let size = j.size;
                writeln!(&mut writer, "{id},{birth},{death},{size}")
                    .unwrap();
            }
        },
        Err(e)  => { panic!("{:?}", e); }
    };
}