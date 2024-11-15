use coreba::*;

const MAX_FRAG: f64 = 1.1;

fn get_crate_root() -> Result<PathBuf, std::env::VarError> {
    Ok(PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?))
}

fn read_from_path(p: &str) -> Result<JobSet, Box<dyn std::error::Error>> {
    let mut csv_path = get_crate_root()?;
    csv_path.push(p);
    let parser = MinimalloCSVParser::new(csv_path);
    let jobs = parser.read_jobs()?;
    assert!(jobs.len() > 0);
    let set = coreba::jobset::init(jobs)?;

    Ok(set)
}

// All of the following tests ensure that the code written up to now
// is problematic only in the sense of it being incomplete. The existing
// functionality is correct.
//
// To be replaced in the future with one big test.
/*
#[test]
fn run_baseline() {
    let set = read_from_path("tests/data/I.1048576.csv").unwrap();
    let total_start = Instant::now();
    let mut input = Instance::new(set);
    let target_load = input.load();

    let (ig, reg) = input.build_interference_graph();
    // SIZE, FIRST
    let makespan = coreba::baselines::make_baseline(&reg, true, false, u32::MAX, &ig);
    
    println!(
        "Total allocation time: {} μs",
        total_start.elapsed().as_micros()
    );
    println!("Makespan:\t{} bytes\nLOAD:\t\t{} bytes\nFragmentation:\t {:.2}%", makespan, target_load, (makespan - target_load) as f64 / target_load as f64 * 100.0);
}
*/
#[test]

fn run_tiny() {
    let set = read_from_path("tests/data/tiny_bert.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_i() {
    let set = read_from_path("tests/data/I.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_pangu() {
    let set = read_from_path("tests/data/pangu_2.6B.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_pangu_small() {
    let set = read_from_path("tests/data/pangu_13B.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_resnet() {
    let set = read_from_path("tests/data/resnet50.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_a() {
    let set = read_from_path("tests/data/A.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}
#[test]

fn run_minimalloc_b() {
    let set = read_from_path("tests/data/B.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_c() {
    let set = read_from_path("tests/data/C.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_d() {
    let set = read_from_path("tests/data/D.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_e() {
    let set = read_from_path("tests/data/E.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_f() {
    let set = read_from_path("tests/data/F.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_g() {
    let set = read_from_path("tests/data/G.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_h() {
    let set = read_from_path("tests/data/H.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_j() {
    let set = read_from_path("tests/data/J.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}

#[test]

fn run_minimalloc_k() {
    let set = read_from_path("tests/data/K.1048576.csv").unwrap();
    coreba::algo::main_loop(set, MAX_FRAG, 0);
}