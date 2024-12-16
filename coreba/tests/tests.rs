use coreba::*;

const MAX_FRAG: f64 = 1.0;

fn get_crate_root() -> Result<PathBuf, std::env::VarError> {
    Ok(PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?))
}

fn read_from_path<T, B>(p: &str) -> Result<JobSet, Box<dyn std::error::Error>> 
where T: JobGen<B> {
    let mut file_path = get_crate_root()?;
    file_path.push(p);
    let parser = T::new(file_path);
    let jobs = parser.read_jobs()?;
    assert!(jobs.len() > 0);
    let set = coreba::jobset::init(jobs)?;

        Ok(set)
}

const MAX_LIVES: u32 = 31;

#[test]
fn run_hello_plc() {
    let set = read_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>("tests/data/hi_again.plc").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_iree_mobilebert() {
    let set = read_from_path::<IREECSVParser, Job>("tests/data/iree_mobilebert.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_iree() {
    let set = read_from_path::<IREECSVParser, Job>("tests/data/iree_first.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_tiny() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/tiny_bert.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_i() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/I.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_pangu() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/pangu_2.6B.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_pangu_small() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/pangu_13B.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_resnet() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/resnet50.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_a() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/A.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_b() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/B.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_c() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/C.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_d() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/D.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_e() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/E.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_f() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/F.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_g() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/G.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_h() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/H.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_j() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/J.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}

#[test]
fn run_minimalloc_k() {
    let set = read_from_path::<MinimalloCSVParser, &[ByteSteps; 3]>("tests/data/K.1048576.csv").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}