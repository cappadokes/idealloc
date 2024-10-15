use coreba::utils::*;

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

#[test]
#[should_panic(expected = "not implemented")]
fn run_pangu() {
    let set = read_from_path("tests/data/pangu_2.6B.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_nezha() {
    let set = read_from_path("tests/data/bert_nezha.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_tiny() {
    let set = read_from_path("tests/data/tiny_bert.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_toy() {
    let set = read_from_path("tests/data/non_existent.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}