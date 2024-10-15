use coreba::utils::*;

fn get_crate_root() -> Result<PathBuf, std::env::VarError> {
    Ok(PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?))
}

fn read_pangu() -> Result<JobSet, Box<dyn std::error::Error>> {
    let mut csv_path = get_crate_root()?;
    csv_path.push("tests/data/pangu_2.6B.csv");
    //csv_path.push("tests/data/tiny_bert.csv");
    //csv_path.push("tests/data/bert_nezha.csv");
    //csv_path.push("tests/data/non_existent.csv");
    let parser = MinimalloCSVParser::new(csv_path);
    let jobs = parser.read_jobs()?;
    assert!(jobs.len() > 0);
    let set = coreba::jobset::init(jobs)?;

    Ok(set)
}

#[test]
#[should_panic]
fn kick_loop() {
    let set = read_pangu().unwrap();
    coreba::algo::main_loop(set, 1);
}