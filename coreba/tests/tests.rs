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

// All of the following tests ensure that the code written up to now
// is problematic only in the sense of it being incomplete. The existing
// functionality is correct.
//
// To be replaced in the future with one big test.
#[test]
#[should_panic(expected = "not implemented")]
fn run_tiny() {
    let set = read_from_path("tests/data/tiny_bert.csv").unwrap();
    coreba::algo::main_loop(set, 10);
}
/*
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
fn run_toy() {
    let set = read_from_path("tests/data/non_existent.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_pangu_small() {
    let set = read_from_path("tests/data/pangu_13B.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_bert_base() {
    let set = read_from_path("tests/data/bert_base.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_bert_large() {
    let set = read_from_path("tests/data/bert_large.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_resnet() {
    let set = read_from_path("tests/data/resnet50.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_a() {
    let set = read_from_path("tests/data/A.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}
#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_b() {
    let set = read_from_path("tests/data/B.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_c() {
    let set = read_from_path("tests/data/C.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_d() {
    let set = read_from_path("tests/data/D.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_e() {
    let set = read_from_path("tests/data/E.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_f() {
    let set = read_from_path("tests/data/F.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_g() {
    let set = read_from_path("tests/data/G.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_h() {
    let set = read_from_path("tests/data/H.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_i() {
    let set = read_from_path("tests/data/I.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_j() {
    let set = read_from_path("tests/data/J.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}

#[test]
#[should_panic(expected = "not implemented")]
fn run_minimalloc_k() {
    let set = read_from_path("tests/data/K.1048576.csv").unwrap();
    coreba::algo::main_loop(set, 1);
}
*/