use coreba::*;

const MAX_FRAG: f64 = 1.0;

enum Parser {
    Minimalloc,
    IREE,
}

fn get_crate_root() -> Result<PathBuf, std::env::VarError> {
    Ok(PathBuf::from(std::env::var("CARGO_MANIFEST_DIR")?))
}

fn read_from_path(p: &str, ptype: Parser) -> Result<JobSet, Box<dyn std::error::Error>> {
    let mut csv_path = get_crate_root()?;
    csv_path.push(p);
    if let Parser::Minimalloc = ptype {
        let parser = MinimalloCSVParser::new(csv_path);
        let jobs = parser.read_jobs()?;
        assert!(jobs.len() > 0);
        let set = coreba::jobset::init(jobs)?;

        Ok(set)
    } else {
        let parser = IREECSVParser::new(csv_path);
        let jobs = parser.read_jobs()?;
        assert!(jobs.len() > 0);
        let set = coreba::jobset::init(jobs)?;

        Ok(set)
    }
}

#[test]
fn run_iree() {
    let set = read_from_path("tests/data/iree_first.csv", Parser::IREE).unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, 1);
}

/*
#[test]
fn run_tiny() {
    let set = read_from_path("tests/data/tiny_bert.csv", Parser::Minimalloc).unwrap();
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

#[test]
fn beat_heuristic_minimalloc_k() {
    let set = read_from_path("tests/data/K.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_j() {
    let set = read_from_path("tests/data/J.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_a() {
    let set = read_from_path("tests/data/A.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_b() {
    let set = read_from_path("tests/data/B.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_c() {
    let set = read_from_path("tests/data/C.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_d() {
    let set = read_from_path("tests/data/D.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_e() {
    let set = read_from_path("tests/data/E.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_f() {
    let set = read_from_path("tests/data/F.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_g() {
    let set = read_from_path("tests/data/G.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_h() {
    let set = read_from_path("tests/data/H.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_minimalloc_i() {
    let set = read_from_path("tests/data/I.1048576.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_tiny() {
    let set = read_from_path("tests/data/tiny_bert.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_resnet() {
    let set = read_from_path("tests/data/resnet50.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_pangu_small() {
    let set = read_from_path("tests/data/pangu_13B.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}

#[test]
fn beat_heuristic_pangu() {
    let set = read_from_path("tests/data/pangu_2.6B.csv").unwrap();
    let heur = baselines::make_baseline(set.clone(), true, false);
    let load = get_load(&set) as f64;
    let coreba = coreba::algo::main_loop(set, MAX_FRAG, 0);
    assert!(coreba.1 <= heur, "{}% worse", (coreba.1 - heur) as f64 / load * 100.0);
    println!("BETTER by {}%", (heur - coreba.1) as f64 / load * 100.0);
}
*/