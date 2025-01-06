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

const MAX_LIVES: u32 = 11;

fn main() {
    let set = read_from_path::<PLCParser, &[u8; 8 * PLC_FIELDS_NUM]>("tests/data/ldb0.plc").unwrap();
    coreba::algo::idealloc(set, MAX_FRAG, 0, MAX_LIVES);
}