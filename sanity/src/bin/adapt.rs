use std::env;
use sanity::{BufReader, File, Path, Request, SimWorld};

fn main() {
    for a in env::args().skip(1) {
        let (target, is_csv) = parse_input(&a);
        let f = BufReader::new(File::open(target).unwrap());
        let mut world = SimWorld::new(f);
        if is_csv {
            while let Some(_) = Request::new_minimalloc_event(&mut world) {};
        } else {
            while let Some(_) = Request::new_trc_event(&mut world) {};
            world.retire_remnants();
        }
        println!("{} objects recorded.", world.objects_num);
        world.give_back(target);
    }
}

// Checks if:
//  i)      input exists
//  ii)     input is file
//  iii)    input is CSV OR TRC
//  
// Returns true if file is CSV.
// False for TRC.
// Panics otherwise.
fn parse_input(arg: &str) -> (&Path, bool) {
    let mut is_csv = false;
    let target = Path::new(arg);
    assert!(target.exists(), "Target doesn't exist.");
    assert!(target.is_file(), "Target ain't file.");
    let suffix = String::from(target.extension()
        .unwrap()
        .to_str()
        .unwrap());
    if suffix.contains("csv") {
        is_csv = true;
    } else if !suffix.contains("trc") {
        panic!("Only CSV, TRC files acceptable.")
    }

    (target, is_csv)
}