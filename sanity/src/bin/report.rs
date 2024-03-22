use sanity::*;

fn main() {
    for a in env::args().skip(1) {
        let target = Path::new(&a);
        assert!(target.is_file());
        let f = BufReader::new(File::open(target).unwrap());
        let mut world = SimWorld::new(f);
        while let Some(_) = Request::new_from_plc(&mut world) {};

        let mut the_truth = TDBP::new(world);
        the_truth.init_mappings();
        the_truth.measure_frag();
        the_truth.get_acc();
        the_truth.report();
        plot::plot_mappings(&the_truth, target);
    }
}