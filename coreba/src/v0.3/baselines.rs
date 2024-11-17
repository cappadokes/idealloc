use crate::{
    algo::placement::do_best_fit, analyze::placement_is_valid, helpe::*
};

/// Orders jobs by size or area and collects them
/// in a best/first-fit compatible binary heap.
pub fn make_baseline(
    jobs:       JobSet,
    by_size:    bool,
    first_fit:  bool,
) -> ByteSteps {
    // We will need an interference graph for the best-fit
    // operation.
    let mut ig: InterferenceGraph = HashMap::new();
    let mut registry: PlacedJobRegistry = HashMap::new();
    let mut live: PlacedJobRegistry = HashMap::new();
    let mut evts = get_events(&jobs);
    while let Some(e) = evts.pop() {
        match e.evt_t {
            EventKind::Birth    => {
                let init_vec: PlacedJobSet = live.values()
                    .cloned()
                    .collect();
                let new_entry = Rc::new(PlacedJob::new(e.job.clone()));
                ig.insert(e.job.id, init_vec);
                registry.insert(e.job.id, new_entry.clone());
                for (_, j) in &live {
                    let vec_handle = ig.get_mut(&j.descr.id).unwrap();
                    vec_handle.push(new_entry.clone());
                }
                live.insert(e.job.id, new_entry);
            },
            EventKind::Death    => {
                live.remove(&e.job.id);
            }
        }
    }
    let sort_fn = |a: &&Rc<PlacedJob>, b: &&Rc<PlacedJob>| {
        if by_size {
            b.descr
                .size
                .cmp(&a.descr.size)
        } else {
            b.descr
                .area()
                .cmp(&a.descr.area())
        }
    };
    let ordered: PlacedJobSet = registry.values() 
        .sorted_by(|a, b| sort_fn(a, b).then(a.descr.id.cmp(&b.descr.id)))
        .cloned()
        .collect();
    let mut symbolic_offset = 0;
    for pj in &ordered {
        pj.offset.set(symbolic_offset);
        symbolic_offset += 1;
    }

    let res = do_best_fit(
        ordered.into_iter()
        .collect(),
        &ig,
        // A large enough value.
        u32::MAX - 1,
        ByteSteps::MAX,
        first_fit,
        0,
    );
    assert!(placement_is_valid(&(ig, registry)));

    res
}