use crate::{
    helpe::*,
    algo::placement::do_best_fit,
};

/// Orders jobs by size or area and collects them
/// in a best/first-fit compatible binary heap.
pub fn _make_baseline(
    jobs:       &PlacedJobRegistry,
    by_size:    bool,
    first_fit:  bool,
    dumb_id:    u32,
    ig:         &InterferenceGraph,
) -> ByteSteps {
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
    let ordered: PlacedJobSet = jobs.values()
        .filter(|j| j.descr.id != dumb_id)
        .sorted_unstable_by(sort_fn)
        .cloned()
        .collect();
    let mut symbolic_offset = 0;
    for pj in &ordered {
        pj.offset.set(symbolic_offset);
        symbolic_offset += 1;
    }

    do_best_fit(
        ordered.into_iter()
        .collect(),
        ig,
        // A large enough value.
        u32::MAX - 1,
        ByteSteps::MAX,
        first_fit
    )
}