use std::rc::Rc;

use crate::{Job, JobSet};
use itertools::Itertools;

/// Initializes a JobSet with a given set of jobs.
/// 
/// > ***ATTENTION:*** it's the *developer's* responsibility to
/// > guarantee for the validity of the `is_sorted` argument.
fn init(in_elts: Vec<Job>, is_sorted: bool) -> JobSet {
    if is_sorted {
        in_elts.into_iter()
            .map(|x| { Rc::new(x) })
            .collect()
    } else {
        in_elts.into_iter()
        // Unstable sorting is faster, as long as one doesn't
        // care about tie-breaks (and we don't).
        .sorted_unstable()
        .map(|x| { Rc::new(x) })
        .collect()
    }
}