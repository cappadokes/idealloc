use std::rc::Rc;

use crate::{ByteSteps, Job, JobSet};
use itertools::Itertools;

impl JobSet {
    /// Initializes a JobSet with a given set of jobs.
    /// 
    /// > ***ATTENTION:*** it's the *developer's* responsibility to
    /// > guarantee for the validity of the `is_sorted` argument.
    fn init(in_elts: Vec<Job>, is_sorted: bool) -> Self {
        Self (
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
        )
    }

    /// Computes the maximum CURRENT height over the JobSet.
    pub fn min_max_height(&self) -> (ByteSteps, ByteSteps) {
        self.0
            .iter()
            .fold((ByteSteps::MAX, ByteSteps::MIN), |(mut min, mut max), j| {
                let curr = j.size.get();
                if curr < min { min = curr; }
                if curr > max { max = curr; }

                (min, max)
            })
    }
}