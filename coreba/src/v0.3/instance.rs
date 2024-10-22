use crate::utils::*;

/// Stores useful information about an [Instance].
#[derive(Clone, Copy)]
pub struct Info {
    // **CAUTION:** we mean the MAXIMUM load!
    load:           Option<ByteSteps>,
    min_max_height: Option<(ByteSteps, ByteSteps)>,
}

impl Info {
    fn merge(this: &mut Instance, that: &mut Instance) -> Self {
        let mut res = Self {
            load:           None,
            min_max_height: None,
        };

        let (this_min, this_max) = this.min_max_height();
        let (that_min, that_max) = that.min_max_height();
        res.min_max_height = Some((this_min.min(that_min), this_max.max(that_max)));

        res
    }
}

impl Instance {
    /// Creates a new [Instance] from a [JobSet].
    pub fn new(jobs: JobSet) -> Self {
        Self {
            jobs: Arc::new(jobs),
            // We will compute the info later, on
            // a need-to basis.
            info: Info {
                load: None,
                min_max_height: None,
            },
        }
    }

    /// Returns (smallest birth, largest death).
    pub fn get_horizon(&self) -> (ByteSteps, ByteSteps) {
        (
            // Assuming that the jobs are sorted, smallest
            // birth is always at the first spot.
            self.jobs
                .first()
                .unwrap()
                .birth,
            self.jobs
                .iter()
                .map(|j| { j.death })
                .max()
                .unwrap()
        )
    }

    /// Calculates the makespan.
    pub fn opt(&self) -> ByteSteps {
        unimplemented!()
    }

    /// Calculates the optimal makespan,
    /// that is, the max load of an instance.
    pub fn load(&mut self) -> ByteSteps {
        match self.info.load {
            // Value has possibly been memoized, since
            // its calculation requires traversal and is
            // thus considered expensive.
            Some(v) => v,
            None => {
                let v = get_load(&self.jobs);
                self.info.load = Some(v);

                v
            }
        }
    }

    /// Runs in case a better solution has been found and
    /// updates input job offsets accordingly.
    pub fn update_offsets(&self) {
        unimplemented!()
        //self.jobs.iter().for_each(|j| j.upd_off());
    }

    /// Returns the minimum and maximum TRUE height over the
    /// instance's jobs.
    pub fn min_max_height(&mut self) -> (ByteSteps, ByteSteps) {
        match self.info.min_max_height {
            Some(v) => v,
            None => {
                let res = self.jobs.iter().fold(
                    (ByteSteps::MAX, ByteSteps::MIN),
                    |(mut min, mut max), j| {
                        let curr = j.size;
                        if curr < min {
                            min = curr;
                        }
                        if curr > max {
                            max = curr;
                        }

                        (min, max)
                    },
                );
                self.info.min_max_height = Some(res);

                res
            }
        }
    }

    /// Splits an [Instance] into two new instances, the first
    /// containing jobs of TRUE size up to `ceil`.
    pub fn split_by_height(self, ceil: ByteSteps) -> (Self, Self) {
        let (small, high): (JobSet, JobSet) = match Arc::try_unwrap(self.jobs) {
            Ok(v) => {
                // If the `Arc` can be unwrapped, we save one
                // round of atomic ref count updates.
                v.into_iter()
                    .partition(|j| j.size <= ceil)
            },
            Err(v)    => {
                v.iter()
                .cloned()
                .partition(|j| j.size <= ceil)
            }
        };

        (Self::new(small), Self::new(high))
    }

    /// Splits an [Instance] into multiple new instances, the first
    /// containing jobs that are live in at least one moment of those
    /// in `pts`.
    pub fn split_by_liveness(self, pts: &BTreeSet<ByteSteps>) -> (JobSet, HashMap<ByteSteps, Instance>) {
        let mut x_is_base: HashMap<ByteSteps, Vec<Arc<Job>>> = HashMap::new();
        let mut live = vec![];
        let mut dealt_with = 0;

        let mut pts_iter = pts.iter()
            .map(|x| *x)
            .enumerate()
            .peekable();
        let mut jobs_iter = self.jobs.iter().peekable();

        'points: loop {
            // Assumption: no remaining, i.e., non-dealt-with Job
            // is born before t_q.
            let (q, t_q) = pts_iter.next().unwrap();
            match pts_iter.peek() {
                Some((q_next, t_q_next))    => {
                    if *q_next == pts.len() - 1 {
                        // We are at the last segment. Everything is a X_i.
                        while let Some(j) = jobs_iter.next() {
                            x_is_base.entry(q)
                                .and_modify(|v| v.push(j.clone()))
                                .or_insert(vec![j.clone()]);
                            dealt_with += 1;
                        }
                        break;
                    } else {
                        // We will deal with as many jobs as we can without breaking
                        // our assumption. Then we'll move on to the next t_q.
                        loop {
                            if let Some(j) = jobs_iter.peek() {
                                if j.lives_within(&(t_q, *t_q_next)) {
                                    let j = jobs_iter.next().unwrap();
                                    x_is_base.entry(q)
                                        .and_modify(|v| v.push(j.clone()))
                                        .or_insert(vec![j.clone()]);
                                    dealt_with += 1;
                                } else if j.is_live_at(*t_q_next) {
                                    let j = jobs_iter.next().unwrap();
                                    live.push(j.clone());
                                    dealt_with += 1;
                                } else {
                                    continue 'points;
                                }
                            } else { break 'points; }
                        }
                    }
                },
                None    => { break; }
            }
        };
        assert!(dealt_with == self.jobs.len(), "Bad liveness splitting!");

        (
            live,
            x_is_base.into_iter()
                .map(|(k, v)| { (k, Self::new(v)) })
                .collect()
        )
    }

    /// Counts how many of the *ORIGINAL* buffers have
    /// been boxed somewhere into the instance.
    pub fn total_originals_boxed(&self) -> u32 {
        self.jobs.iter().fold(0, |sum, j| sum + j.originals_boxed)
    }

    /// Merges `self` with another [Instance].
    pub fn merge_with(mut self, mut other: Self) -> Self {
        let all: Vec<Arc<Job>> = match Arc::try_unwrap(self.jobs) {
            Ok(v) => {
                v.into_iter()
                    .chain(other.jobs
                        .iter()
                        .cloned()
                    ).sorted_unstable()
                    .collect()
            },
            Err(arc)    => {
                arc.iter()
                .chain(other.jobs.iter())
                .cloned()
                .sorted_unstable()
                .collect()
            }
        };
        self.jobs = Arc::new(all);
        self.info = Info::merge(&mut self, &mut other);

        self
    }

    /// Does the same as [`Instance::merge_with`], but without consuming
    /// `self`. Used in the context of consolidating `Mutex`-protected results.
    pub fn merge_via_ref(&mut self, mut other: Self) {
        let all: Vec<Arc<Job>> = self.jobs
            .iter()
            .chain(other.jobs.iter())
            .cloned()
            .sorted_unstable()
            .collect();
        self.jobs = Arc::new(all);
        self.info = Info::merge(self, &mut other);
    }
}
