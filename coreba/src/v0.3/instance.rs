use crate::utils::*;

/// Stores useful information about an [Instance].
#[derive(Clone, Copy)]
pub struct Info {
    // **CAUTION:** we mean the MAXIMUM load!
    load:           Option<ByteSteps>,
    // **CAUTION:** we mean the CURRENT height!
    min_max_height: Option<(ByteSteps, ByteSteps)>,
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

    /// Applies Buchsbaum et al.'s Corollary 17.
    /// As far as floating point values go, we adopt
    /// 32bit-wide ones all across the project.
    /// 
    /// If C17 proves invalid for the current instance,
    /// configures ε so as to ensure convergence of the main loop.
    pub fn init_e(&mut self) -> f32 {
        let (h_min, h_max) = self.min_max_height();
        let test = (h_min as f32 / self.load() as f32).powf(1.0 / 7.0);
        let r = h_max as f32 / h_min as f32;

        if test.powi(5) > (r.log2().powi(12)) / r {
            // This is C17. The condition stems from
            // T16's "H" (see paper), which must be
            // bigger than the instance's minimum height.
            test
        } else {
            // This is our own hack. We take the above
            // condition and apply the FOURTH instead
            // of the fifth root.
            (r.log2().powi(12) / r).powf(1.0 / 4.0)
        }
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
                let (mut running, mut max) = (0, 0);
                let mut evts = get_events(&self.jobs);
                // The `evts` variable is a min-priority queue on the
                // births and deaths of the jobs. Deaths have priority
                // over births. By popping again and again, we have
                // our "traversal" from left to right.
                while let Some(evt) = evts.pop()  {
                    match evt.evt_t {
                        EventKind::Birth    => {
                            running += evt.job.size;
                            if running > max {
                                max = running;
                            }
                        },
                        EventKind::Death    => {
                            if let Some(v) = running.checked_sub(evt.job.size) {
                                running = v;
                            } else {
                                panic!("Almost overflowed load!");
                            }
                        }
                    }
                }
                self.info.load = Some(max);

                max
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
        let (small, high) = match Arc::try_unwrap(self.jobs) {
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

        // TODO: assert that the two collections preserve sorting!
        (Self::new(small), Self::new(high))
    }

    /// Splits an [Instance] into two new instances, the first
    /// containing jobs that are live in at least one moment of those
    /// in `pts`.
    pub fn split_by_liveness(self, pts: &BTreeSet<ByteSteps>) -> (Self, Self) {
        let (live, non_live) = match Arc::try_unwrap(self.jobs) {
            Ok(v) => {
                // If the `Arc` can be unwrapped, we save one
                // round of atomic ref count updates.
                v.into_iter()
                    .partition(|j| {
                        pts.iter()
                            .any(|t| { j.is_live_at(*t) })
                    })
            },
            Err(v)    => {
                v.iter()
                .cloned()
                .partition(|j| {
                    pts.iter()
                        .any(|t| { j.is_live_at(*t) })
                })
            }
        };

        // TODO: assert that the two collections preserve sorting!
        (Self::new(live), Self::new(non_live))
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
        self.info.load = None;
        let (this_min, this_max) = self.min_max_height();
        let (that_min, that_max) = other.min_max_height();
        self.info.min_max_height = Some((this_min.min(that_min), this_max.max(that_max)));

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
        self.info.load = None;
        let (this_min, this_max) = self.min_max_height();
        let (that_min, that_max) = other.min_max_height();
        self.info.min_max_height = Some((this_min.min(that_min), this_max.max(that_max)));
    }

    /// Restores current sizes to the original ones.
    pub fn restore_heights(&self) {
        /*
        if self.jobs.iter().any(|j| {
            // No reason to overwrite everything if
            // it's already good.
            j.size.get() != j.home.get().alloc_size
        }) {
            self.jobs.iter().for_each(|j| {
                j.size.set(j.home.get().alloc_size);
            });
        }
        */
        unimplemented!()
    }

    /// Changes current sizes to `h`.
    pub fn change_current_heights(&self, h: ByteSteps) {
        /*(
        self.jobs.iter().for_each(|j| {
            j.size.set(h);
        });
        */
        unimplemented!()
    }

    /// Changes INITIAL sizes to `h`, and sets current
    /// size accordingly.
    pub fn change_init_heights(&self, h: ByteSteps) {
        /*
        self.jobs.iter().for_each(|j| {
            let mut new_home = j.home.get();
            new_home.alloc_size = h;
            j.home.replace(new_home);
            j.size.set(h);
        });
        */
        unimplemented!()
    }
}
