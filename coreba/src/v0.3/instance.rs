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
            jobs: Rc::new(jobs),
            // We will compute the info later, on
            // a need-to basis.
            info: Info {
                load: None,
                min_max_height: None,
            },
        }
    }

    /// Applies Buchsbaum et al.'s Corollary 17.
    /// As far as floating point values go, we adopt
    /// 32bit-wide ones all across the project.
    pub fn init_e(&mut self) -> f32 {
        (self.min_max_height().1 as f32 / self.load() as f32).powf(1.0 / 7.0)
    }

    /// Calculates the makespan.
    pub fn opt(&mut self) -> ByteSteps {
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
                unimplemented!()
            }
        }
    }

    /// Runs in case a better solution has been found and
    /// updates input job offsets accordingly.
    pub fn update_offsets(&self) {
        self.jobs.iter().for_each(|j| j.upd_off());
    }

    /// Returns the minimum and maximum CURRENT height over the
    /// instance's jobs.
    pub fn min_max_height(&mut self) -> (ByteSteps, ByteSteps) {
        match self.info.min_max_height {
            Some(v) => v,
            None => {
                let res = self.jobs.iter().fold(
                    (ByteSteps::MAX, ByteSteps::MIN),
                    |(mut min, mut max), j| {
                        let curr = j.size.get();
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
    /// containing jobs of CURRENT size up to `ceil`.
    pub fn split_by_height(self, ceil: ByteSteps) -> (Self, Self) {
        let (small, high) = self
            .jobs
            .iter()
            .cloned()
            .partition(|j| j.size.get() <= ceil);

        // TODO: assert that the two collections preserve sorting!
        (Self::new(small), Self::new(high))
    }

    /// Counts how many of the *ORIGINAL* buffers have
    /// been boxed somewhere into the instance.
    pub fn total_originals_boxed(&self) -> u32 {
        self.jobs.iter().fold(0, |sum, j| sum + j.originals_boxed)
    }

    /// Merges `self` with another [Instance].
    pub fn merge_with(&mut self, mut other: Self) {
        let all: Vec<Rc<Job>> = self
            .jobs
            .iter()
            .chain(other.jobs.iter())
            .cloned()
            .sorted_unstable()
            .collect();
        self.jobs = Rc::new(all);
        self.info.load = None;
        let (this_min, this_max) = self.min_max_height();
        let (that_min, that_max) = other.min_max_height();
        self.info.min_max_height = Some((this_min.min(that_min), this_max.max(that_max)));
    }

    /// Restores current sizes to the original ones.
    pub fn restore_heights(&self) {
        if self.jobs.iter().any(|j| {
            // No reason to overwrite everything if
            // it's already good.
            j.size.get() != j.home.get().alloc_size
        }) {
            self.jobs.iter().for_each(|j| {
                j.size.set(j.home.get().alloc_size);
            });
        }
    }

    /// Changes current sizes to `h`.
    pub fn change_current_heights(&self, h: ByteSteps) {
        self.jobs.iter().for_each(|j| {
            j.size.set(h);
        });
    }

    /// Changes INITIAL sizes to `h`, and sets current
    /// size accordingly.
    pub fn change_init_heights(&self, h: ByteSteps) {
        self.jobs.iter().for_each(|j| {
            let mut new_home = j.home.get();
            new_home.alloc_size = h;
            j.home.replace(new_home);
            j.size.set(h);
        });
    }
}
