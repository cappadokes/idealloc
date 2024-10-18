use core::panic;

use rayon::vec;

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
    /// 
    /// If C17 proves invalid for the current instance,
    /// configures ε so as to ensure convergence of the main loop.
    pub fn init_e(&mut self) -> f64 {
        let (h_min, h_max) = self.min_max_height();
        let res = (h_min as f64 / self.load() as f64).powf(1.0 / 7.0);
        let r = h_max as f64 / h_min as f64;
        let lgr = r.log2();

        if lgr.powi(2) < 1.0 / res {
            res
        } else {
            let mu = res / lgr.powi(2);
            let h_cap = (mu.powi(5) * h_max as f64 / lgr.powi(2)).ceil();
            let target_size = (mu * h_cap).floor();
            // This is the condition which ensures convergence (for now...).
            if target_size > h_min as f64 && mu < 1.0 {
                res
            } else {
                // Watch out for overflow.
                if h_min > 3 {
                    assert!(((lgr.powi(2) * (h_min - 3) as f64) / h_max as f64) < 1.0, "No solution exists");
                }
                let small_end = 0.0_f64.max(lgr.powi(14) * (h_min as f64 - 3.0) / h_max as f64);
                let big_end = lgr.powi(12);

                // Beginning from the aforementioned condition and
                // solving for ε ends up in the inequality
                // small_end < ε^6 < big_end. 
                ((big_end - small_end) / 2.0 + small_end).powf(1.0 / 6.0)
            }
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

    /// Splits an [Instance] into multiple new instances, the first
    /// containing jobs that are live in at least one moment of those
    /// in `pts`.
    pub fn split_by_liveness(self, pts: &BTreeSet<ByteSteps>) -> (Self, HashMap<ByteSteps, Instance>) {
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
            Self::new(live),
            x_is_base.into_iter()
                .map(|(k, v)| { (k, Self::new(v)) })
                .collect()
        )
    }

    // Forms Theorem 2's R_i groups. 
    pub fn split_ris(self, pts: &[ByteSteps]) -> Vec<Instance> {
        assert!(!self.jobs.is_empty());
        let mut res = vec![];
        // The algorithm recursively splits around (q/2).ceil(), where
        // q = pts.len() - 2. The minimum value for the ceiling function
        // is 1. Thus the length of the points must be at least 3.
        if pts.len() >= 3 {
            let q = pts.len() - 2;
            let idx_mid = (q as f32 / 2.0).ceil() as ByteSteps;
            let t_mid = pts[idx_mid];
            match Arc::into_inner(self.jobs) {
                Some(v) => {
                    let mut live_at: Vec<Arc<Job>> = vec![];
                    let mut die_before: Vec<Arc<Job>> = vec![];
                    let mut born_after: Vec<Arc<Job>> = vec![];
                    for j in v {
                        if j.is_live_at(t_mid) { live_at.push(j); }
                        else if j.dies_before(t_mid) { die_before.push(j); }
                        else if j.born_after(t_mid) { born_after.push(j); }
                        else { panic!("Unreachable!"); }
                    }
                    res.push(Self::new(live_at));
                    if !die_before.is_empty() {
                        res.append(
                            &mut Self::new(die_before)
                                .split_ris(&pts[..idx_mid])
                        );
                    };
                    if !born_after.is_empty() {
                        res.append(
                            &mut Self::new(born_after)
                                .split_ris(&pts[idx_mid + 1..])
                        );
                    }
                },
                None    => { panic!("Expected singly-owned jobs vec."); }
            };
        } else {
            panic!("Unreachable");
        }

        res
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
}
