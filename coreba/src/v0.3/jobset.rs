use crate::utils::*;

/// Initializes a JobSet with a given set of jobs.
/// A successfully returned JobSet is guaranteed to be
/// compliant with all of `idealloc`'s assumptions. These are:
/// - no job has zero size
/// - all current sizes are in agreement with the "allocated" ones
/// - all deaths are bigger than all births
/// - all lifetimes have a length of at least 2 units
/// - all jobs are original
/// - no job has zero alignment
/// - allocated job size is equal or greater to the requested one
/// - job is original
///
/// This function is the gatekeeper to the rest of the library.
pub fn init(mut in_elts: Vec<Job>) -> Result<JobSet, JobError> {
    for (idx, j) in in_elts.iter_mut().enumerate() {
        if j.size == 0 {
            return Err(JobError {
                message: String::from("Job with 0 size found!"),
                culprit: in_elts.remove(idx),
            });
        } else if j.size != j.req_size {
            return Err(JobError {
                message: String::from("Job with disagreeing req/alloc size found!"),
                culprit: in_elts.remove(idx),
            });
        } else if j.birth >= j.death {
            return Err(JobError {
                message: String::from("Job with birth >= death found!"),
                culprit: in_elts.remove(idx),
            });
        } else if (j.death - j.birth) < 2 {
            return Err(JobError {
                message: String::from("Job with lifetime < 2 found!"),
                culprit: in_elts.remove(idx),
            });
        } else if let Some(a) = j.alignment {
            if a == 0 {
                return Err(JobError {
                    message: String::from("Job with 0 alignment found!"),
                    culprit: in_elts.remove(idx),
                });
            }
        } else if !j.is_original() {
            return Err(JobError {
                message: String::from("Unoriginal job found! (non-empty contents)"),
                culprit: in_elts.remove(idx),
            });
        } else if j.originals_boxed != 0 {
            return Err(JobError {
                message: String::from("Unoriginal job found! (non-zero originals_boxed)"),
                culprit: in_elts.remove(idx),
            });
        } else if j.size < j.req_size {
            return Err(JobError {
                message: String::from("Job with req > alloc size found!"),
                culprit: in_elts.remove(idx),
            });
        }
    }

    Ok(in_elts
        .into_iter()
        // Unstable sorting is faster, as long as one doesn't
        // care about tie-breaks (and we don't).
        //
        // There is no way to check whether the input vector is
        // sorted without having to sort it one more time. We *could*
        // rely on the user to provide an additional `is_sorted`
        // parameter, *or* we could wait until `Vec`'s [`is_sorted`](https://doc.rust-lang.org/std/vec/struct.Vec.html#method.is_sorted)
        // method gets stabilized.
        .sorted_unstable()
        .map(|x| Arc::new(x))
        .collect())
}

/// Forms Theorem 2's R_i groups. 
pub fn split_ris(jobs: JobSet, pts: &[ByteSteps]) -> Vec<JobSet> {
    let mut res = vec![];
    // The algorithm recursively splits around (q/2).ceil(), where
    // q = pts.len() - 2. The minimum value for the ceiling function
    // is 1. Thus the length of the points must be at least 3.
    if pts.len() >= 3 {
        let q = pts.len() - 2;
        let idx_mid = (q as f32 / 2.0).ceil() as ByteSteps;
        // The fact that we need to index within `pts` is why we're
        // passing a slice instead of the original `BTreeSet`.
        let t_mid = pts[idx_mid];
        let mut live_at: Vec<Arc<Job>> = vec![];
        let mut die_before: Vec<Arc<Job>> = vec![];
        let mut born_after: Vec<Arc<Job>> = vec![];
        for j in jobs {
            if j.is_live_at(t_mid) { live_at.push(j); }
            else if j.dies_before(t_mid) { die_before.push(j); }
            else if j.born_after(t_mid) { born_after.push(j); }
            else { panic!("Unreachable!"); }
        }
        res.push(live_at);
        if !die_before.is_empty() {
            res.append(
                &mut split_ris(
                    die_before,
                    &pts[..idx_mid]
                )
            );
        };
        if !born_after.is_empty() {
            res.append(
                &mut split_ris(
                    born_after,
                    &pts[idx_mid + 1..]
                )
            );
        }
    } else {
        panic!("Unreachable");
    }

    res
}

#[derive(PartialEq, Eq)]
/// An [Event] is either a birth or a death.
pub enum EventKind {
    Birth,
    Death,
}

#[derive(Eq)]
pub struct Event {
    pub job:    Arc<Job>,
    pub evt_t:  EventKind,
    // Copy time here to elude pattern matching during
    // comparison.
    pub time:   ByteSteps,
}

/// Traversal of a [JobSet] can be thought as an ordered stream
/// of events, with increasing time of occurence. Each [Job] generates
/// two events, corresponding to the start/end of its lifetime
/// respectively.
/// 
/// We use these events to calculate things such as maximum load,
/// interference graphs, fragmentation, critical points, etc.
type Events = BinaryHeap<Event>;

impl Ord for Event {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We're using a BinaryHeap, which is
        // a max-priority queue. We want a min-one
        // and so we're reversing the order of `cmp`.
        if self.time != other.time {
            other.time.cmp(&self.time)
        } else {
            if self.evt_t == other.evt_t {
                other.time.cmp(&self.time)
            } else {
                match self.evt_t {
                    EventKind::Birth    => { std::cmp::Ordering::Less },
                    EventKind::Death    => { std::cmp::Ordering::Greater },
                }
            }
        }
    }
}
impl PartialOrd for Event {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for Event {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time
    }
}

pub fn get_events(jobs: &JobSet) -> Events {
    let mut res = BinaryHeap::new();
    for j in jobs {
        res.push(Event {
            job:    j.clone(),
            evt_t:  EventKind::Birth,
            time:   j.birth,
        });
        res.push(Event {
            job:    j.clone(),
            evt_t:  EventKind::Death,
            time:   j.death,
        });
    };

    res
}