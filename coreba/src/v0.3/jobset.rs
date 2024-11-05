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

    if in_elts.is_sorted() {
        Ok(in_elts
            .into_iter()
            .map(|x| Arc::new(x))
            .collect())
    } else {
        Ok(in_elts
            .into_iter()
            .sorted_unstable()
            .map(|x| Arc::new(x))
            .collect())
    }
}

pub fn originals_contained(jobs: &JobSet) -> u32 {
    let version_1 = jobs.iter().fold(0, |count, j|  {
        if j.is_original() { count + 1 }
        else { count + j.originals_boxed }
    });

    let mut version_2 = 0;
    for j in jobs {
        if j.is_original() {
            version_2 += 1;
        } else {
            let contents_ref = j.contents.as_ref().unwrap();
            version_2 += originals_contained(contents_ref);
        }
    };

    assert!(version_1 == version_2);

    version_2
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
        res.push(jobs);
    }

    res
}

pub fn get_load(jobs: &JobSet) -> ByteSteps {
    let (mut running, mut max) = (0, 0);
    let mut evts = get_events(jobs);
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

    max
}

pub fn get_total_originals_boxed(jobs: &JobSet) -> u32 {
    jobs.iter().fold(0, |sum, j| sum + j.originals_boxed)
}

/// Self-explanatory. Each [JobSet] of the returned vector
/// is an IGC row. Rows returned are ordered by DECREASING
/// lifespan--the longest rows are put first.
pub fn interval_graph_coloring(jobs: JobSet) -> Vec<JobSet> {
    let mut res: Vec<JobSet> = vec![];
    // This is our inventory of free rows. We'll be pulling
    // space from here (lowest first), and adding higher rows
    // along the way whenever we run out.
    let mut free_rows = BTreeSet::from([0]);
    // The highest spawned row.
    let mut max_row = 0;
    // A mapping from job IDs to row nums.
    let mut cheatsheet: HashMap<u32, ByteSteps> = HashMap::new();

    // Traverse jobs...
    let mut evts = get_events(&jobs);
    while let Some(evt) = evts.pop() {
        match evt.evt_t {
            EventKind::Birth    => {
                // Get the lowest free row.
                let row_to_fill = free_rows.pop_first().unwrap();
                // Update map.
                cheatsheet.insert(evt.job.id, row_to_fill);
                match res.get_mut(row_to_fill) {
                    Some(v) => {
                        v.push(evt.job);
                    },
                    None    => {
                        assert!(row_to_fill == res.len(), "Bad IGC impl!");
                        res.push(vec![evt.job]);
                    }
                };
                if free_rows.is_empty() {
                    // No free space! Add one more row to the top.
                    assert!(free_rows.insert(max_row + 1), "Bad cheatsheet"); 
                    max_row += 1;
                }
            },
            EventKind::Death    => {
                let row_to_vacate = cheatsheet.remove(&evt.job.id).unwrap();
                assert!(free_rows.insert(row_to_vacate), "Bad bad bad");
            }
        }
    };

    /*
        Doing away with this, as it both hurts boxing time
        and doesn't make sense intuitively.

        TODO: remove the comment in the future, when 100% sure.

    // Put longest rows to the bottom.
    res.sort_unstable_by(|a, b| {
        b.iter()
            .fold(0, |acc_life, j| { acc_life + j.lifetime()} )
            .cmp(&a.iter()
                    .fold(0, |acc_life, j| { acc_life + j.lifetime()})
            )
    });
    */

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
                std::cmp::Ordering::Equal
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
    debug_assert!(jobs[..].is_sorted(), "{}", Backtrace::force_capture());
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

pub fn get_loose_placement(
    mut jobs:           JobSet,
    mut start_offset:   ByteSteps,
    control_state:      UnboxCtrl,
) -> LoosePlacement {
    let mut res = BinaryHeap::new();
    match control_state {
        UnboxCtrl::SameSizes(row_height)    => {
            jobs.sort_unstable();
            for row in interval_graph_coloring(jobs) {
                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping));
                start_offset += row_height;
            }
        },
        UnboxCtrl::NonOverlapping   => {
            let thread_safe_res: Arc<Mutex<LoosePlacement>> = Arc::new(Mutex::new(BinaryHeap::new()));
            jobs.into_par_iter().for_each(|j| {
                // First, unwrap job. There shouldn't be any other
                // references to it.
                //let j = Arc::into_inner(j).unwrap();
                if j.is_original() {
                    let mut to_put = PlacedJob::new(j);
                    to_put.offset = start_offset;
                    let mut guard = thread_safe_res.lock().unwrap();
                    guard.push(Arc::new(to_put));
                } else {
                    let mut guard = thread_safe_res.lock().unwrap();
                    guard.append(&mut get_loose_placement(Arc::unwrap_or_clone(j).contents.unwrap(), start_offset, UnboxCtrl::Unknown));
                }
            });
        },
        UnboxCtrl::Unknown  => {
            let size_probe = jobs[0].size;
            if jobs.iter()
                .skip(1)
                .all(|j| { j.size == size_probe }) {
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::SameSizes(size_probe)));
            } else {
                jobs.sort_unstable();
                let mut evts = get_events(&jobs);
                let mut last_was_birth = false;
                let mut non_overlapping = true;
                while let Some(e) = evts.pop() {
                    match e.evt_t {
                        EventKind::Birth    => {
                            if last_was_birth {
                                non_overlapping = false;
                                break;
                            }
                            last_was_birth = true;
                        },
                        EventKind::Death    => {
                            last_was_birth = false;
                        }
                    }
                }
                if non_overlapping {
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::NonOverlapping));
                } else {
                    unimplemented!();
                }
            }
        }
    };

    res
}