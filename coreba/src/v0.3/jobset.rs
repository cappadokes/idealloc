use crate::utils::*;

/// Initializes a JobSet with a given set of jobs.
/// A successfully returned JobSet is guaranteed to be
/// compliant with all of `idealloc`'s assumptions. These are:
/// - no job has zero size
/// - all current sizes are in agreement with the "allocated" ones
/// - all deaths are bigger than all births
/// - all lifetimes have a length of at least 1 unit
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
        } else if j.lifetime() < 1 {
            return Err(JobError {
                message: String::from("Job with lifetime < 1 found!"),
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

pub fn get_max_size(jobs: &JobSet) -> ByteSteps {
    jobs.iter()
        .map(|j| j.size)
        .max()
        .unwrap()
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
/// is an IGC row.
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
                        debug_assert!(row_to_fill == res.len(), "Bad IGC impl!");
                        res.push(vec![evt.job]);
                    }
                };
                if free_rows.is_empty() {
                    // No free space! Add one more row to the top.
                    free_rows.insert(max_row + 1); 
                    max_row += 1;
                }
            },
            EventKind::Death    => {
                let row_to_vacate = cheatsheet.remove(&evt.job.id).unwrap();
                free_rows.insert(row_to_vacate);
            }
        }
    };

    res
}

#[derive(PartialEq, Eq, Clone)]
/// An [Event] is either a birth or a death.
pub enum EventKind {
    Birth,
    Death,
}

#[derive(Eq, Clone)]
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
pub type Events = BinaryHeap<Event>;

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
    ig:                 &PlacedJobRegistry,
    dumb_id:            u32,
) -> LoosePlacement {
    let mut res = BinaryHeap::new();
    match control_state {
        UnboxCtrl::SameSizes(row_height)    => {
            // If jobs are same-sized, do IGC!
            // The jobs in each row will be non-overlapping.
            jobs.sort_unstable();
            for row in interval_graph_coloring(jobs) {
                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                start_offset += row_height;
            }
        },
        UnboxCtrl::NonOverlapping   => {
            // If jobs are non-overlapping, they can all be put
            // at the same offset.
            for j in jobs {
                if j.is_original() {
                    if j.id != dumb_id {
                        let to_put = ig.get(&j.id).unwrap().clone();
                        to_put.offset.set(start_offset);
                        res.push(to_put.clone());
                    }
                } else {
                    res.append(&mut get_loose_placement(Arc::unwrap_or_clone(j).contents.unwrap(), start_offset, UnboxCtrl::Unknown, ig, dumb_id));
                }
            }
        },
        UnboxCtrl::Unknown  => {
            // We must find out on our own the jobs' characteristics.
            // First check if they're all of the same size.
            let size_probe = jobs[0].size;
            if jobs.iter()
                .skip(1)
                .all(|j| { j.size == size_probe }) {
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::SameSizes(size_probe), ig, dumb_id));
            } else {
                // Then check if they're non-overlapping. We can do that
                // by demanding that the corresponding events are always
                // alternating between births and deaths.
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
                    res.append(&mut get_loose_placement(jobs, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                } else {
                    // Here we know for a fact that the jobs are of multiple sizes, and they're also
                    // overlapping. One idea is to use "big rocks first". This can be combined with
                    // clustering (maybe more than one jobs are of the same size and can thus be
                    // put in the same cluster).
                    let mut size_buckets: HashMap<ByteSteps, JobSet> = HashMap::new();
                    for j in jobs {
                        size_buckets.entry(j.size)
                            .and_modify(|e| e.push(j.clone()))
                            .or_insert(vec![j]);
                    }
                    for (row_height, mut size_class) in size_buckets.into_iter()
                        .sorted_unstable_by(|a, b| { b.0.cmp(&a.0)}) {
                            size_class.sort_unstable();
                            let igc_rows = interval_graph_coloring(size_class);
                            let num_rows = igc_rows.len();
                            for row in igc_rows {
                                res.append(&mut get_loose_placement(row, start_offset, UnboxCtrl::NonOverlapping, ig, dumb_id));
                                start_offset += row_height * num_rows;
                            }
                    }
                }
            }
        }
    };

    res
}