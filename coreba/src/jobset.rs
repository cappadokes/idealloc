use crate::helpe::*;

/// Initializes a JobSet with a given set of jobs.
/// A successfully returned JobSet is guaranteed to be
/// compliant with all of `idealloc`'s assumptions. These are:
/// - no job has zero size
/// - all deaths are bigger than all births
/// - no job has bad alignment (zero, or alloc. size not multiple of i)
/// - all jobs are original
/// - allocated job size is equal or greater to the requested one
///
/// This function is the gatekeeper to the rest of the library.
pub fn init(mut in_elts: Vec<Job>) -> Result<JobSet, JobError> {
    for (idx, j) in in_elts.iter_mut().enumerate() {
        if j.size == 0 {
            return Err(JobError {
                message: String::from("Job with 0 size found!"),
                culprit: in_elts.remove(idx),
            });
        } else if j.birth >= j.death {
            return Err(JobError {
                message: String::from("Job with birth >= death found!"),
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
        .map(|x| Arc::new(x))
        .collect())
}

/// Forms Theorem 2's R_i groups. 
#[inline(always)]
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

#[inline(always)]
pub fn get_max_size(jobs: &JobSet) -> ByteSteps {
    jobs.iter()
        .map(|j| j.size)
        .max()
        .unwrap()
}

#[inline(always)]
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
#[inline(always)]
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

#[inline(always)]
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

/// Finds gaps in between jobs of an IGC row, and adds
/// their endpoints to an ordered set, eventually returned.
/// 
/// Used in the context of Theorem 2.
#[inline(always)]
pub fn gap_finder(row_jobs: &JobSet, (alpha, omega): (ByteSteps, ByteSteps)) -> BTreeSet<ByteSteps> {
    let mut res = BTreeSet::new();
    // Again we use event traversal. Row jobs are already sorted
    // since IGC itself is a product of event traversal.
    let mut evts = get_events(&row_jobs);
    // We either have found the next gap's start, or we haven't.
    // Initialize it optimistically to the left extreme of our
    // horizon.
    let mut gap_start = Some(alpha);

    while let Some(evt) = evts.pop() {
        match evt.evt_t {
            EventKind::Birth    => {
                if let Some(v) = gap_start {
                    if v < evt.time {
                        res.insert(v);
                        res.insert(evt.time);
                    }
                    gap_start = None;
                }
            },
            EventKind::Death    => { gap_start = Some(evt.time); }
        }
    }
    let last_gap_start = gap_start.unwrap();
    if last_gap_start < omega { res.insert(last_gap_start); }

    res
}