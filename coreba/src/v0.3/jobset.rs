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
        if j.size.get() == 0 {
            return Err(JobError {
                message: String::from("Job with 0 size found!"),
                culprit: in_elts.remove(idx),
            });
        } else if j.size.get() != j.home.get().alloc_size {
            return Err(JobError {
                message: String::from("Job with disagreeing current/alloc size found!"),
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
        } else if let Some(a) = j.home.get().alignment {
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
        } else if j.home.get().req_size > j.home.get().alloc_size {
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
        .map(|x| Rc::new(x))
        .collect())
}