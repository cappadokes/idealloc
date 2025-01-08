use crate::helpe::*;

impl Job {
    pub fn get_id(&self) -> u32 {
        self.id
    }

    pub fn get_req_size(&self) -> ByteSteps {
        self.req_size
    }

    pub fn get_alignment(&self) -> Option<ByteSteps> {
        self.alignment
    }

    /// Creates a dummy [Job], to be filled
    /// with meaningful data from someone else.
    pub fn new() -> Self {
        Self {
            birth:              0,
            death:              0,
            size:               0,
            req_size:           0,
            alignment:          None,
            contents:           None,
            originals_boxed:    0,
            id:                 0,
        }
    }
  
    /// Creates a by-guarantee valid box containing
    /// the jobs in `contents`.
    /// 
    /// The new job's contents are sorted by the "big rocks first"
    /// heuristic--that is, by size.
    #[inline(always)]
    pub fn new_box(
        contents:   JobSet,
        height:     ByteSteps,
    ) -> Self {
        use std::{sync::atomic::AtomicU32, u32};
        static NEXT_ID: AtomicU32 = AtomicU32::new(u32::MAX);

        // The box must be high enough to enclose all jobs.
        debug_assert!(get_load(&contents) <= height, "Bad boxing requested");

        let mut birth = ByteSteps::MAX;
        let mut death = 0;
        let mut originals_boxed = 0;
        for j in &contents {
            // The box's temporal endpoints are the minimum
            // birth to the left, maximum death to the right.
            if j.birth < birth { birth = j.birth; }
            if j.death > death { death = j.death; }
            // We also keep track of how many original jobs
            // this box contains, somewhere in its hierarchy.
            if j.is_original() {
                originals_boxed += 1;
            } else { originals_boxed += j.originals_boxed; }
        }
        let id = NEXT_ID.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
        assert!(id != u32::MAX / 2 + 1);
        Self {
            size:               height,
            birth,
            death,
            req_size:           height,
            alignment:          None,
            contents:           Some(contents),
            originals_boxed,
            id,
        }
    }

    /// Returns `true` if the job is live at moment `t`.
    #[inline(always)]
    pub fn is_live_at(&self, t: usize) -> bool {
        self.birth < t && self.death > t
    }

    /// Returns `true` if job's lifetime is a subset of `space`.
    #[inline(always)]
    pub fn lives_within(&self, space: &(ByteSteps, ByteSteps)) -> bool {
        self.birth >= space.0 && self.death <= space.1
    }

    /// Returns `true` if the job is original, i.e., was
    /// part of the user input and not created in the context
    /// of boxing.
    #[inline(always)]
    pub fn is_original(&self) -> bool {
        if let Some(_) = self.contents {
            false
        } else {
            true
        }
    }

    /// Returns `true` if the job's entire lifetime ends
    /// before `t`.
    #[inline(always)]
    pub fn dies_before(&self, t: ByteSteps) -> bool {
        self.death <= t
    }

    /// Returns `true` if the job's entire lifetime starts 
    /// after `t`.
    #[inline(always)]
    pub fn born_after(&self, t: ByteSteps) -> bool {
        self.birth >= t
    }

    /// Returns the total number of discrete logical time units
    /// in which the [Job] is live.
    ///
    /// Given the fact that we consider *open* intervals, a job's
    /// lifetime must be AT LEAST 1, else there is no point in time
    /// in which it is considered live.
    ///
    /// This function assumes that the lifetime is legit.
    #[inline(always)]
    pub fn lifetime(&self) -> ByteSteps {
        self.death - self.birth - 1
    }

    #[inline(always)]
    pub fn area(&self) -> ByteSteps {
        self.size * self.lifetime()
    }
}

//-----TREATING GROUPS OF JOBS (START)---------------------
/*
   A (very) common operation is iterating over a set of jobs
   in order of increasing birth.

   To support such job containers, we implement the Ord trait
   of Job according to the `birth` field.
*/
impl Ord for Job {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.birth.cmp(&other.birth)
    }
}

impl PartialOrd for Job {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Job {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Job {}

impl Hash for Job {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
//-----TREATING GROUPS OF JOBS (END)---------------------
