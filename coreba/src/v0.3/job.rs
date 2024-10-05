use crate::Job;

impl Job {
    /// Returns `true` if the job is live at moment `t`.
    pub fn is_live_at(&self, t: usize) -> bool {
        self.birth < t && self.death > t
    }

    /// Returns `true` if the job is original, i.e., was
    /// part of the user input and not created in the context
    /// of boxing.
    pub fn is_original(&self) -> bool {
        if let Some(_) = self.contents { false }
        else { true }
    }

    pub fn upd_off(&self) {
        let plc = self.home.as_ptr();
        unsafe {
            // I need the `unsafe` in order to change the offset
            // in-place instead of copying and re-setting stuff.
            (*plc).offset.replace((*plc).curr_offset.unwrap());
        }
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
        self.birth
            .cmp(&other.birth)
    }
}

impl PartialOrd for Job {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Job {
    fn eq(&self, other: &Self) -> bool {
        self.birth == other.birth
    }
}
//-----TREATING GROUPS OF JOBS (END)---------------------