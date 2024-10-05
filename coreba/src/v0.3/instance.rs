use crate::{ByteSteps, Instance};

impl Instance {
    /// Applies Buchsbaum et al.'s Corollary 17.
    /// As far as floating point values go, we adopt
    /// 32bit-wide ones all across the project.
    pub fn init_e(&self) -> f32 {
        unimplemented!()
    }

    /// Calculates the makespan.
    pub fn opt(&self) -> ByteSteps {
        unimplemented!()
    }

    /// Calculates the optimal makespan,
    /// that is, the max load of an instance.
    pub fn load(&self) -> ByteSteps {
        match self.load {
            // Value has possibly been memoized, since
            // its calculation requires traversal and is
            // thus considered expensive.
            Some(v) => { v },
            None    => { unimplemented!() }
        }
    }

    /// Runs in case a better solution has been found and
    /// updates input job offsets accordingly.
    pub fn update_offsets(&self) {
        self.jobs.0
            .iter()
            .for_each(|j| { j.upd_off() });
    }
}