use crate::Instance;

/// The boxing procedure is governed by
/// this floating-point value.
///
/// It is the ε in the "2+ε" bound provided
/// by Buchsbaum et al.
pub struct Epsilon {
    pub val: f32,
    last: u32,
    limit: f32,
}

impl Epsilon {
    pub fn new(jobs: &mut Instance) -> Self {
        Self {
            val: jobs.init_e(),
            last: 0,
            limit: 0.0,
        }
    }

    pub fn update(&mut self, ns: u32) {
        if ns > self.last {
            self.limit = self.val;
        }
        // Previous implementation was incrementing ε
        // by 1%. We care about faster convergence, so
        // we 10x the increase rate.
        self.val *= 1.1;
        self.last = ns;

        if self.limit > 0.0 && self.val > 2.0 * self.limit {
            self.val = self.limit;
        }
    }
}
