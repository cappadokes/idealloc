use std::rc::Rc;
use std::vec;

use indexmap::{IndexMap, IndexSet};

use crate::elements::{
    HeapSet, Job, JobSet, LiveSet, Placement,
    Result};
use crate::utils::myerrors::T16Error;
use crate::utils::{Area, IGCCtrl};
use crate::mcts::RandTameLogic;

impl Job {
    #[inline]
    pub fn overlaps_with(&self, other: &Self) -> bool {
        let (ab, ad) = self.endpoints();
        let (bb, bd) = other.endpoints();

        ad > bb && bd > ab
    }

    pub fn space_overlaps_with(&self, other: &Self) -> bool {
        let (ab, ad) = (self.offset(), self.next_avail_addr() - 1);
        let (bb, bd) = (other.offset(), other.next_avail_addr() - 1);
        let res = !(ad < bb || bd < ab);

        res
    }

    #[inline]
    pub fn lifetime(&self) -> usize {
        let (b, d) = self.endpoints();

        d - b
    }

    #[inline]
    pub fn area(&self) -> usize {
        self.lifetime().wrapping_mul(self.original_size())
    }

    #[inline]
    pub fn is_original(&self) -> bool {
        self.contents.borrow().is_none()
    }

    #[inline]
    fn is_live_at(&self, t: usize) -> bool {
        t > self.birth.get() && t < self.death.get()
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.height.get().0
    }

    #[inline]
    pub fn endpoints(&self) -> (usize, usize) {
        (self.birth.get(), self.death.get())
    }

    #[inline]
    pub fn initialize_size(&self, h: usize) {
        self.height.set((h, h));
    }

    #[inline]
    pub fn original_size(&self) -> usize {
        self.height.get().1
    }

    #[inline]
    pub fn restore_size(&self) {
        self.initialize_size(self.original_size())
    }

    #[inline]
    pub fn change_height_to(&self, h: usize) {
        let og = self.original_size();
        self.height.set((h, og));
    }

    #[inline]
    fn dies_before(&self, t: usize) -> bool {
        let (_birth, death) = self.endpoints();
        death <= t
    }

    #[inline]
    fn born_after(&self, t: usize) -> bool {
        let (birth, _death) = self.endpoints();
        birth >= t
    }

    #[inline]
    fn lives_within(&self, space: &(usize, usize)) -> bool {
        let (birth, death) = self.endpoints();
        birth >= space.0 && death <= space.1
    }

    #[inline]
    pub fn is_live_within(&self, space: &(usize, usize)) -> bool {
        let (birth, death) = self.endpoints();
        birth < space.1 && death > space.0
    }

    fn _push(&self, entry: Rc<Job>) {
        // Adds an ID-indexed job to a job's
        // ID-indexed contents.
        match *self.contents.borrow_mut() {
            None => {
                panic!("Attempted job pushing to empty contents.");
            }
            Some(ref mut m) => {
                if self.just_boxes.get() && entry.is_original() {
                    self.just_boxes.set(false);
                }
                if self.just_jobs.get() && !entry.is_original() {
                    self.just_jobs.set(false);
                }
                if !self.overlappin.get() && m.iter().any(|(_id, j_in)| j_in.overlaps_with(&entry))
                {
                    self.overlappin.set(true);
                }
                m.insert(entry.id(), entry.clone());
            }
        }
    }

    #[inline]
    pub fn place(&self, offset: usize) {
        let home = self.home.get();
        let mut final_address = home.heap + offset;
        if home.alignment != 0 && final_address % home.alignment != 0 {
            final_address += final_address % home.alignment;
        }

        self.home.set(Placement::new(
            home.heap,
            final_address - home.heap,
            home.alignment,
            home.req_size,
        ));
    }

    #[inline]
    pub fn next_avail_addr(&self) -> usize {
        self.home.get().offset.unwrap() + self.size()
    }

    #[inline]
    pub fn alignment(&self) -> usize {
        self.home.get().alignment
    }

    #[inline]
    pub fn offset(&self) -> usize {
        self.home.get().offset.unwrap()
    }
}

fn heights_closure(h: usize) -> impl Fn(&Rc<Job>) -> bool {
    // Make a closure for partitioning jobs according
    // to their height.
    move |j| j.size() <= h
}

// Lifetime parameters inferred from compiler errors via brute force.
fn time_closure<'c>(ctrl: &'c T2Control) -> impl Fn(&'_ Rc<Job>) -> bool + 'c {
    // Returns true iff job is live at *any* critical time (see T2).
    |j| ctrl.critical_points.iter().any(|t| j.is_live_at(*t))
}

impl JobSet {
    pub fn c_17(&mut self) -> f32 {
        let h_max = self.min_max_height().1 as f32;
        let l_max = self.max_load().1 as f32;

        (h_max / l_max).powf(1.0 / 7.0)
    }

    pub fn t_16(mut self, e: f32, rand_tamer: &mut RandTameLogic) -> Result<Self, T16Error> {
        match self.t_16_cond(e) {
            (true, _, _) => {
                let h_max = self.min_max_height().1;
                self.c_15((h_max as f32 / e).ceil() as usize, e, rand_tamer)
            }
            (false, mu, h) => {
                let target_size = (mu * (h as f32)).ceil() as usize;
                if target_size < self.min_max_height().0 || target_size == 1 {
                    Err(T16Error::new(self.jobs_boxed))
                } else {
                    let (x_s, mut x_l) = self.partition_by(heights_closure(target_size), false);
                    let small_boxed = x_s.c_15(h, mu, rand_tamer)?;
                    if small_boxed.jobs_boxed <= x_l.jobs_boxed {
                        return Err(T16Error::new(small_boxed.jobs_boxed));
                    }
                    // If all went well, we're gonna go one level deeper in the
                    // recursion. Update tamer's outer index!
                    //rand_tamer.dims.0 += 1;
                    x_l = x_l.merge_with(small_boxed, true, false);
                    x_l.t_16(e, rand_tamer)
                }
            }
        }
    }

    fn t_16_cond(&mut self, e: f32) -> (bool, f32, usize) {
        let (h_min, h_max) = self.min_max_height();
        let r = h_max as f32 / h_min as f32;

        let lg2r = r.log2().powi(2);
        let mu = e / lg2r;

        (
            lg2r < 1.0 / e,
            mu,
            (mu.powi(5) * (h_max as f32) / lg2r).ceil() as usize,
        )
    }

    fn c_15(self, h: usize, e: f32, rand_tamer: &mut RandTameLogic) -> Result<Self, T16Error> {
        let mut res: Self = JobSet::new();
        res.inherit_state(&self);
        //let target = self.jobs_in();
        let buckets = self.make_buckets(e);
        //rand_tamer.dims.1 = 0;
        for (h_i, mut unit_jobs) in buckets {
            unit_jobs.jobs_boxed = 0;
            unit_jobs.next_id = res.next_id;
            let h_param = 1.max((h as f32 / h_i as f32).floor() as usize);
            //let target = unit_jobs.jobs_in();
            let boxed = unit_jobs.t2(h_param, e, None, rand_tamer)?;
            //rand_tamer.dims.1 += 1;
            //debug_assert!(target == boxed.jobs_in(), "lalalolo");
            res = res.merge_with(boxed.change_heights(h), false, false);
        }

        //debug_assert!(res.jobs_in() == target, "lalaland");

        Ok(res)
    }

    fn make_buckets(self, e: f32) -> HeapSet {
        let mut i = 1;
        let mut source = self.clone();
        let mut res: HeapSet = IndexMap::default();
        while source.get_len() > 0 {
            let h = (1.0 + e).powi(i).floor() as usize;
            let (mut toward_bucket, rem) = source.partition_by(heights_closure(h), false);
            if toward_bucket.get_len() > 0 {
                toward_bucket = toward_bucket.round_heights(1);
                res.insert(h, toward_bucket);
            }
            source = rem;
            i += 1;
        }

        res
    }

    #[inline]
    fn round_heights(mut self, h: usize) -> JobSet {
        self.contents = self
            .contents
            .into_iter()
            .map(|(id, j)| {
                j.change_height_to(h);
                (id, j)
            })
            .collect();

        self
    }

    #[inline]
    fn change_heights(mut self, h: usize) -> JobSet {
        self.contents = self
            .contents
            .into_iter()
            .map(|(id, j)| {
                j.initialize_size(h);
                (id, j)
            })
            .collect();

        self
    }

    fn t2(
        mut self,
        h: usize,
        e: f32,
        ctrl: Option<T2Control>,
        rand_tamer: &mut RandTameLogic,
    ) -> Result<Self, T16Error> {
        let mut res = self.make_clone(IndexMap::default());
        let mut all_unresolved = self.make_clone(IndexMap::default());

        // We either come from the surface, or deeper in the recursion.
        let ctrl = if let Some(v) = ctrl {
            // "We are one level deeper in the recursion"
            //rand_tamer.dims.2 += 1;
            v
        } else {
            // "We are on the surface level of recursion"
            //rand_tamer.dims.2 = 0;
            // Holds the "real" ID of the bucket that triggered this recursion.
            //rand_tamer.dims.3 = 0;
            // "We operate on 0th bucket of Xth level of recursion".
            //rand_tamer.dims.4 = 1;
            T2Control::new(&mut self, rand_tamer)
        };

        let (r_coarse, x_coarse) = self.partition_by(time_closure(&ctrl), false);

        let ris = r_coarse.partition_ris(&ctrl);
        let xis = x_coarse.partition_xis(&ctrl);

        for (_space_id, mut set) in ris {
            // Each iteration might box some jobs, and thus change next_id.
            set.inherit_state(&res);
            let (boxed, unresolved) = set.lemma_1(h, e);
            res = res.merge_with(boxed, true, false);
            // All unresolved jobs will be subsequently packed with IGC.
            all_unresolved = all_unresolved.merge_with(unresolved, true, false);
        }

        all_unresolved.sort_inc_birth();
        let igc_rows: Vec<LiveSet> = if !all_unresolved.contents.is_empty() {
            all_unresolved.igc()
        } else { vec![] };

        // 2 tasks remain: allocate free spaces to future generations
        // and box the jobs packed via IGC.
        //
        // Future generations comprise X_is coupled with their own control structures.
        let mut future_gens: Vec<(T2Control, JobSet)> = vec![];
        for (idx, jobs) in xis {
            future_gens.push((ctrl.new_from_xi(idx), jobs));
        }

        let mut buf: LiveSet = IndexMap::default();
        let mut row_count = 0;
        for jobs in igc_rows {
            row_count += 1;
            allocate_spaces(&mut future_gens, &jobs);
            buf = merge_livesets(buf, jobs);
            if row_count % h == 0 {
                res.box_and_update(buf, h);
                buf = IndexMap::default();
            }
        }
        if buf.len() > 0 {
            res.box_and_update(buf, h);
        }

        // All critical points have been injected. Sort them and calculate
        // spaces.
        //let mut mem = rand_tamer.dims;
        for (mut ctrl, mut jobs) in future_gens {
            //let last_created_id = rand_tamer.dims.4;
            //rand_tamer.dims = mem;
            //rand_tamer.dims.4 = last_created_id + 1;
            jobs.inherit_state(&res);
            // Critical point injection helps a lot.
            // We want to make sure that the next call will box at
            // least one job.
            if !jobs.contents
                .values()
                .any(|j| {
                    ctrl.critical_points
                        .iter()
                        .any(|t| {
                            j.is_live_at(*t)
                        })
                }
            ) {
                let mut test = T2Control::compute_crit(&mut jobs, ctrl.bounding_interval, rand_tamer);
                while ctrl.critical_points.contains(&test) {
                    if jobs.get_len() == 1 {
                        panic!("Deep sh*t");
                    }
                    test = T2Control::compute_crit(&mut jobs, ctrl.bounding_interval, rand_tamer);
                }
                ctrl.critical_points.insert(test);
            }

            //rand_tamer.update_current_points(test);
            //mem = rand_tamer.dims;
            //rand_tamer.dims.3 = rand_tamer.dims.4;
            ctrl.critical_points.sort_unstable();
            ctrl.spaces = ctrl.get_critical_spaces();
            res = res.merge_with(jobs.t2(h, e, Some(ctrl), rand_tamer)?, true, false);
        }

        Ok(res)
    }

    #[inline]
    pub fn igc(&self) -> Vec<LiveSet> {
        // Performs Interval Graph Coloring (IGC) on own
        // contents. The result is a packing split in rows:
        // each row contains non-overlapping jobs.
        //
        // The result vector's ordering corresponds to the
        // ordering of the packing: first row is the lowest, then
        // the next-lowerst, etc.
        assert!(self._is_same_sized(), "Bad IGC requested!");
        let mut res = IGCCtrl::new();
        Area::traverse(&self, &mut res, Area::igc_update);

        // Give priority to rows containing most space load.
        res.rows.sort_unstable_by(|a, b| {
            b.values()
                .map(|j| j.lifetime())
                .fold(0, |acc: usize, j| acc.wrapping_add(j))
                .cmp(
                    &(a.values()
                        .map(|j| j.lifetime())
                        .fold(0, |acc: usize, j| acc.wrapping_add(j))),
                )
        });

        res.rows
    }

    fn _is_same_sized(&self) -> bool {
        let test_size = self.contents[0].size();
        self.contents.iter().all(|(_k, j)| j.size() == test_size)
    }

    #[inline]
    fn inherit_state(&mut self, other: &JobSet) {
        self.jobs_boxed = other.jobs_boxed;
        self.next_id = other.next_id;
    }

    fn partition_xis(self, ctrl: &T2Control) -> HeapSet {
        let mut res = IndexMap::default();
        let mut source = self.clone();
        for (i, space) in ctrl.spaces.iter().enumerate() {
            let (x_i, rest) = source.partition_by(|j| j.lives_within(space), false);
            if x_i.has_stuff() {
                res.insert(i, x_i);
            }
            source = rest;
        }

        res.sort_unstable_by(|idx_a, _, idx_b, _| (*idx_a).cmp(idx_b));

        res
    }

    fn partition_ris(self, ctrl: &T2Control) -> HeapSet {
        // The i in R_i is an index to the final map.
        let mut res = IndexMap::default();

        if ctrl.critical_points.len() < 2 {
            res.insert(ctrl.critical_points[0], self);
        } else {
            let q = ctrl.get_q();
            let mid = (q as f32 / 2.0).ceil() as usize;
            let t_mid = ctrl.get_point(mid);
            let (r_mid, rest) = self.partition_by(|j| j.is_live_at(t_mid), false);
            if r_mid.has_stuff() {
                res.insert(t_mid, r_mid);
            }

            if rest.has_stuff() {
                let (p, rest) = rest.partition_by(|j| j.dies_before(t_mid), false);
                if p.has_stuff() {
                    res = res
                        .into_iter()
                        .chain(p.partition_ris(&ctrl.inherit_and_spawn(mid, true)))
                        .collect();
                }

                if rest.has_stuff() {
                    let (q, _rest) = rest.partition_by(|j| j.born_after(t_mid), false);
                    res = res
                        .into_iter()
                        .chain(q.partition_ris(&ctrl.inherit_and_spawn(mid, false)))
                        .collect();
                }
            }
        }

        res
    }

    #[inline]
    pub fn restore_heights(&mut self) {
        for (_k, j) in &self.contents {
            j.restore_size();
        }
    }

    #[inline]
    fn has_stuff(&self) -> bool {
        self.contents.len() > 0
    }

    fn try_box(mut self, h: usize) -> (Rc<Job>, usize, usize) {
        // Boxes a set of jobs into a bigger box.
        // Assumes that stats have already been computed.
        // Panics if box size is bigger than candidate
        // contents' maximum load.
        let mut originals_boxed: usize = 0;
        let res = Rc::new(Job::new());

        // New box birth, death match the contents' horizon.
        // (smallest_birth, biggest_death)
        let (birth, death) = self.get_horizon();
        res.birth.set(birth);
        res.death.set(death);

        // Size taken from the input.
        res.initialize_size(h);
        // ID is stored in the JobSet.
        res.id.set(self.next_id);

        // We now fill the contents map with ID-indexed
        // clones of the JobSet.
        res.contents.replace(Some(IndexMap::default()));
        for (_, entry) in self.contents {
            if entry.is_original() {
                originals_boxed += 1;
            }
            res._push(entry);
        }

        (res, self.next_id + 1, originals_boxed)
    }

    #[inline]
    fn get_horizon(&mut self) -> (usize, usize) {
        match self.stats.horizon {
            None => {
                let mut min_birth = usize::MAX;
                let mut max_death = 0;

                for j in self.contents.values() {
                    let (b, d) = j.endpoints();
                    if b < min_birth {
                        min_birth = b;
                    }
                    if d > max_death {
                        max_death = d;
                    }
                }

                self.stats.horizon = Some((min_birth, max_death));

                (min_birth, max_death)
            }
            Some(v) => v,
        }
    }

    fn lemma_1(mut self, h: usize, e: f32) -> (JobSet, JobSet) {
        let (out_vertical, out_horizontal) = self.strip_cuttin(h, e, true);
        let mut unresolved = self.make_clone(merge_livesets(out_vertical, out_horizontal));
        for (k, _j) in &unresolved.contents {
            self.contents.remove(k);
        }
        // Now self.contents is a buffer from where inner strips will be
        // repeatedly cut and boxed.

        // This is where boxed jobs are stored to be returned as a JobSet
        // ready to be merged.
        let mut res = self.make_clone(IndexMap::default());

        while self.contents.len() > 0 {
            // Cut next strips...
            let (vertical, horizontal) = self.strip_cuttin(h, e, false);
            // Update buffer...
            vertical
                .iter()
                .chain(horizontal.iter())
                .for_each(|(k, _j)| {
                    self.contents.remove(k);
                });

            // At every step, we box together every h elements
            // of each strip.
            res.strip_boxin(vertical, h, true);
            res.strip_boxin(horizontal, h, false);
        }

        unresolved.inherit_state(&res);

        (res, unresolved)
    }

    fn strip_boxin(&mut self, mut trg: LiveSet, h: usize, is_vert: bool) {
        let mut buffer: LiveSet = IndexMap::default();
        if is_vert {
            sort_dec_death(&mut trg);
        } else {
            sort_inc_birth(&mut trg);
        }

        for (k, j) in trg.into_iter() {
            buffer.insert(k, j);
            if buffer.len() == h {
                self.box_and_update(buffer, h);
                buffer = IndexMap::default();
            }
        }

        if buffer.len() > 0 {
            self.box_and_update(buffer, h);
        }
    }

    #[inline]
    fn box_and_update(&mut self, trg: LiveSet, h: usize) {
        let helper = self.make_clone(trg);
        let (new_box, next_id, to_box) = helper.try_box(h);

        self.insert_job(new_box);
        self.next_id = next_id;
        self.jobs_boxed += to_box;
    }

    fn strip_cuttin(&mut self, h: usize, e: f32, cut_outer: bool) -> (LiveSet, LiveSet) {
        let strip_cnt: usize;
        if cut_outer {
            strip_cnt = h * (1.0 / e.powi(2)).ceil() as usize;
        } else {
            strip_cnt = h * (1.0 / e).ceil() as usize;
        }

        // Vertical strips rely on jobs being ordered by increasing birth.
        self.sort_inc_birth();
        let vert: LiveSet = self
            .contents
            .iter()
            .take(strip_cnt)
            // Clone instead of moving, to use self.contents further below.
            .map(|(id, refr)| (*id, refr.clone()))
            .collect();

        // Horizontal strips rely on jobs being ordered by decreasing death.
        self.sort_dec_death();
        let hor: LiveSet = self
            .contents
            .iter()
            // Since we keep self.contents intact, make sure that this
            // strip does not contain jobs of the other one.
            .filter(|(k, _j)| !vert.contains_key(*k))
            .take(strip_cnt)
            .map(|(id, refr)| (*id, refr.clone()))
            .collect();

        (vert, hor)
    }

    #[inline]
    pub fn check_overlap(&mut self, are_jobs: bool) {
        if self.has_stuff() {
            if are_jobs {
                self.just_jobs = true;
                self.just_boxes = false;
            } else {
                self.just_boxes = true;
                self.just_jobs = false;
            }
            let mut to_skip = 0;
            self.overlappin = loop {
                match self.contents.iter().skip(to_skip).next() {
                    Some((_, j)) => {
                        to_skip += 1;
                        if self
                            .contents
                            .iter()
                            .skip(to_skip)
                            .any(|(_, jt)| j.overlaps_with(jt))
                        {
                            break true;
                        }
                    }
                    None => {
                        break false;
                    }
                }
            };
        }
    }

    #[inline]
    pub fn jobs_in(&self) -> usize {
        let mut count = 0;

        for (_, j) in &self.contents {
            if j.is_original() {
                count += 1;
            } else {
                let temp = j.contents.borrow();
                count += jobs_in_liveset(temp.as_ref().unwrap());
            }
        }

        count
    }
}

pub struct LilEGen {
    pub val: f32,
    last: usize,
    limit: f32,
}

impl LilEGen {
    pub fn new(jobs: &mut JobSet) -> Self {
        Self {
            val: jobs.c_17(),
            last: 0,
            limit: 0.0,
        }
    }

    pub fn update(&mut self, ns: usize) {
        if ns > self.last {
            self.limit = self.val;
        }
        self.val *= 1.01;
        self.last = ns;

        if self.limit > 0.0 && self.val > 2.0 * self.limit {
            self.val = self.limit;
        }
    }
}

struct T2Control {
    bounding_interval: (usize, usize),
    pub critical_points: IndexSet<usize>,
    spaces: Vec<(usize, usize)>,
}

impl T2Control {
    fn new(jobs: &mut JobSet, rand_tamer: &mut RandTameLogic) -> Self {
        let mut res = Self {
            bounding_interval: jobs.get_horizon(),
            critical_points: Self::init_crit(jobs, rand_tamer),
            spaces: vec![],
        };

        res.spaces = res.get_critical_spaces();

        res
    }

    #[inline]
    fn encloses(&self, time: usize) -> bool {
        self.bounding_interval.0 <= time && self.bounding_interval.1 >= time
    }

    #[inline]
    fn new_from_xi(&self, idx: usize) -> Self {
        let (s, e) = self.spaces[idx];
        let mut res = Self {
            bounding_interval: (s, e),
            critical_points: IndexSet::new(),
            spaces: vec![],
        };

        res.critical_points.insert(s);
        res.critical_points.insert(e);

        res
    }

    #[inline]
    fn init_crit(jobs: &mut JobSet, rand_tamer: &mut RandTameLogic) -> IndexSet<usize> {
        let mut res = IndexSet::new();
        let hor = jobs.get_horizon();

        res.insert(hor.0);
        res.insert(hor.1);
        let rand_point = Self::compute_crit(jobs, hor, rand_tamer);
        res.insert(rand_point);

        res
    }

    fn compute_crit(
        jobs:       &mut JobSet,
        hor:        (usize, usize),
        rand_tamer: &mut RandTameLogic,
    ) -> usize {
        // This is (arguably) the only place where randomness creeps in.
        // A random moment is picked, fulfilling the following criteria:
        //  a) it is part of the horizon
        //  b) at least one job is live at said moment
        //
        // Our initial approach was quite naive and time-consuming: we used
        // a random number generator iteratively fishing the horizon pond
        // until both of the criteria was satisfied.
        // 
        // We have now leveled up to a classy Monte Carlo Tree Search approach
        // (check `lib/mcts.rs`)

        rand_tamer.gen_next_point(jobs, hor)
    }

    #[inline]
    fn get_q(&self) -> usize {
        match self.critical_points.len().checked_sub(2) {
            Some(v) => v,
            None => {
                panic!("Bad q requested.");
            }
        }
    }

    #[inline]
    fn get_point(&self, idx: usize) -> usize {
        *self
            .critical_points
            .get_index(idx)
            .expect("Bad crit pt idxing.")
    }

    fn inherit_and_spawn(&self, pivot: usize, get_lower: bool) -> Self {
        // Used while recursively partitioning R during T2.
        // Only critical points are of interest. Keeps either
        // bottom or top half, excluding pivot element.
        Self {
            bounding_interval: self.bounding_interval,
            critical_points: match get_lower {
                true => self.critical_points.iter().take(pivot).copied().collect(),
                false => self
                    .critical_points
                    .iter()
                    .skip(pivot + 1)
                    .copied()
                    .collect(),
            },
            spaces: vec![],
        }
    }

    #[inline]
    fn get_critical_spaces(&self) -> Vec<(usize, usize)> {
        // Used for partitioning x_coarse during T2.
        // Each X_i holds jobs that live within the
        // spaces one gets if one zips each critical
        // point with the next one.
        self.critical_points
            .iter()
            .copied()
            .zip(self.critical_points.iter().skip(1).copied())
            .collect()
    }
}

#[inline]
pub fn merge_livesets(one: LiveSet, other: LiveSet) -> LiveSet {
    let mut res: LiveSet = IndexMap::default();
    for (id, j) in one {
        if let Some(j) = res.insert(id, j) {
            panic!("Tried to override {:?}", j);
        }
    }
    for (id, j) in other {
        if let Some(j) = res.insert(id, j) {
            panic!("Tried to override {:?}", j);
        }
    }

    res
}

#[inline]
fn sort_dec_death(trg: &mut LiveSet) {
    trg.sort_unstable_by(|_, a, _, b| {
        let (_ab, ad) = a.endpoints();
        let (_bb, bd) = b.endpoints();

        bd.cmp(&ad)
    });
}

#[inline]
pub fn sort_inc_birth(trg: &mut LiveSet) {
    trg.sort_unstable_by(|_, a, _, b| {
        let (ab, _ad) = a.endpoints();
        let (bb, _bd) = b.endpoints();

        ab.cmp(&bb)
    });
}

fn allocate_spaces(fut: &mut Vec<(T2Control, JobSet)>, jobs: &LiveSet) {
    for (_, j) in jobs {
        let (birth, death) = j.endpoints();
        let mut started = false;
        for (ctrl, _xi) in fut.iter_mut() {
            if !started && ctrl.encloses(birth) {
                started = true;
                ctrl.critical_points.insert(birth);
            };
            if started && ctrl.encloses(death) {
                ctrl.critical_points.insert(death);
                break;
            }
        }
    }
}

#[inline]
fn jobs_in_liveset(t: &LiveSet) -> usize {
    let mut count = 0;

    for (_, j) in t {
        if j.is_original() {
            count += 1;
        } else {
            let temp = j.contents.borrow();
            count += jobs_in_liveset(temp.as_ref().unwrap());
        }
    }

    count
}