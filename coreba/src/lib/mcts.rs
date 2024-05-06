use std::hash::BuildHasherDefault;
use std::rc::Rc;
use std::cell::Cell;

use ahash::AHasher;
/*
    TAME RANDOMNESS START
    ----------------------

    Arguably the biggest problem of idealloc is its random
    nature, as expressed by the consecutive "critical point injection"
    in Theorem 2. We want to go one step further and make this process
    less random.

    As shall be shown later, this is a textbook single-player Monte Carlo
    tree search case.
*/

// We have already introduced compressing the available
// random points into subsets of concurrently live jobs.
// We now formulate point selection as Monte Carlo Tree Search.
//
// We refer to each point in the core idealloc algorithm where
// a random point must be selected as a "state". States are
// linked to other states, to encode dependencies between point
// selection. A state at which we arrive may or may not have been
// discovered before (in previous itereations of the algorithm).
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use rand::prelude::*;
use rand_chacha::ChaCha8Rng;
use core::cell::RefCell;
use crate::{elements::JobSet, utils::Area};

const IRREGULARITY: f64 = 0.75;
/*
type StateTime = (usize, usize, usize, usize, usize);

// One part of the state says at which last round were
// two jobs boxed together. It's a sparse matrix. Since
// job IDs can be arbitrary, we also keep a map to array indices.
struct BoxingArray {
    table:  sprs::CsMat<u16>,
    idmap:  AddrSet,
    n:      usize,
}

impl BoxingArray {
    fn new(jobs: &JobSet) -> Self {
        // Runs whenever a new heap is ready to be processed.
        let mut idx = 0;
        let mut idmap: AddrSet = IndexMap::default();
        for j in jobs.contents.values() {
            assert!(idmap.insert(j.id(), idx).is_none());
            idx += 1;
        };
        let n = idx + 1;
        let trimat = sprs::TriMatBase::new((n, n));
        Self {
            table:  trimat.to_csc(),
            idmap,
            n
        }
    }

    fn clear(&mut self) {
        // Runs after each idealloc iteration, to
        // restart the boxing array state without
        // erasing the mapping info.
        //let n = self.idmap.len();
        let trimat = sprs::TriMatBase::new((self.n, self.n));
        self.table = trimat.to_csc();
    }
}

struct StateDescription {
    box_arr:    sprs::CsMat<u16>,
    dims:       StateTime,
    jobs:       OriginalsIn,
    hor:        (usize, usize),
}

impl StateDescription {
    fn new(mother: &RandTameLogic, hor: (usize, usize), jobset: &mut JobSet) -> Self {
        let jobs: OriginalsIn = jobset.contents
            .values()
            .cloned()
            .collect();
        let box_arr = if let Some(a) = &mother.box_arr {
            a.table.clone()
        } else {
            panic!("No empty table allowed.");
        };
        Self {
            box_arr,
            dims:   mother.dims,
            jobs,
            hor
        }
    }
}

impl PartialEq for StateDescription {
    fn eq(&self, other: &Self) -> bool {
        self.hor == other.hor &&
        self.dims == other.dims &&
        self.jobs == other.jobs &&
        self.box_arr == other.box_arr
    }
}
*/

// Core structure to be passed back and forth between idealloc's
// machinery. Each new random point will be generated via this
// interface.
pub struct RandTameLogic {
    // At some points, randomness is unavoidable.
    // This generator serves that purpose.
    gen:        RefCell<ChaCha8Rng>,
    pol:        MCTSPolicy,
    // Lower bound on what can be achieved, equal to
    // the maximum load of the jobs to be placed.
    // Used to derive rewards.
    bound:      f64,
    // Transpositions of all visited states.
    table:      RefCell<Transpositions>,
    // History of games played.
    games:      Vec<Game>,
    // All moves encountered in the game.
    moves:      RefCell<GlobalActions>,
    // Selection or playout.
    stage:      Cell<MCTStage>,
    // Best reward reaped among all games.
    record:     f64,
    // ID for the next state to be created.
    next_id:    Cell<usize>,
    // Running game book-keeping.
    this_game:  RefCell<Game>,
    // Which node survives backprop.
    leaf_found: Cell<Option<usize>>,
    //box_arr:    Option<BoxingArray>,
    //pub dims:   StateTime,
    //keep_pol:   Cell<bool>,
}

impl RandTameLogic {
    /*
    fn get_job_idx(&self, id: usize) -> usize {
        if let Some(ref a) = self.box_arr {
            *a.idmap.get(&id).unwrap()
        } else {
            panic!("Can't have empty state at this point!");
        }
    }

    pub fn update_boxarr(&mut self, rf: Ref<OriginalsIn>) {
        for j_i in rf.iter() {
            for j_j in rf.iter() {
                let round_num = self.this_game.borrow().moves.len();
                if (round_num as u16) + 1 == u16::MAX {
                    panic!("Reached state limit!");
                }
                let i = self.get_job_idx(j_i.id());
                let j = self.get_job_idx(j_j.id());
                if let Some(ref mut a) = self.box_arr {
                    a.table.insert(i, j, round_num as u16);
                } else {
                    panic!("Can't have empty state at this point!");
                }
            }
        }
    }

    pub fn init_state(&mut self, jobs: &JobSet) {
        if let MCTSPolicy::Madman = self.get_policy() {
            return;
        }
        self.box_arr = Some(BoxingArray::new(jobs));
        self.dims = (0, 0, 0, 0, 0);
    }
    */

    pub fn report_hardness(&self) {
        let total_moves_num = self.games
            .iter()
            .fold(0.0, |acc, g| {
                acc + (g.moves.len() as f64)
            });
        let avg_moves_per_game = total_moves_num / (self.games.len() as f64);
        let total_moves_available = self.table
            .borrow()
            .iter()
            .fold(
                0.0, |acc, (_id, s)| {
                    acc + (s.retrieve_actions().borrow().len() as f64)
                }
            );
        let avg_branching_fact = total_moves_available / (self.table.borrow().len() as f64);
        println!("Average game length: {:.2}", avg_moves_per_game);
        println!("Average branching factor: {:.2}", avg_branching_fact);
    }

    pub fn new(pol: MCTSPolicy) -> Self {
        Self {
            gen:        RefCell::new(ChaCha8Rng::seed_from_u64(62)),
            pol,
            bound:      0.0,
            table:      RefCell::new(IndexMap::default()),
            games:      vec![],
            moves:      RefCell::new(IndexMap::default()),
            // "With a 75% probability, follow your policy
            // With a 25% probability, follow max rewards"
            // "In both cases the updates you have to do don't change"
            stage:      Cell::new(MCTStage::Selection(IRREGULARITY)),
            record:     f64::MIN,
            next_id:    Cell::new(0),
            this_game:  RefCell::new(Game::new()),
            leaf_found: Cell::new(None),
            //box_arr:    None,
            //dims:       (0, 0, 0, 0, 0),
            //keep_pol:   Cell::new(true),
        }
    }

    pub fn set_policy(&mut self, pol: MCTSPolicy) {
        self.pol = pol;
    }

    pub fn set_bound(&mut self, bound: f64) {
        // This is the lower bound for all possible
        // placements, used to derive each game's reward.
        self.bound = bound;
        // `set_bound` is called once per job set.
        // keep state IDs in control
        self.next_id.set(0);
    }

    pub fn postamble(&mut self, new_makespan: usize) {
        if let MCTSPolicy::Madman = self.get_policy() {
            return;
        }

        // Actions taken after a complete iteration is complete.
        // First, calculate new reward.
        let new_reward = self.bound / (new_makespan as f64);

        // Second, update your games memory.
        let mut last_game = self.this_game.replace(Game::new());
        last_game.outcome = new_reward;

        // Third, check if you broke your record. If you didn't, you
        // should erase all non-tree nodes from your table, to save memory.
        let mut keep_aethereals = false;
        if new_reward > self.record { 
            self.record = new_reward;
            keep_aethereals = true;
        }

        // Last but not least, you should do backprop.
        let mut it = last_game.moves
            .iter_mut()
            .rev();
        while let Some(round) = it.next() { 
                let state_id = round.state();
                let action = round.action;
                if !keep_aethereals {
                    match round.kind() {
                        RoundType::Aethereal    => {
                            self.remove_state(state_id);
                            continue;
                        },
                        RoundType::TopKnown => {
                            /*
                                We support no transpositions. TopKnown appears only
                                for cases that point toward a leaf. They should be
                                maintained--only the transition must be erased.
                            */
                            self.get_state(state_id)
                                .clean_transition(action);
                        },
                        RoundType::LowKnown => {
                            panic!("Impossible transition appeared!");
                        },
                        _   => {
                            if let Some(leaf_node_id) = self.leaf_found.get() {
                                if state_id == leaf_node_id {
                                    self.remove_state(state_id);
                                    continue;
                                }
                            }
                        },
                    }
                } else {
                    round.kind.set(RoundType::Concrete);
                }

                // In the rest of cases, everything should be kept.
                let entry = self.get_state(state_id);
                let stats = entry.get_action_stats(action);
                // Update total visits.
                let current_n = entry.visits.get();
                entry.visits.replace(current_n + 1.0);
                // Update max reward.
                let current_z_max = stats.borrow().z_max.get();
                if new_reward > current_z_max {
                    stats.borrow().z_max.replace(new_reward);
                }
                match self.get_policy() {
                    MCTSPolicy::UCT(_)  => {
                        stats.borrow().update(false, new_reward);
                    },
                    MCTSPolicy::RAVE    => {
                        // RAVE book-keeping happens just once.
                        //self.rave_bookkeeping(new_reward);
                    },
                    MCTSPolicy::Madman  => { /* nothing to do */ },
                    MCTSPolicy::UCTRAVE(_, _)   => {
                        stats.borrow().update(false, new_reward);
                        //self.rave_bookkeeping(new_reward);
                    }
                    _   => { unimplemented!(); }
                }
        }

        match self.get_policy() {
            MCTSPolicy::RAVE    => {
                self.rave_bookkeeping(new_reward, &last_game);
            },
            MCTSPolicy::UCTRAVE(_, _)   => {
                self.rave_bookkeeping(new_reward, &last_game);
            },
            _   => {},
        }

        // Now you may continue.
        self.games.push(last_game);
        self.stage.set(MCTStage::Selection(IRREGULARITY));
        //self.dims = (0, 0, 0, 0, 0);
        self.leaf_found.set(None);
        /*
        self.keep_pol.set(true);
        if let Some(ref mut arr) = self.box_arr {
            arr.clear();            
        } else {
            panic!("Can't have empty boxing array!");
        }
        */
    }

    fn remove_state(&self, key: usize) {
        self.table
            .borrow_mut()
            .remove(&key)
            // Assert that a node was there.
            .unwrap();
    }

    fn rave_bookkeeping(&self, new_reward: f64, g: &Game) {
        for (idx, round) in g 
            //.borrow()
            .moves
            .iter()
            .enumerate() {
                if let RoundType::Concrete = round.kind() {
                    // Keep info only for nodes that exist in the tree.
                    let state = self.get_state(round.state.get());
                    let actions = state.actions.borrow_mut();
                    for subtree_round in self.this_game
                        .borrow()
                        .moves
                        .iter()
                        .skip(idx + 1) {
                            let action_taken = subtree_round.action;
                            if let Some(s) = actions.get(&action_taken) {
                                s.borrow().update(true, new_reward);
                            }
                    }
                }
        }
    }

    pub fn gen_next_point(
        &mut self,
        jobs:   &mut JobSet,
        hor:    (usize, usize),
    ) -> usize {
        // No point in creating states with only one action.
        let mut candidate_actions: (IndexSet<usize>, (usize, usize)) = (IndexSet::new(), hor);
        // Core assumption of traversal is that jobs are sorted.
        jobs.sort_inc_birth();
        Area::traverse(jobs, &mut candidate_actions, Area::rand_update);
        if candidate_actions.0.len() == 1 {
            return candidate_actions.0.pop().unwrap();
        }
        if let MCTSPolicy::Madman = self.pol {
            // In the madman case, everything is random.
            // We just use the generator to pick a random moment.
            let num_actions = candidate_actions.0.len();
            let random_idx = self.gen.borrow_mut().gen_range(0..num_actions);

            return *(candidate_actions.0
                .get_index(random_idx)
                .unwrap());
        }

        // First step: identify actions, consolidate transpositions.
        let (this_state_id, is_leaf) = self.create_actions_state(candidate_actions.0, hor);
        let this_state = self.get_state(this_state_id);

        // Second step: we MIGHT need to consolidate transpositions with
        // this new state we're in, i.e., check if the last taken action
        // in the last visited state has Some value on its `next_state` field,
        // and behave accordingly.
        if let Some(prev_round) = self.this_game
            .borrow()
            .moves
            .last() {
                self.check_update_transition(prev_round, this_state_id, is_leaf);
        }
        /*
        let game_binder = self.this_game.replace(Game::new());
        if let Some(prev_round) = game_binder
            .moves
            .last() {
                match self.check_update_transition(
                    prev_round,
                    &mut this_state_id,
                    is_leaf) {
                        CheckResult::Found  => {
                            self.this_game.replace(game_binder);
                        },
                        CheckResult::Broken => {
                            // This is where state determinism breaks down.
                            // We must disregard everything related to this
                            // game, and switch to random play.
                            self.remove_state(this_state_id);
                            self.keep_pol.set(false);
                            let mut it = game_binder.moves
                                .iter()
                                .rev()
                                .peekable();
                            while let Some(round) = it.next() { 
                                    let state_id = round.state();
                                    let action = round.action;
                                    match round.kind() {
                                        RoundType::Aethereal    => {
                                            self.remove_state(state_id);
                                            continue;
                                        },
                                        RoundType::TopKnown => {
                                            if let Some(leaf_id) = self.leaf_found.get() {
                                                if leaf_id == state_id {
                                                    // We must also make sure that this transition never
                                                    // gets selected!
                                                    let parent_transition = it.peek().unwrap();
                                                    let parent_state_id = parent_transition.state();
                                                    let parent_action = parent_transition.action;
                                                    let parent_state = self.get_state(parent_state_id);
                                                    parent_state.clean_transition(parent_action);
                                                    parent_state.make_unreachable(parent_action);
                                                }
                                            }
                                            let tree_node = self.get_state(state_id);
                                            tree_node.clean_transition(action);
                                            continue;
                                        },
                                        RoundType::LowKnown => {
                                            self.remove_state(state_id);
                                            continue;
                                        },
                                        RoundType::Concrete   => {
                                            if let Some(leaf_id) = self.leaf_found.get() {
                                                if leaf_id == state_id {
                                                    // We must also make sure that this transition never
                                                    // gets selected!
                                                    let r = it.peek().unwrap();
                                                    let parent_state_id = r.state();
                                                    let parent_action = r.action;
                                                    let parent_state = self.get_state(parent_state_id);
                                                    parent_state.clean_transition(parent_action);
                                                    parent_state.make_unreachable(parent_action);
                                                    continue;
                                                }
                                            }
                                        },
                                    }
                            }
                        }
                }
        }
        */

        let candidate_actions = this_state.retrieve_actions().borrow();

        // All is now set up for action selection!
        let pol = self.get_policy();
        let res = match pol {
            MCTSPolicy::Madman  => {
                panic!("unreachable");
            },
            _   => {
                match self.stage.get() {
                    MCTStage::Selection(chance) => {
                        let floatcoin = self.gen.borrow_mut().gen_range(0.0..1.0);
                        let mut best = 0;
                        let mut max_val = f64::MIN;
                        for (k, stats) in candidate_actions.iter() {
                            let mut test: f64 = 0.0;
                            if floatcoin <= chance {
                                match pol {
                                    MCTSPolicy::Madman  => { /* unreachable */},
                                    MCTSPolicy::UCT(c)  => {
                                        test = stats.borrow().mean_val(false);
                                        if test == f64::MAX {
                                            best = *k;
                                            break;
                                        }
                                        // In the UCT case, we also have to add the exploration factor.
                                        let n_p = this_state.visits.get();
                                        test += c * (n_p.ln() / stats.borrow().n.get()).sqrt();
                                        test += stats.borrow().single_player();
                                    },
                                    MCTSPolicy::RAVE    => {
                                        test = stats.borrow().mean_val(true);
                                        if test == f64::MAX {
                                            best = *k;
                                            break;
                                        }
                                    },
                                    MCTSPolicy::UCTRAVE(c, b)   => {
                                        let n = stats.borrow().n.get();
                                        let n_amaf = stats.borrow().n_amaf.get();
                                        if n == 0.0 || n_amaf == 0.0 {
                                            best = *k;
                                            break;
                                        }
                                        let q = stats.borrow().mean_val(false);
                                        let q_amaf = stats.borrow().mean_val(true);
                                        let n_p = this_state.visits.get();
                                        let beta = (b / (3.0 * n_p + b)).sqrt();
                                        test = (1.0 - beta) * q + beta * q_amaf;
                                        test += c * (n_p.ln() / stats.borrow().n.get()).sqrt();

                                    }
                                    _   => { unimplemented!(); }
                                }
                            } else {
                                test = stats.borrow().z_max.get();
                                if test == 0.0 {
                                    best = *k;
                                    break;
                                }
                            }
                            if test > max_val {
                                best = *k;
                                max_val = test;
                            } 
                        }
                        
                        if best == 0 {
                            panic!("Again");
                        }
                        best
                    },
                    MCTStage::Playout   => {
                        let num_actions = candidate_actions.len();
                        let random_idx = self.gen.borrow_mut().gen_range(0..num_actions);

                        *(candidate_actions
                            .get_index(random_idx)
                            .unwrap()
                            .0)
                    }
                }
            }
        };

        // At this point we know (i) what state we're in and (ii) what action
        // we took. Only thing remaining book-keeping-wise is update the running
        // game vector as well as the global actions table.
        //
        // Let's start with the vector.
        if let Some(_) = this_state.transition_is_known(res) {
            // Both ends of a concrete transition are kept in the table.
            self.add_game_round(this_state_id, res, RoundType::Concrete);
        } else {
            if is_leaf {
                if let Some(selection_end_id) = self.leaf_found.get() {
                    if selection_end_id == this_state_id {
                        self.add_game_round(this_state_id, res, RoundType::TopKnown);
                    } else {
                        // If this is a leaf, the transition may change upon re-entry to "LowKnown".
                        self.add_game_round(this_state_id, res, RoundType::Aethereal);
                    }
                } else {
                    panic!("sdffl;ksa");
                }
                if let MCTStage::Selection(_) = self.stage.get() {
                    self.stage.set(MCTStage::Playout);
                }
            } else {
                // If this ain't a leaf, the transition may change upon re-entry to "Concrete",
                // if the *first* leaf is then found.
                self.add_game_round(this_state_id, res, RoundType::TopKnown);
            }
        }

        // Global actions need only be updated if (i) we're GRAVE and (ii) action ain't there.
        match self.get_policy() {
            MCTSPolicy::GRAVE   => {
                self.moves
                    .borrow_mut()
                    .entry(res)
                    .or_insert((0.0, 0.0));
            },
            _   => {}
        }

        res
    }

    fn add_game_round(
        &self,
        state_id:   usize,
        action:     usize,
        r_type:     RoundType,
    ) {
        self.this_game
            .borrow_mut()
            .moves
            .push(Rc::new(GameRound::new(state_id, action, r_type)));
    }

    fn check_update_transition(
        &self, 
        prev_round:     &GameRound,
        this_state:     usize,
        low_leaf:       bool,
    ) {
        // This function consolidates transpositions
        // with newly-learned transitions.
        let prev_state = self.get_state(prev_round.state.get());
        let action_taken = prev_round.action;
        if let Some(a) = prev_state.transition_is_known(action_taken) {
            match prev_round.kind() {
                RoundType::Concrete => {},
                _   => {
                    panic!("Non-concrete known transition!");
                }
            }
            // If transition is known, it should be stable.
            if a != this_state {
                /*
                // If it's not stable, mark previous round as concrete
                // so as not to mess with its transitions. Game should
                // be completely disregarded, all new states erased.
                prev_round.kind.set(RoundType::Concrete);
                CheckResult::Broken
            } else {
                CheckResult::Found
                */
                panic!("Whaaa!");
            }
        } else {
            match prev_round.kind() {
                RoundType::TopKnown => {
                    if !low_leaf {
                        // Maybe this branch never gets taken.
                        prev_round.kind.set(RoundType::Concrete);
                    } else {
                        // This is an exception: if the top is known and the leaf
                        // is that where selection stops, we mark the transition
                        // as concrete in order for the new node not to be deleted.
                        if let Some(leaf_id) = self.leaf_found.get() {
                            if leaf_id == prev_round.state() {
                                prev_round.kind.set(RoundType::Concrete);
                            }
                        }
                    }
                },
                RoundType::Aethereal    => {
                    if !low_leaf {
                        prev_round.kind.set(RoundType::LowKnown);
                    }
                },
                _   => {
                    panic!("Non-feasible transition!");
                }
            }
            prev_state.set_transition(action_taken, this_state);
        }
    }

    pub fn get_policy(&self) -> MCTSPolicy {
        self.pol
    }

    fn create_actions_state(
        &self,
        cand:   IndexSet<usize>,
        _hor:    (usize, usize),
    ) -> (usize, bool) {
        // This function returns the ID of an either existing, or
        // newly added, state of the transpositions table.
        //
        // It also returns a flag signifying whether the state
        // is a tree leaf or not, i.e., whether it existed in the table.

        // The first step of each game round is to identify both the
        // state we're in, and the actions available to it.
        //
        // It is possible that this state has been encountered before. If this
        // is true, a mere traversal of the transpositions table suffices.
        //

        if let Some(matching_key) = self.check_table() {
            return (matching_key, false);
        }

        // We know now that a new entry must be added in the transpositions.
        // Let's first define the actions available to it.
        //
        // These actions form the identity (description) of the state
        // we're finding ourselves in, to be used in order to find
        // transpositions.

        
        (self.insert_state(cand), true)
    }

    fn insert_state(
        &self,
        a:      IndexSet<usize>,
    ) -> usize {
        // Creates a new transposition, returns its ID
        // in the table.
        let new_entry = TableEntry::new(a);
        self.table
            .borrow_mut()
            .insert(self.next_id.get(), Rc::new(new_entry));

        // Check if this leaf should be where backprop stops pruning.
        if let None = self.leaf_found.get() {
            // Found!
            self.leaf_found.set(Some(self.next_id.get()));
        } // ...else we update nothing.

        // Update id of next state to be inserted.
        self.next_id.replace(self.next_id.get() + 1);

        self.next_id.get() - 1
    }

    fn get_state(&self, key: usize) -> Rc<TableEntry> {
        if let Some(entry) = self.table
            .borrow()
            .get(&key)
            .cloned() {
                entry
            }
        else {
            panic!("Bad state handling!");
        }
    }

    fn check_table(
        &self,
    ) -> Option<usize> {
        // Returns matching states from earlier simulations.
        if self.games.is_empty() {
            None
        } else if self.this_game
            .borrow()
            .moves
            .is_empty() {
                Some(0)
        } else {
            // This is classic UCT: doesn't care about transpositions.
            let last_round_handle = self.this_game
                .borrow();
            let last_round = last_round_handle.moves
                .last()
                .unwrap();
            let last_state_id = last_round.state();
            let last_action = last_round.action;
            let last_state = self.get_state(last_state_id);
            if let Some(s) = last_state.transition_is_known(last_action) {
                Some(s)
            } else {
                None
            }
            /*
            for (state_id, state) in self.table.borrow().iter() {
                if state.desc == *this_desc {
                    return Some(*state_id);
                }
            }

            None
            */

        }
    }
}

/*
enum CheckResult {
    Found,
    Broken,
}
*/

#[derive(Clone, Copy)]
pub enum MCTSPolicy {
    // Everything is random all the time.
    Madman,
    // Either greedy MCTS or UCT, depending on enclosed C value.
    UCT(f64),
    RAVE,
    // Either MC-RAVE or UCT-RAVE, depending on C (first) value.
    UCTRAVE(f64, f64),
    GRAVE,
}

// Each policy produces moves in two different stages.
// In the "selection" stage, moves are selected w.r.t. their
// policy-specific score. In the playout they're selected
// randomly. Selection may be "polluted" with moves that do
// not come from applying the policy criterion, but rather
// the max score criterion. This was taken from CADIAPLAYER.
#[derive(Clone, Copy)]
enum MCTStage {
    Selection(f64),
    Playout
}

// Each move made involves 2 states (initial, final). We must
// know which one of them belongs to the search tree, which not.
#[derive(Clone, Copy)]
enum RoundType {
    // Both states in the search tree.
    Concrete,
    // Neither of states in the search tree (playout under way).
    Aethereal,
    // Top state in the search tree.
    TopKnown,
    // Low state in the search tree.
    LowKnown,
}

// Each game is a series of rounds, where a state was met,
// and an action (i.e. point-in-time) was selected.
// All states are uniquely identified w/ usize numbers, same for actions.
struct GameRound {
    state:  Cell<usize>,
    action: usize,
    kind:   Cell<RoundType>,
}

impl GameRound {
    fn new(state: usize, action: usize, kind: RoundType) -> Self {
        Self {
            state:  Cell::new(state),
            action,
            kind:   Cell::new(kind),
        }
    }

    fn _coarse_eq(&self, other: &Self) -> bool {
        self.state() == other.state() &&
        self.action == other.action
    }

    fn state(&self) -> usize {
        self.state.get()
    }

    fn kind(&self) -> RoundType {
        self.kind.get()
    }
}

struct Game {
    moves:      Vec<Rc<GameRound>>,
    outcome:    f64
}

impl Game {
    fn new() -> Self {
        Self {
            moves:      vec![],
            outcome:    0.0,
        }
    }

    fn _get_round_idx(&self, i: usize) -> Option<Rc<GameRound>> {
        self.moves
            .get(i)
            .cloned()
    }
}

// In each state, many actions are allowed. Each action is evaluated,
// in that state's context, based on some stats. It also leads the game
// to some new state. All that info is kept here.
struct ActionStats {
    // In how many games was this action selected in this state.
    n:          Cell<f64>,
    // What's the cummulative reward for those games.
    z:          Cell<f64>,
    // What's the cummulative squared reward for those games.
    z_sq:          Cell<f64>,
    // Max reward scored with this move.
    z_max:      Cell<f64>,
    // Respective AMAF stats (updated also during other games).
    n_amaf:     Cell<f64>,
    z_amaf:     Cell<f64>,
    // What state is the next one (used for tree drawing).
    // Use options because we do not immediately know what
    // that next state is (simulation has to proceed).
    next_state: Cell<Option<usize>>,
}

impl ActionStats {
    fn new() -> Self {
        Self {
            n:          Cell::new(0.0),
            z:          Cell::new(0.0),
            z_sq:       Cell::new(0.0),
            z_max:      Cell::new(0.0),
            n_amaf:     Cell::new(0.0),
            z_amaf:     Cell::new(0.0),
            next_state: Cell::new(None),
        }
    }

    fn single_player(&self) -> f64 {
        const D: f64 = 0.62;

        let t_ni = self.n.get();
        if t_ni == 0.0 {
            return f64::MAX;
        }
        let x_mean_sq = self.mean_val(false).powi(2);
        let expectation = x_mean_sq * t_ni;
        let sum_x_sq = self.z_sq.get();
        if sum_x_sq - expectation + D < 0.0 {
            panic!("Bad D!");
        } else {
            ((sum_x_sq - expectation + D) / t_ni).sqrt()
        }
    }

    fn mean_val(&self, amaf: bool) -> f64 {
        let n = if !amaf {
            self.n.get()
        } else { self.n_amaf.get() };
        let z = if !amaf {
            self.z.get()
        } else { self.z_amaf.get() };
        if n == 0.0 {
            f64::MAX
        } else { z / n }
    }

    fn update(&self, amaf: bool, new_reward: f64) {
        // Update cummulative reward.
        let current_z = if !amaf {
            self.z.get()
        } else { self.z_amaf.get() };
        let current_n = if !amaf {
            self.n.get()
        } else { self.n_amaf.get() };
        if !amaf {
            self.z.replace(current_z + new_reward);
            self.n.replace(current_n + 1.0);
            // For the single-player term.
            let current_z_sq = self.z_sq.get();
            self.z_sq.replace(current_z_sq + new_reward.powi(2));
        } else {
            self.z_amaf.replace(current_z + new_reward);
            self.n_amaf.replace(current_n + 1.0);
        }
    }
}

// A transpositions table indexed by unique state IDs is used for game-playing.
// Each entry holds available actions, as well as a description.
struct TableEntry {
    actions:        RefCell<IndexMap<usize, Rc<RefCell<ActionStats>>, BuildHasherDefault<AHasher>>>,
    visits:         Cell<f64>,
}

impl TableEntry {
    fn new(
        actions:        IndexSet<usize>,
    ) -> Self {
        let mut res = IndexMap::default();
        actions.into_iter()
            .for_each(|a| { 
                res.insert(a, Rc::new(RefCell::new(ActionStats::new()))); 
            }
        );

        Self {
            actions:    RefCell::new(res),
            visits:     Cell::new(0.0),
        }
    }

    fn clean_transition(&self, action: usize) {
        let a = self.get_action_stats(action);
        let a = a.borrow_mut();
        a.next_state
            .set(None)
    }

    fn retrieve_actions(&self) -> &RefCell<IndexMap<usize, Rc<RefCell<ActionStats>>, BuildHasherDefault<AHasher>>> {
        &self.actions
    }

    fn transition_is_known(&self, action: usize) -> Option<usize> {
        let action_stats = self.get_action_stats(action);
        let action_stats_handler = action_stats.borrow();

        action_stats_handler
            .next_state
            .get()
    }

    fn get_action_stats(&self, key: usize) -> Rc<RefCell<ActionStats>> {
        if let Some(v) = self.actions
            .borrow()
            .get(&key)
            .cloned() {
                v
        } else { 
            panic!("Gotcha");
        }
    }

    fn set_transition(&self, a: usize, s: usize) {
        assert!(self.actions.borrow().keys().contains(&a));
        let a = self.get_action_stats(a);
        let a = a.borrow_mut();
        a.next_state.set(Some(s));
    }
}

// GRAVE updates AMAF values not only for moves not only in a game's context,
// but in all contexts! So we keep all moves here.
type GlobalActions  = IndexMap<usize, (f64, f64), BuildHasherDefault<AHasher>>;
// TableEntries will be needed in many points, so I'm wrapping them in Rc's.
type Transpositions = IndexMap<usize, Rc<TableEntry>, BuildHasherDefault<AHasher>>;