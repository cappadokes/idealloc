use crate::ffi::*;
use once_cell::sync::Lazy;
use std::sync::{Mutex, TryLockError, Condvar};

use std::hash::BuildHasherDefault;
use indexmap::IndexMap;
use ahash::AHasher;

type AppOwnedBook = IndexMap<pthread_t, bool, BuildHasherDefault<AHasher>>;
pub static mut APP_OWNED: Lazy<AppOwnedBook> = Lazy::new(|| {
    AppOwnedBook::default()
});
pub const MAX_THREADS_NUM: usize = 256;

pub unsafe fn is_app_owned() -> bool {
    //! The same logic is followed with the tracer's `must_log` function.
    
    static mut INIT_LOCK: Option<Mutex<()>> = Some(Mutex::new(()));
    static mut INIT_THREAD: pthread_t = 0;

    let tid = pthread_self();
    match &INIT_LOCK {
        // Existence of the mutex is a signal that the map
        // has not been initialized.
        Some(mtx)   => {
            match mtx.try_lock() {
                Ok(_)   => {
                    INIT_THREAD = tid;
                    // This operation will trigger one more allocation...
                    APP_OWNED.reserve(MAX_THREADS_NUM);
                    INIT_LOCK = None;
                },
                Err(e)  => {
                    match e {
                        TryLockError::Poisoned(_)   => {
                            graceful_exit("Poisoned mutex upon TID map init.");
                        },
                        TryLockError::WouldBlock    => {
                            if tid == INIT_THREAD {
                                // ...which will reach this block. Since we're
                                // in the context of book-keeping, no logging
                                // must be made.
                                return false;
                            }
                            // The rest of the threads must wait until
                            // initialization is complete.
                            while let Some(_) = &INIT_LOCK {}
                        }
                    }
                }
            }
        },
        None    => {
            if tid == INIT_THREAD {
                // Corner case. Unfortunately the first
                // `insert` command allocates too.
                INIT_THREAD = 0;
                if let None = APP_OWNED.get(&tid) {
                    return false;
                }
            }
        }
    };
    // This is the most frequently run block. The code above
    // deals with initialization.
    match APP_OWNED.get(&tid) {
        Some(verdict)   => { 
            *verdict 
        },
        None            => {
            if APP_OWNED.len() + 1 > MAX_THREADS_NUM {
                graceful_exit("More threads observed than maximum number allowed!");
            }
            APP_OWNED.insert(tid, true);
            true
        }
    }
}

#[inline(always)]
pub unsafe fn steal_ownership() {
    let rf = APP_OWNED.get_mut(&pthread_self()).unwrap();
    *rf = false;
}

#[inline(always)]
pub unsafe fn return_ownership() {
    let rf = APP_OWNED.get_mut(&pthread_self()).unwrap();
    *rf = true;
}

use std::fs::File;
use std::io::{Seek, BufReader, Read};
use std::env;
use std::path::Path;

/// A request may have one of 7 types, accompanied by
/// the type's arguments--all of which have been stored
/// as `usize`-length in the trace file.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ReqType {
    Malloc          (usize),
    Free            (usize),
    Calloc          (usize, usize),
    ReAlloc         (usize, usize),
    AlignedAlloc    (usize, usize),
    MemAlign        (usize, usize),
    PosixMemAlign   (usize, usize, usize),
    Done
}

impl ReqType {
    fn is_free(&self) -> bool {
        if let ReqType::Free(_) = self {
            true
        } else { 
            false 
        }
    }
    
    /// Works in 2 levels: on the first level, returns a predicate
    /// about if 2 request types are *mostly* the same, that is,
    /// same type, same non-ptr arguments. On the second level, it
    /// returns potential ptr-based arguments and the traced values
    /// they must match.
    fn fine_eq(&self, other: &Self) -> (bool, usize, usize) {
        match self {
            ReqType::Malloc(real_size)  => {
                if let ReqType::Malloc(traced_size) = other {
                    return (real_size == traced_size, 0, 0);
                }
            },
            ReqType::Free(real_address)  => {
                if let ReqType::Free(traced_address) = other {
                    return (true, *real_address, *traced_address); 
                }
            },
            ReqType::Calloc(real_nobj, real_size)  => {
                if let ReqType::Calloc(traced_nobj, traced_size) = other {
                    return (
                        real_nobj == traced_nobj && real_size == traced_size,
                        0, 0
                    );
                }
            },
            ReqType::ReAlloc(real_address, real_size)  => {
                if let ReqType::ReAlloc(traced_address, traced_size) = other { 
                    return (
                        real_size == traced_size,
                        *real_address,
                        *traced_address
                    );
                }
            },
            ReqType::AlignedAlloc(real_alignment, real_size)  => {
                if let ReqType::AlignedAlloc(traced_alignment, traced_size) = other { 
                    return (
                        real_alignment == traced_alignment && real_size == traced_size,
                        0, 0
                    );
                }
            },
            ReqType::MemAlign(real_alignment, real_size)  => {
                if let ReqType::MemAlign(traced_alignment, traced_size) = other {
                    return (
                        real_alignment == traced_alignment && real_size == traced_size,
                        0, 0
                    );
                }
            },
            ReqType::PosixMemAlign(real_address, real_alignment, real_size)  => {
                if let ReqType::PosixMemAlign(traced_address, traced_alignment, traced_size) = other {
                    return (
                        real_alignment == traced_alignment && real_size == traced_size,
                        *real_address,
                        *traced_address
                    )
                }
            },
            ReqType::Done  => {},
        };

        (false, 0, 0)
    }
}

// Exploit trace packet's first sentinel byte to swiftly index into
// the correct request type.
const CODES: [ReqType; 7] = [
    ReqType::Malloc(0),
    ReqType::Free(0),
    ReqType::Calloc(0, 0),
    ReqType::ReAlloc(0, 0),
    ReqType::AlignedAlloc(0, 0),
    ReqType::MemAlign(0, 0),
    ReqType::PosixMemAlign(0, 0, 0),
];

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Request {
    // Info held matches the TRACED data!
    rtype:  ReqType,
    tid:    usize
}

impl Request {
    fn is_useless(&self) -> bool {
        match self.rtype {
            ReqType::Free(0)    => { true },
            _                   => { false }
        }
    }

    pub fn new_event(trace: &mut BufReader<File>) -> Event {
        //! Spawns a new event from a trace file.
        let mut sentinel: [u8; 1] = [0];
        let mut baby_req = Self { rtype: ReqType::Done, tid: 0 };
        let mut baby_place = None;
        if let Ok(_) = trace.read_exact(&mut sentinel) {
            if  sentinel[0] != 0x05 &&
                sentinel[0] != 0x12 &&
                sentinel[0] != 0x26 &&
                sentinel[0] != 0x36 &&
                sentinel[0] != 0x46 &&
                sentinel[0] != 0x56 &&
                sentinel[0] != 0x67 {
                    unsafe { graceful_exit("Corrupted trace file.");} 
            }
            let req_type = CODES[((sentinel[0] & 0b11110000u8) >> 4) as usize];
            baby_req.rtype = req_type;
            let words_left = sentinel[0] & 0b00001111u8;
            // Buffer to get filled repeatedly.
            let mut word_buf: [u8; 8] = [0; 8];
            let mut i: u8 = 0;
            let mut block_start: usize = 0;
            let mut map_base: usize = 0;
            let mut tid: usize = 0;
            let mut block_size: usize = 0;
            while i < words_left {
                trace.read_exact(&mut word_buf).expect("Unexpected trace EOF");
                let data = usize::from_be_bytes(word_buf);
                if i == words_left - 1 && !req_type.is_free() {
                    map_base = data;
                } else if i == words_left - 2 && !req_type.is_free() {
                    tid = data;
                } else if !req_type.is_free() && i == words_left - 3 {
                    block_size = data;
                } else if !req_type.is_free() && i == words_left - 4 {
                    block_start = data;
                }
                match baby_req.rtype {
                    ReqType::Malloc(ref mut size)  => {
                        if i == 0 { *size = data; }
                    },
                    ReqType::Calloc(ref mut nobj, ref mut size)  => {
                        if i == 0 { *nobj = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::ReAlloc(ref mut p, ref mut size)  => {
                        if i == 0 { *p = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::AlignedAlloc(ref mut a, ref mut size) => {
                        if i == 0 { *a = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::MemAlign(ref mut a, ref mut size) => {
                        if i == 0 { *a = data; }
                        else if i == 1 { *size = data; }
                    },
                    ReqType::PosixMemAlign(ref mut p, ref mut a, ref mut size) => {
                        if i == 0 { *p = data; }
                        else if i == 1 { *a = data; }
                        else if i == 2 { *size = data; }
                    },
                    ReqType::Free(ref mut p)   => {
                        if i == 0 { *p = data; }
                        else { tid = data; }
                    },
                    ReqType::Done   => {
                        unsafe { graceful_exit("Unreachable block reached."); }
                    },
                };
                i += 1;
            };
            if !baby_req.rtype.is_free() {
                baby_place = Some(Placement::new(map_base, block_start - map_base, block_size));
            }
            baby_req.tid = tid;
        };

        (baby_req, baby_place)
    }

    pub fn new_call(r: ReqType, world: &mut State) -> Self {
        //! Generates a Request based on real-time data. This
        //! must match the next-expected one, dictated by the
        //! `world` structure.
        
        // To be returned if request cannot be created right now,
        // e.g., a `free` for which the address doesn't yet exist.
        let baby = Self { rtype: ReqType::Done, tid: 0 };
        let (coarse_eq, real_add, traced_add) = r.fine_eq(&world.ner.0.rtype);
        // 1st-level check: non-ptr args equality.
        if !coarse_eq { return baby; }
        // 2nd-level check: ptr arguments.
        if traced_add != 0 {
            // NULL ptrs have been traced as zero values.
            match world.objects.get(&real_add) {
                None    => { return baby; },
                Some(a) => {
                    if *a != traced_add { return baby; }
                }
            }            
        } else if real_add != 0 { return baby; }

        // Reaching this point means: (i) non-ptr args are equal and
        // (ii) either ptr args are BOTH null or ptr args are non-null
        // and equal. The last thing to check is whether the TIDs match.
        //
        // If the TID appears for the first time, it by definition matches
        // the one expected (yes, there is a deadlock hazard here).
        unsafe {
            let real_tid = pthread_self() as usize;
            match world.threads.get(&real_tid) {
                None    => { 
                    let fake_tid = world.ner.0.tid;
                    steal_ownership();
                    world.threads.insert(real_tid, fake_tid);
                    return_ownership();
                }
                Some(fake_tid)  => {
                    if *fake_tid != world.ner.0.tid { return baby; }
                }
            };
        };

        // Requests are equal. Return NER so that
        // the comparison in `lobby`succeeds by definition.
        world.ner.0
    }

    fn is_free(&self) -> bool {
        self.rtype.is_free()
    }

    pub fn is_done(&self) -> bool {
        if let ReqType::Done = self.rtype { true }
        else { false }
    }
}

#[derive(Clone, Copy)]
pub struct Placement {
    heap:   usize,
    offset: usize,
    size:   usize
}

impl Placement {
    fn new(heap: usize, offset: usize, size: usize) -> Self {
        Self { heap, offset, size }
    }
    fn implied_makespan(&self) -> usize {
        self.offset + self.size - 1
    }
}

struct Mapping {
    base:   *mut void,
    _ceil:   *mut void,
}

impl Mapping {
    fn new(makespan: usize) -> Self {
        unsafe {
            let mem = libc::mmap(core::ptr::null_mut(), 
                        makespan,
                        libc::PROT_READ | libc::PROT_WRITE,
                        libc::MAP_PRIVATE | libc::MAP_ANONYMOUS, 
                        -1, 
                        0);
            if mem.is_null() { graceful_exit("mmap error"); }
            else { Self { base: mem, _ceil: mem.add(makespan) } }
        }
    }
}

type Event      = (Request, Option<Placement>);
type MapInfo    = (Option<Mapping>, usize);

pub struct State {
    trace:          BufReader<File>,
    ner:            Event,
    heaps:          IndexMap<usize, MapInfo, BuildHasherDefault<AHasher>>,
    pub threads:    IndexMap<usize, usize, BuildHasherDefault<AHasher>>,
    objects:        IndexMap<usize, usize, BuildHasherDefault<AHasher>>,
}

impl State {
    pub unsafe fn new() -> Self {
        steal_ownership();
        let mut trace: BufReader<File>;
        let threads = IndexMap::default();
        let objects = IndexMap::default();
        let mut heaps = IndexMap::default();
        let mut ner: Event;
        match env::var("TRACEFILE") {
            Err(_)          => { graceful_exit("TRACEFILE not defined!"); },
            Ok(s) => {
                let file_path = Path::new(&s);
                if !file_path.exists() { graceful_exit("Trace file doesn't exist!"); }
                else if !file_path.is_file() { graceful_exit("Trace file is not a file!"); }
                else {
                    trace = BufReader::new(File::open(file_path).unwrap());
                    loop {
                        let (next_req, placement) = Request::new_event(&mut trace);
                        if next_req.is_useless() { continue; }
                        if let ReqType::Done = next_req.rtype { break; }
                        if !next_req.is_free() {
                            let placement = placement.unwrap();
                            let makespan = placement.implied_makespan();
                            match heaps.get_mut(&placement.heap) {
                                None    => { heaps.insert(placement.heap, (None, makespan)); },
                                Some((_, v))    => { if makespan > *v { *v = makespan; } }
                            };
                        }
                    }
                }
            }
        };
        trace.rewind().expect("Trace rewind failure!");
        ner = Request::new_event(&mut trace);
        while ner.0.is_useless() { ner = Request::new_event(&mut trace); }
        return_ownership();
        Self { trace, ner, heaps, objects, threads }
    }
}

static WORLD_LOCK:  Mutex<()> = Mutex::new(());
static ITS_TIME:    Condvar = Condvar::new();
static mut WORLD:   Lazy<State> = Lazy::new(|| { unsafe {State::new() }});

pub fn lobby(r: ReqType) -> *mut void {
    let mut guard = WORLD_LOCK.lock().unwrap();
    unsafe {
        while Request::new_call(r, &mut WORLD) != WORLD.ner.0 {
            guard = ITS_TIME.wait(guard).unwrap();
        };

        // SUCCESS! Reaching this block means that the thread which
        // holds the lock gave a request which completely matches
        // NER. So we have to reproduce the traced decision contained there.
        //
        // First step: check if we're talking allocation/de-allocation.
        steal_ownership();
        let mut res = std::ptr::null_mut::<void>();
        if let ReqType::Free(real_add) = r {
            // In the case of de-allocation, update objects book.
            if real_add != 0 {
                WORLD.objects.remove(&real_add).expect("Tried to free non-existent object.");
            }
        } else {
            // In the case of allocation, first check if heap has been spawned.
            let traced_heap = WORLD.ner.1.unwrap().heap;
            let traced_offset = WORLD.ner.1.unwrap().offset;
            let (ref mut spawned_heap, ref makespan) = WORLD.heaps.get_mut(&traced_heap).unwrap();
            if spawned_heap.is_none() {
                *spawned_heap = Some(Mapping::new(*makespan));
            }
            if let Some(m) = spawned_heap {
                res = m.base.add(WORLD.ner.1.unwrap().offset);
            }

            if let ReqType::ReAlloc(real_add, _) = r {
                if real_add != 0 {
                    WORLD.objects.remove(&real_add).expect("Tried to free non-existent object!");
                }
            }

            // Now the space is, in theory, allocated. Update objects.
            if let Some(_) = WORLD.objects.insert(res as usize, traced_heap + traced_offset) {
                graceful_exit("Unexpected object found in place!");
            };

            if let ReqType::Calloc(nobj, size) = r {
                std::ptr::write_bytes(res, 0, nobj * size);
            }

            if let ReqType::PosixMemAlign(real_memptr, _, _) = r {
                let start = real_memptr as *mut usize;
                *start = res as usize;
            }

        }
        // Don't forget to update what you expect next!
        WORLD.ner = Request::new_event(&mut WORLD.trace);
        while WORLD.ner.0.is_useless() { WORLD.ner = Request::new_event(&mut WORLD.trace); }
        // ...also inform potentially blocked threads.
        ITS_TIME.notify_all();
        
        return_ownership();
        res
    }
}