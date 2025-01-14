use crate::logger::*;
use crate::ffi::*;
use std::fs::File;
use std::io::prelude::*;

#[link(name = "c")]
extern {
    pub fn malloc_usable_size(ptr: *mut void) -> size_t;
}

// To be used for mapping assignment. It is important to renew the
// byte stream via `rewind` each time we run the assignment process,
// thus a BufReader would be pointless.
static mut PROC_FILE: Lazy<File> = Lazy::new(|| { 
    let id = std::process::id();
    let path = format!("/proc/{}/maps", id);

    File::open(path).expect("[SERVER]: Failed to open maps file.")
});

// Multiple threads could be accessing the /proc file (although our use
// of the cheat sheet minimizes this probability). In any case, we better
// protect it with a mutex.
static PROC_FD_MTX: Mutex<()> = Mutex::new(());

// Address size is 64 bits. Needs 16 hex characters to be represented.
// Each character is encoded in an ASCII byte. Read one more for the
// expected `-` character at the end of floor address, or the whitespace
// at the end of the ceil. Smaller addresses have been witnessed though.
const BUF_LEN: usize = 8192;

// Informs what phase of scanning we're in, and how deep in the buffer
// we are.
#[derive(Clone, Copy)]
enum ScanState {
    ScanningFloor(usize),
    ScanningCeil(usize),
    ScanningGarbage(usize)
}

impl ScanState {
    #[inline(always)]
    fn scan_non_garbage(is_floor: bool, idx: &mut usize, buf: &mut [u8; BUF_LEN]) -> Endpoint {
        let mut new_ep = Endpoint::new();
        let mut next_char: u8;
        'outer:
        loop {
            while *idx < BUF_LEN {
                next_char = buf[*idx];
                // Floor scanning ends at '-' (0x2D in ASCII),
                // ceil at ' ' (0x20 in ASCII),
                // garbage at '\n' (0x0A in ASCII)
                if (is_floor && next_char != 0x2Du8) || (!is_floor && next_char != 0x20u8) {
                    new_ep.buffer[new_ep.length as usize] = next_char;
                    new_ep.length += 1;
                    *idx += 1;
                    continue;
                };
                // We reach this point when the dash has been scanned.
                *idx += 1;
                break 'outer new_ep;
            };
            // We reach this point when scanned buffer has been exhausted
            // yet no dash has been reached yet. A new read must be made.
            unsafe { PROC_FILE.read(buf).expect("[SERVER]: Failed to read proc file.") };
            *idx = 0;
        }
    }

    // Makes a transition from one scanning state to another. May or
    // may not yield a new endpoint.
    #[inline(always)]
    fn update(&mut self, buf: &mut [u8; BUF_LEN]) -> Option<Endpoint> {
        match self {
            ScanState::ScanningFloor(ref mut idx)   => {
                let new_ep = Self::scan_non_garbage(true, idx, buf);
                *self = ScanState::ScanningCeil(*idx);
                Some(new_ep)
            },
            ScanState::ScanningCeil(ref mut idx)    => {
                let new_ep = Self::scan_non_garbage(false, idx, buf);
                *self = ScanState::ScanningGarbage(*idx);
                Some(new_ep)
            },
            ScanState::ScanningGarbage(ref mut idx) => {
                // Code could be less duplicated in future version.
                let mut next_char: u8;
                'outer:
                loop {
                    while *idx < BUF_LEN {
                        next_char = buf[*idx];
                        // The only difference w/ scan_non_garbage is that we check for line feed
                        // (0x0A in ASCII) and we don't create new endpoints along the way.
                        if next_char != 0x0Au8 {
                            *idx += 1;
                            continue;
                        };
                        *idx += 1;
                        break 'outer;
                    };
                    unsafe { PROC_FILE.read(buf).expect("[SERVER]: Failed to read proc file.") };
                    *idx = 0;
                };
                *self = ScanState::ScanningFloor(*idx);

                None
            }
        }
    }
}

#[derive(Clone, Copy)]
struct Endpoint {
    buffer: [u8; 256],
    length: u8
}

impl Endpoint {
    fn new() -> Endpoint {
        Endpoint { buffer: [0; 256], length: 0 }
    }

    fn to_usize(&self) -> usize {
        let mut exp = self.length as u32;
        let mut idx: usize = 0;
        let mut res: usize = 0;
        while idx <= (self.length - 1) as usize {
            exp -= 1;
            res += (char::from(self.buffer[idx])
                    .to_digit(16)
                    .expect("[SERVER]: Failure upon byte-digit conversion") 
                    as usize) * 16usize.pow(exp);
            idx += 1;
        }

        res
    }
}

impl PartialEq for Endpoint {
    fn eq(&self, other: &Self) -> bool {
        if self.length != other.length { false }
        else {
            let mut runner = 0;
            while runner < self.length as usize {
                if self.buffer[runner] != other.buffer[runner] { return false; }
                runner += 1;
            }

            true
        }
    }
}

#[derive(Clone, Copy)]
enum MapIDState {
    Started,
    Ongoing,
    Complete
}

#[derive(Clone, Copy)]
struct MapFindState {
    scan_buffer: [u8; BUF_LEN],
    old_floor: Endpoint,
    current_floor: Endpoint,
    current_ceil: Endpoint,
    id_state: MapIDState,
    scan_state: ScanState,
}

impl MapFindState {
    fn new() -> Self {
        let mut buf: [u8; BUF_LEN] = [0; BUF_LEN];
        unsafe { PROC_FILE.read(&mut buf).expect("[SERVER]: Failed to read proc file.") };
        MapFindState {
            scan_buffer: buf, 
            id_state: MapIDState::Started, 
            scan_state: ScanState::ScanningFloor(0), 
            old_floor: Endpoint::new(),
            current_floor: Endpoint::new(),
            current_ceil: Endpoint::new(),
        }            
    }

    fn find(&mut self, add: usize, size: usize) -> (usize, usize) {
        loop {
            match self.id_state {
                MapIDState::Started     => {
                    self.current_floor = self.scan_state.update(&mut self.scan_buffer).unwrap();
                    self.current_ceil = self.scan_state.update(&mut self.scan_buffer).unwrap();
                    // We want to remember the current floor, in case next line is contiguous.
                    self.old_floor = self.current_floor;
                    self.scan_state.update(&mut self.scan_buffer);
                    self.id_state = MapIDState::Ongoing;
                },
                MapIDState::Ongoing     => {
                    let test_floor = self.scan_state.update(&mut self.scan_buffer).unwrap();
                    if test_floor == self.current_ceil {
                        self.current_ceil = self.scan_state.update(&mut self.scan_buffer).unwrap();
                        self.scan_state.update(&mut self.scan_buffer);
                    } else {
                        self.old_floor = test_floor;
                        self.id_state = MapIDState::Complete;
                    }
                },
                MapIDState::Complete    => {
                    let cand_map = self.current_floor.to_usize();
                    let cand_map_end = self.current_ceil.to_usize();
                    if add >= cand_map && add + size - 1 <= cand_map_end {
                        break (cand_map, cand_map_end);
                    } else {
                        self.current_floor = self.old_floor;
                        self.current_ceil = self.scan_state.update(&mut self.scan_buffer).unwrap();
                        self.scan_state.update(&mut self.scan_buffer);
                        self.id_state = MapIDState::Ongoing;
                    }
                }
            }
        }
    }
}

pub fn find_mapping(add: *mut void) -> (usize, usize) {
    // According to /proc docs, rewinding the FD of /proc/maps essentially
    // renews the file's contents. Rewinding is very expensive though, as is
    // the overall find mechanism. We opportunistically save the last computed
    // endpoints, in case they match the next request. Only if they don't do
    // we trigger the find mechanism.
    static mut CHEAT_SHEET: [((usize, usize), usize); MAX_THREADS_NUM] = [((0, 0), 0); MAX_THREADS_NUM];
    static mut CHEAT_IDX: usize = 0;

    unsafe {
        let size = malloc_usable_size(add);
        let mut runner = 0;
        let mut min_idx = runner;
        let mut min_val = usize::MAX;
        while runner < CHEAT_IDX {
            let test_floor = CHEAT_SHEET[runner].0.0;
            if CHEAT_SHEET[runner].1 < min_val && CHEAT_IDX == MAX_THREADS_NUM {
                // Check for minimum hits, in case something
                // has to be replaced (but only if array is full).
                min_val = CHEAT_SHEET[runner].1;
                min_idx = runner;
            }
            if add as usize >= test_floor && add as usize + size - 1 <= CHEAT_SHEET[runner].0.1 {
                // We had a hit!
                CHEAT_SHEET[runner].1 += 1;
                return (test_floor, size);
            }
            runner += 1;
        }

        // We didn't have a hit! Time to update...
        disable_logging();
        let lock = PROC_FD_MTX.lock().unwrap();
        PROC_FILE.rewind().expect("[SERVER]: Failed to rewind proc file.");
        let (new_floor, new_ceil) = MapFindState::new()
                                    .find(add as usize, size);
        drop(lock);
        enable_logging();
        if CHEAT_IDX < MAX_THREADS_NUM {
            CHEAT_SHEET[CHEAT_IDX] = ((new_floor, new_ceil), 0);
            CHEAT_IDX += 1;
        } else {
            CHEAT_SHEET[min_idx] = ((new_floor, new_ceil), 0);
        };
        
        (new_floor, size)
    }
}