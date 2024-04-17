use crate::elements::{AddrSet, Job, ReqType, Request};
pub use crate::utils::{Error, Result};
use std::env;
pub use std::fs::File;
pub use std::io::{BufReader, BufWriter, Read, Write};
use std::rc::Rc;
use crate::mcts::MCTSPolicy;

pub struct Config {
    pub maybe_trc:  Option<BufReader<File>>,
    pub plc:        BufReader<File>,
    pub diss:       bool,
    pub trials:     usize,
    pub mcts:       MCTSPolicy,
}

pub fn parse_args() -> Result<Config> {
    let mut diss = false;
    let mut trials = 10_000;
    let mut mcts = MCTSPolicy::UCT(2.0_f64.sqrt());
    let mut requests_trace = None;
    let mut inp: Option<BufReader<File>> = None;
    for (n, arg) in env::args().skip(1).enumerate() {
        if n > 5 {
            return Err(Error::msg("Bad arguments."));
        } else if let Ok(t) = usize::from_str_radix(&arg, 10) {
            trials = t;
        } else if arg.contains("MCTS") {
            let components: Vec<&str> = arg.split(':').collect();
            if components.len() != 2 {
                panic!("USAGE: coreba [--diss] [trials] [MCTS:{{RAND,UCT(C),RAVE,UCTRAVE(C,k),GRAVE}}] [TRC FILE] PLC FILE");
            }
            match components[1] {
                "RAND"  => { mcts = MCTSPolicy::Madman },
                "RAVE"  => { mcts = MCTSPolicy::RAVE; },
                "GRAVE" => { mcts = MCTSPolicy::GRAVE; },
                _   => {
                    if components[1].contains("RAVE") {
                        let inner: Vec<&str> = components[1][8..].split(',').collect();
                        if inner.len() != 2 {
                            panic!("Bad policy formatting.");
                        } else {
                            if let Ok(c) = inner[0].parse::<f64>() {
                                if let Ok(k) = inner[1].split(')').next().unwrap().parse::<f64>() {
                                    mcts = MCTSPolicy::UCTRAVE(c, k);
                                } else {
                                    panic!("Bad policy formatting.");
                                }
                            } else {
                                panic!("Bad policy formatting.");
                            }
                        }
                    } else if components[1].contains("UCT") {
                        if let Ok(c) = components[1][4..].split(')').next().unwrap().parse::<f64>() {
                            mcts = MCTSPolicy::UCT(c)
                        }
                    } else {
                        panic!("Bad policy formatting.");
                    }
                }
            }
        } else if arg == "--diss" {
            diss = true;
        } else if arg.contains(".trc") {
            requests_trace = Some(BufReader::new(File::open(arg)?));
        } else if !arg.contains(".plc") {
            panic!("A PLC file must be given.");
        } else {
            inp = Some(BufReader::new(File::open(arg)?));
        }
    }

    Ok(
        Config {
            maybe_trc:  requests_trace,
            plc:        inp.unwrap(),
            diss,
            trials,
            mcts
        }
    )
}

pub fn update_plc(w: &mut BufWriter<File>, j: &Rc<Job>, ba: &AddrSet) -> Result<()> {
    let mut buff: Vec<u8> = vec![];

    let args = get_args(j, ba);
    for arg in args {
        let arg_bytes = arg.to_be_bytes();
        for b in &arg_bytes {
            buff.push(*b);
        }
    }

    w.write(buff.as_slice()).unwrap();

    Ok(())
}

pub fn update_trace(
    w: &mut BufWriter<File>,
    r: Request,
    j: Option<(Rc<Job>, Option<Rc<Job>>)>,
    ba: &AddrSet,
) -> Result<()> {
    let mut buff: Vec<u8> = vec![];
    buff.push(r.rtype.get_sentinel());

    let args = get_trc_args(r, j.unwrap(), ba);

    for arg in args {
        let arg_bytes = arg.to_be_bytes();
        for b in &arg_bytes {
            buff.push(*b);
        }
    }

    w.write(buff.as_slice())?;
    Ok(())
}

fn get_trc_args(r: Request, j: (Rc<Job>, Option<Rc<Job>>), ba: &AddrSet) -> Vec<usize> {
    let mut res: Vec<usize> = vec![];

    match r.rtype {
        ReqType::Malloc(s) => {
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::Free(p) => {
            if p != 0 {
                res.push(*ba.get(&j.0.id()).expect("Bad addr2"));
            } else {
                res.push(0);
            }
            res.push(r.tid);
        }
        ReqType::Calloc(nobj, s) => {
            res.push(nobj);
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::ReAlloc(p, s) => {
            if p != 0 {
                res.push(*ba.get(&j.1.unwrap().id()).expect("Bad addr2"));
            } else {
                res.push(0);
            }
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::AlignedAlloc(a, s) => {
            res.push(a);
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::MemAlign(a, s) => {
            res.push(a);
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::PosixMemAlign(_ptr, a, s) => {
            res.push(*ba.get(&j.1.unwrap().id()).expect("Bad addr2"));
            res.push(a);
            res.push(s);
            res.push(*ba.get(&j.0.id()).expect("Bad addr"));
            res.push(j.0.original_size());
            res.push(r.tid);
            res.push(j.0.home.get().heap);
        }
        ReqType::Done => {
            panic!("No dones allowed.");
        }
    }

    res
}

fn get_args(j: &Rc<Job>, ba: &AddrSet) -> Vec<usize> {
    let (birth, death) = j.endpoints();
    vec![
        j.id(),
        birth,
        death,
        j.original_size(),
        j.home.get().heap,
        *ba.get(&j.id()).unwrap() - j.home.get().heap,
        j.home.get().alignment,
        j.home.get().req_size,
    ]
}
