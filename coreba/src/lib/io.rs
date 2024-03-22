pub use std::io::{BufWriter, BufReader, Read, Write};
pub use std::fs::File;
use std::env;
use std::rc::Rc;
pub use crate::utils::{Result, Error};
use crate::elements::{AddrSet, Job, LiveSet, ReqType, Request};

pub fn parse_args() -> Result<(BufReader<File>, bool, Option<BufReader<File>>)> {
    let mut diss = false;
    let mut requests_trace = None;
    let mut inp: Option<BufReader<File>> = None;
    for (n, arg) in env::args()
                                    .skip(1)
                                    .enumerate() {
        if n > 2 {
            return Err(Error::msg("Bad arguments."));
        } else if arg == "--diss" {
            diss = true;
        } else if arg.contains(".trc") {
            requests_trace = Some(BufReader::new(File::open(arg)?));
        } else if !arg.contains(".plc") {
            panic!("A PLC file must be given.");
        } else {
            inp = Some(BufReader::new(File::open(arg)?));
        }
    };

    Ok((inp.unwrap(), diss, requests_trace))
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

pub fn update_trace(w: &mut BufWriter<File>, r: &Request, j: &Rc<Job>, b: &mut LiveSet, ba: &AddrSet, id: &mut usize) -> Result<()> {
    let mut buff: Vec<u8> = vec![];
    buff.push(r.rtype.get_sentinel());

    let args = get_trc_args(r, j, b, ba, id);

    for arg in args {
        let arg_bytes = arg.to_be_bytes();
        for b in &arg_bytes {
            buff.push(*b);
        }
    }

    w.write(buff.as_slice())?;
    Ok(())
}

fn get_trc_args(r: &Request, j: &Rc<Job>, b: &mut LiveSet, ba: &AddrSet, id: &mut usize) -> Vec<usize> {
    let mut res: Vec<usize> = vec![];

    match r.rtype {
        ReqType::Malloc(s)  => {
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::Free(p)  => {
            if p != 0 {
                res.push(*ba.get(&b.remove(&p).expect("lala").id()).expect("Bad addr2"));
            } else { res.push(0); }
            res.push(r.tid);
        },
        ReqType::Calloc(nobj, s)  => {
            res.push(nobj);
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::ReAlloc(p, s)  => {
            if p != 0 {
                res.push(*ba.get(&b.remove(&p).expect("lala").id()).expect("Bad addr2"));
            } else { res.push(0); }
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::AlignedAlloc(a, s)  => {
            res.push(a);
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::MemAlign(a, s)  => {
            res.push(a);
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::PosixMemAlign(ptr, a, s)  => {
            res.push(*ba.get(&b.remove(&ptr).expect("lala").id()).expect("Bad addr2"));
            res.push(a);
            res.push(s);
            res.push(*ba.get(id).expect("Bad addr"));
            j.id.set(*id);
            *id += 1;
            res.push(j.original_size());
            res.push(r.tid);
            res.push(j.home.get().heap);
            if let Some(_) = b.insert(j.origin.get(), j.clone()) {
                panic!("bababa");
            }
        },
        ReqType::Done   => { panic!("No dones allowed."); }
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
        j.home.get().req_size
    ]
}