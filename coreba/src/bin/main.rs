use coreba::{io, elements};

fn main() {
    match io::parse_args() {
        Ok((plc, diss, maybe_trc)) => {
            let mut wrld = elements::World::new();
            match wrld.populate(plc, diss, maybe_trc) {
                Ok(_)  => { 
                    wrld.optimize();
                },
                Err(msg)    => panic!("{:?}", msg)
            }
        },
        Err(msg)    => {
            panic!("{:?}", msg);
        }
    }
}
