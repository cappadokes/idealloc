use coreba::{elements, io};

fn main() {
    match io::parse_args() {
        Ok(config) => {
            let mut wrld = elements::World::new(&config);
            match wrld.populate(config) {
                Ok(_) => {
                    wrld.optimize();
                }
                Err(msg) => panic!("{:?}", msg),
            }
        }
        Err(msg) => {
            panic!("{:?}", msg);
        }
    }
}
