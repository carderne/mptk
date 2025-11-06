use crate::data::Entry;

pub fn print_entries(entries: &Vec<Entry>, verbose: bool) {
    for d in entries {
        if verbose {
            println!("{:?}", d)
        } else {
            println!("{}", d)
        }
    }
}
