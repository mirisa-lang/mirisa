mod parser;

use std::io::{self, Read, stdin};

fn main() -> io::Result<()> {
    let mut source = String::new();
    stdin().read_to_string(&mut source)?;
    println!("{:#?}", parser::ParsedProgram::try_from(source.as_str()));
    Ok(())
}
