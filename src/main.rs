mod parser;

use std::io::{self, Read, stdin};

fn main() -> io::Result<()> {
    let mut source = String::new();
    stdin().read_to_string(&mut source)?;
    match parser::ParsedProgram::try_from(source.as_str()) {
        Ok(cst) => println!("{:#?}", cst),
        Err(error) => eprintln!("{}", error)
    };
    Ok(())
}
