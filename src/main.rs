mod parser;
mod ast;
mod typed_ast;
mod codegen;

use std::io::{self, Read, stdin};

fn main() -> io::Result<()> {
    let mut source = String::new();
    stdin().read_to_string(&mut source)?;
    match parser::ParsedProgram::try_from(source.as_str()) {
        Ok(cst) => match typed_ast::TypedProgram::try_from(ast::AstProgram::from(cst)) {
            Ok(program) => println!("{}", codegen::codegen(program)),
            Err(error) => eprintln!("{:#?}", error)
        },
        Err(error) => eprintln!("{}", error)
    };
    Ok(())
}
