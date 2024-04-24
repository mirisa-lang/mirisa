use std::collections::HashSet;
use crate::typed_ast::{TypedExpression, TypedExpressionInner, TypedProgram, TypedStatement, TypedStatementInner, TypedType};

fn codegen_type(r#type: TypedType) -> String {
	match r#type {
		TypedType::U8 => "u8".to_string(),
		TypedType::I32 => "i32".to_string(),
		TypedType::Unit => "void".to_string(),
		TypedType::Array(is_mut, size, inner_type) => {
			let is_mut_str = match is_mut {
				false => "const ",
				true => ""
			};
			match size {
				Some(n) => format!("[{n}]{is_mut_str}{}", codegen_type(*inner_type)),
				None => format!("[]{is_mut_str}{}", codegen_type(*inner_type))
			}
		},
		_ => todo!("{:?}", r#type)
	}
}

fn codegen_expression(expression: TypedExpression) -> String {
	match expression.inner {
		TypedExpressionInner::Integer(n) => n.to_string(),
		TypedExpressionInner::String(s) => format!(r#""{s}""#),
		TypedExpressionInner::Identifier(id) => id.to_string(),
		TypedExpressionInner::Call(function, arguments) => {
			let mut out = String::new();
			out.push_str(&codegen_expression(*function));
			out.push_str("(");
			let len = arguments.len();
			for (i, argument) in arguments.into_iter().enumerate() {
				out.push_str(&codegen_expression(argument));
				if i < len - 1 {
					out.push_str(",");
				}
			}
			out.push_str(")");
			out
		},
		_ => todo!("{:?}", expression)
	}
}

fn codegen_statement(statement: TypedStatement) -> String {
	match statement.inner {
		TypedStatementInner::Expression(expression) => format!("{};", codegen_expression(expression)),
		TypedStatementInner::Return(expression) => format!("return {};", codegen_expression(expression)),
		_ => todo!("{:?}", statement)
	}
}

pub fn codegen(mut program: TypedProgram) -> String {
	let mut out = r#"fn print_char(char:u8)void{@import("std").debug.print("{}",.{char});}fn print(str:[]const u8)void{@import("std").debug.print("{s}",.{str});}fn println(str:[]const u8)void{@import("std").debug.print("{s}\n",.{str});}

"#.to_string();
	for declaration in program.declarations {
		out.push_str(match declaration.is_mut {
			false => "const ",
			true => "var "
		});
		out.push_str(declaration.name);
		out.push_str(":");
		out.push_str(&codegen_type(declaration.declaration_type));
		out.push_str("=");
		if let Some(value) = declaration.value {
			out.push_str(&codegen_expression(value));
		}
		out.push_str(";");
	}
	let base_effects = HashSet::from_iter(["consoleIo"]);
	for function in program.functions {
		if !function.effect_set.is_subset(&base_effects) {
			todo!("effects aren't implemented yet");
		}
		out.push_str("pub fn ");
		out.push_str(function.name);
		out.push_str("(");
		let len = function.arguments.len();
		for (i, (is_mut, name, r#type)) in function.arguments.into_iter().enumerate() {
			if !is_mut {
				out.push_str("const ");
			}
			out.push_str(name);
			out.push_str(":");
			out.push_str(&codegen_type(r#type));
			if i < len - 1 {
				out.push_str(",");
			}
		}
		out.push_str(")");
		out.push_str(&codegen_type(function.return_type));
		out.push_str("{");
		for statement in function.body {
			out.push_str(&codegen_statement(statement));
		}
		out.push_str("}");
	}
	out.push_str("\n");
	out
}
