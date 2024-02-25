#![allow(dead_code)]

use pest::{error::Error, iterators::{Pair, Pairs}, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MirisaParser;

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedPrimitiveType {
	I8,
	U8,
	I16,
	U16,
	I32,
	U32,
	F32,
	I64,
	U64,
	F64,
	Usize,
	Char,
	Unit
}

impl From<&str> for ParsedPrimitiveType {
	fn from(source: &str) -> Self {
		match source {
			"i8" => Self::I8,
			"u8" => Self::U8,
			"i16" => Self::I16,
			"u16" => Self::U16,
			"i32" => Self::I32,
			"u32" => Self::U32,
			"f32" => Self::F32,
			"i64" => Self::I64,
			"u64" => Self::U64,
			"f64" => Self::F64,
			"usize" => Self::Usize,
			"char" => Self::Char,
			"unit" => Self::Unit,
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedPrimaryType<'a> {
	Primitive(ParsedPrimitiveType),
	Identifier(&'a str),
	Structure(Vec<(&'a str, ParsedType<'a>)>),
	Variants(Vec<&'a str>),
	Union(Vec<(&'a str, ParsedType<'a>)>)
}

impl<'a> From<Pair<'a, Rule>> for ParsedPrimaryType<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::primitive_type => Self::Primitive(ParsedPrimitiveType::from(source.as_str())),
			Rule::identifier => Self::Identifier(source.as_str()),
			Rule::structure_type => Self::Structure(source.into_inner().map(|entry| {
				let mut inner = entry.into_inner();
				let entry_name = inner.next().unwrap().as_str().trim();
				let entry_type = ParsedType::from(inner.next().unwrap().into_inner());
				(entry_name, entry_type)
			}).collect()),
			Rule::variants_type => Self::Variants(source.into_inner().map(|variant|
				variant.as_str().trim()
			).collect()),
			Rule::union_type => Self::Union(source.into_inner().map(|entry| {
				let mut inner = entry.into_inner();
				let entry_name = inner.next().unwrap().as_str().trim();
				let entry_type = ParsedType::from(inner.next().unwrap().into_inner());
				(entry_name, entry_type)
			}).collect()),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedTypeModifier {
	Pointer,
	PointerToConst,
	Array(Option<u64>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedType<'a> {
	PrimaryType(ParsedPrimaryType<'a>),
	WithModifier(ParsedTypeModifier, Box<ParsedType<'a>>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedType<'a> {
	fn from(source: Pairs<'a, Rule>) -> Self {
		let mut modifiers = Vec::new();
		for pair in source {
			match pair.as_rule() {
				Rule::pointer => modifiers.push(match pair.as_str().trim() {
					"*" => ParsedTypeModifier::Pointer,
					"*const" => ParsedTypeModifier::PointerToConst,
					_ => unreachable!()
				}),
				Rule::array_t => modifiers.push(ParsedTypeModifier::Array(pair.into_inner().next().and_then(|size| size.as_str().trim().parse().ok()))),
				Rule::primary_type => {
					let mut result = Self::PrimaryType(ParsedPrimaryType::from(pair.into_inner().next().unwrap()));
					for modifier in modifiers.into_iter().rev() {
						result = Self::WithModifier(modifier, Box::new(result));
					}
					return result;
				},
				_ => unreachable!()
			}
		}
		unreachable!()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedLiteral<'a> {
	Integer(i128),
	Float(f64),
	Char(i8),
	String(&'a str),
	Unit
}

impl<'a> From<Pair<'a, Rule>> for ParsedLiteral<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::integer => Self::Integer(source.as_str().trim().parse().unwrap()),
			Rule::float => Self::Float(source.as_str().trim().parse().unwrap()),
			Rule::char => Self::Char( match source.into_inner().next().unwrap().as_str().trim() {
				"\\'" => '\'' as i8,
				"\\\\" => '\\' as i8,
				"\\0" => '\0' as i8,
				"\\e" => '\x1b' as i8,
				"\\r" => '\r' as i8,
				"\\t" => '\t' as i8,
				"\\n" => '\n' as i8,
				x if x.len() == 4 && &x[0..2] == "\\x" => i8::from_str_radix(&x[2..4], 16).unwrap(),
				c => c.bytes().next().unwrap() as i8
			}),
			Rule::string => Self::String(source.into_inner().next().unwrap().as_str()),
			Rule::unit => Self::Unit,
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedPrimaryExpression<'a> {
	Literal(ParsedLiteral<'a>),
	Identifier(&'a str),
	Array(Vec<ParsedExpression<'a>>),
	Structure(Vec<(&'a str, ParsedExpression<'a>)>),
	Union(Vec<(&'a str, ParsedExpression<'a>)>),
	Expression(Box<ParsedExpression<'a>>)
}

impl<'a> From<Pair<'a, Rule>> for ParsedPrimaryExpression<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::literal => Self::Literal(ParsedLiteral::from(source.into_inner().next().unwrap())),
			Rule::identifier => Self::Identifier(source.as_str()),
			Rule::array => Self::Array(source.into_inner().map(|entry|
				ParsedExpression::from(entry.into_inner())
			).collect()),
			Rule::structure => Self::Structure(source.into_inner().map(|entry| {
				let mut inner = entry.into_inner();
				let entry_name = inner.next().unwrap().as_str().trim();
				let entry_type = ParsedExpression::from(inner.next().unwrap().into_inner());
				(entry_name, entry_type)
			}).collect()),
			Rule::union => Self::Union(source.into_inner().map(|entry| {
				let mut inner = entry.into_inner();
				let entry_name = inner.next().unwrap().as_str().trim();
				let entry_type = ParsedExpression::from(inner.next().unwrap().into_inner());
				(entry_name, entry_type)
			}).collect()),
			Rule::expression_level_or => Self::Expression(Box::new(ParsedExpression::from(source.into_inner()))),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionPostfix<'a> {
	Call(Vec<ParsedExpression<'a>>),
	Indexing(Box<ParsedExpression<'a>>),
	FieldAccess(&'a str)
}

impl<'a> From<Pair<'a, Rule>> for ParsedExpressionPostfix<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::call => Self::Call(source.into_inner().map(|argument| ParsedExpression::from(argument.into_inner())).collect()),
			Rule::indexing => Self::Indexing(Box::new(ParsedExpression::from(source.into_inner().next().unwrap().into_inner()))),
			Rule::field_access => Self::FieldAccess(source.into_inner().next().unwrap().as_str().trim()),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelBottom<'a> {
	PrimaryExpression(ParsedPrimaryExpression<'a>),
	Postfix(Box<ParsedExpressionLevelBottom<'a>>, ParsedExpressionPostfix<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelBottom<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::PrimaryExpression(ParsedPrimaryExpression::from(source.next().unwrap()));
		for postfix in source.map(|postfix| postfix.into_inner().next().unwrap()) {
			result = Self::Postfix(Box::new(result), ParsedExpressionPostfix::from(postfix));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedOpLevelPrefix {
	Neg,
	Ref
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelPrefix<'a> {
	LevelBottom(ParsedExpressionLevelBottom<'a>),
	Operator(ParsedOpLevelPrefix, Box<ParsedExpressionLevelPrefix<'a>>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelPrefix<'a> {
	fn from(source: Pairs<'a, Rule>) -> Self {
		let mut prefixes = Vec::new();
		for pair in source {
			match pair.as_rule() {
				Rule::op_level_prefix => prefixes.push(match pair.as_str().trim() {
					"not" => ParsedOpLevelPrefix::Neg,
					"&" => ParsedOpLevelPrefix::Ref,
					_ => unreachable!()
				}),
				Rule::expression_level_bottom => {
					let mut result = Self::LevelBottom(ParsedExpressionLevelBottom::from(pair.into_inner()));
					for op in prefixes.into_iter().rev() {
						result = Self::Operator(op, Box::new(result));
					}
					return result;
				},
				_ => unreachable!()
			}
		}
		unreachable!()
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelAs<'a> {
	LevelPrefix(ParsedExpressionLevelPrefix<'a>),
	Operator(Box<ParsedExpressionLevelAs<'a>>, ParsedType<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelAs<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelPrefix(ParsedExpressionLevelPrefix::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			result = Self::Operator(Box::new(result), ParsedType::from(source.nth(1).unwrap().into_inner()));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedOpLevelMul {
	Mul,
	Div,
	Rem
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelMul<'a> {
	LevelAs(ParsedExpressionLevelAs<'a>),
	Operator(ParsedOpLevelMul, Box<ParsedExpressionLevelMul<'a>>, ParsedExpressionLevelAs<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelMul<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelAs(ParsedExpressionLevelAs::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			let op = match source.next().unwrap().as_str().trim() {
				"*" => ParsedOpLevelMul::Mul,
				"/" => ParsedOpLevelMul::Div,
				"%" => ParsedOpLevelMul::Rem,
				_ => unreachable!()
			};
			result = Self::Operator(op, Box::new(result), ParsedExpressionLevelAs::from(source.next().unwrap().into_inner()));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedOpLevelAdd {
	Add,
	Sub
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelAdd<'a> {
	LevelMul(ParsedExpressionLevelMul<'a>),
	Operator(ParsedOpLevelAdd, Box<ParsedExpressionLevelAdd<'a>>, ParsedExpressionLevelMul<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelAdd<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelMul(ParsedExpressionLevelMul::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			let op = match source.next().unwrap().as_str().trim() {
				"+" => ParsedOpLevelAdd::Add,
				"-" => ParsedOpLevelAdd::Sub,
				_ => unreachable!()
			};
			result = Self::Operator(op, Box::new(result), ParsedExpressionLevelMul::from(source.next().unwrap().into_inner()));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedOpLevelComp {
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelComp<'a> {
	LevelAdd(ParsedExpressionLevelAdd<'a>),
	Operator(ParsedOpLevelComp, Box<ParsedExpressionLevelComp<'a>>, ParsedExpressionLevelAdd<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelComp<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelAdd(ParsedExpressionLevelAdd::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			let op = match source.next().unwrap().as_str().trim() {
				"=" => ParsedOpLevelComp::Eq,
				"/=" => ParsedOpLevelComp::Ne,
				"<" => ParsedOpLevelComp::Lt,
				"<=" => ParsedOpLevelComp::Le,
				">" => ParsedOpLevelComp::Gt,
				">=" => ParsedOpLevelComp::Ge,
				_ => unreachable!()
			};
			result = Self::Operator(op, Box::new(result), ParsedExpressionLevelAdd::from(source.next().unwrap().into_inner()));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelAnd<'a> {
	LevelComp(ParsedExpressionLevelComp<'a>),
	Operator(Box<ParsedExpressionLevelAnd<'a>>, ParsedExpressionLevelComp<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelAnd<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelComp(ParsedExpressionLevelComp::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			result = Self::Operator(Box::new(result), ParsedExpressionLevelComp::from(source.nth(1).unwrap().into_inner()));
		}
		result
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionLevelOr<'a> {
	LevelAnd(ParsedExpressionLevelAnd<'a>),
	Operator(Box<ParsedExpressionLevelOr<'a>>, ParsedExpressionLevelAnd<'a>)
}

impl<'a> From<Pairs<'a, Rule>> for ParsedExpressionLevelOr<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let mut result = Self::LevelAnd(ParsedExpressionLevelAnd::from(source.next().unwrap().into_inner()));
		while source.len() != 0 {
			result = Self::Operator(Box::new(result), ParsedExpressionLevelAnd::from(source.nth(1).unwrap().into_inner()));
		}
		result
	}
}

type ParsedExpression<'a> = ParsedExpressionLevelOr<'a>;

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedDeclaration<'a> {
	pub(crate) is_const: bool,
	pub(crate) name: &'a str,
	pub(crate) declaration_type: ParsedType<'a>,
	pub(crate) value: Option<ParsedExpression<'a>>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedDeclaration<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let is_const = match source.next().unwrap().as_str().trim() {
			"const" => true,
			"var" => false,
			_ => unreachable!()
		};
		let name = source.next().unwrap().as_str().trim();
		let declaration_type = ParsedType::from(source.next().unwrap().into_inner());
		let value = source.next().map(Pair::into_inner).map(ParsedExpression::from);
		Self { is_const, name, declaration_type, value }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTypeDeclaration<'a> {
	pub(crate) name: &'a str,
	pub(crate) declaration_type: ParsedType<'a>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedTypeDeclaration<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let name = source.next().unwrap().as_str().trim();
		let declaration_type = ParsedType::from(source.next().unwrap().into_inner());
		Self { name, declaration_type }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedLexpression<'a> {
	Identifier(&'a str),
	Dereference(Box<ParsedLexpression<'a>>),
	Indexing(Box<ParsedLexpression<'a>>, Box<ParsedExpression<'a>>),
	FieldAccess(Box<ParsedLexpression<'a>>, &'a str)
}

impl<'a> From<Pair<'a, Rule>> for ParsedLexpression<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::lexpression_inner => {
				let mut inner = source.into_inner();
				let mut result = Self::Identifier(inner.next().unwrap().as_str().trim());
				for postfix in inner {
					result = match postfix.as_rule() {
						Rule::dereference => Self::Dereference(Box::new(result)),
						Rule::indexing => Self::Indexing(Box::new(result), Box::new(ParsedExpression::from(postfix.into_inner()))),
						Rule::field_access => Self::FieldAccess(Box::new(result), postfix.as_str().trim()),
						_ => unreachable!()
					};
				}
				result
			},
			Rule::lexpression => ParsedLexpression::from(source.into_inner().next().unwrap()),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedReassignmentOp {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	None
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedReassignment<'a> {
	pub(crate) lexpression: ParsedLexpression<'a>,
	pub(crate) reassignment_op: ParsedReassignmentOp,
	pub(crate) value: ParsedExpression<'a>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedReassignment<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let lexpression = ParsedLexpression::from(source.next().unwrap().into_inner().next().unwrap());
		let reassignment_op = match source.next().unwrap().as_str().trim() {
			"+=" => ParsedReassignmentOp::Add,
			"-=" => ParsedReassignmentOp::Sub,
			"*=" => ParsedReassignmentOp::Mul,
			"/=" => ParsedReassignmentOp::Div,
			"%=" => ParsedReassignmentOp::Rem,
			":=" => ParsedReassignmentOp::None,
			_ => unreachable!()
		};
		let value = ParsedExpression::from(source.next().unwrap().into_inner());
		Self { lexpression, reassignment_op, value }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedIf<'a> {
	pub(crate) condition: ParsedExpression<'a>,
	pub(crate) then: Vec<ParsedStatement<'a>>,
	pub(crate) elseifs: Vec<(ParsedExpression<'a>, Vec<ParsedStatement<'a>>)>,
	pub(crate) else_body: Option<Vec<ParsedStatement<'a>>>,
}

impl<'a> From<Pairs<'a, Rule>> for ParsedIf<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let condition = ParsedExpression::from(source.next().unwrap().into_inner());
		let then = source.next().unwrap().into_inner().map(|statement|
			ParsedStatement::from(statement.into_inner().next().unwrap().into_inner().next().unwrap())
		).collect();
		let mut elseifs = Vec::new();
		for branch in source {
			match branch.as_rule() {
				Rule::if_elseif => {
					let mut inner = branch.into_inner();
					let expression = ParsedExpression::from(inner.next().unwrap().into_inner());
					let body = inner.next().unwrap().into_inner().map(|statement| ParsedStatement::from(statement.into_inner().next().unwrap())).collect();
					elseifs.push((expression, body))
				},
				Rule::if_else => return Self { condition, then, elseifs, else_body: Some(
					branch.into_inner().next().unwrap().into_inner().map(|statement| ParsedStatement::from(statement.into_inner().next().unwrap())).collect()
				) },
				_ => unreachable!()
			};
		}
		let elseifs = elseifs;
		Self { condition, then, elseifs, else_body: None }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedWhile<'a> {
	pub(crate) condition: ParsedExpression<'a>,
	pub(crate) body: Vec<ParsedStatement<'a>>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedWhile<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let condition = ParsedExpression::from(source.next().unwrap().into_inner());
		let body = source.next().unwrap().into_inner().map(|statement|
			ParsedStatement::from(statement.into_inner().next().unwrap())
		).collect();
		Self { condition, body }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedDoWhile<'a> {
	pub(crate) body: Vec<ParsedStatement<'a>>,
	pub(crate) condition: ParsedExpression<'a>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedDoWhile<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let body = source.next().unwrap().into_inner().map(|statement|
			ParsedStatement::from(statement.into_inner().next().unwrap())
		).collect();
		let condition = ParsedExpression::from(source.next().unwrap().into_inner());
		Self { body, condition }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedOperationHandler<'a> {
	pub(crate) operation: &'a str,
	pub(crate) arguments: Vec<&'a str>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

impl<'a> From<Pairs<'a, Rule>> for ParsedOperationHandler<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let operation = source.next().unwrap().as_str().trim();
		let arguments = source.next().unwrap().into_inner().map(|handler|
			handler.as_str().trim()
		).collect();
		let body = source.next().unwrap().into_inner().map(|handler|
			ParsedStatement::from(handler.into_inner().next().unwrap())
		).collect();
		Self { operation, arguments, body }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedHandler<'a> {
	pub(crate) effect: &'a str,
	pub(crate) handlers: Vec<ParsedOperationHandler<'a>>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedHandler<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let effect = source.next().unwrap().as_str().trim();
		let handlers = source.map(|handler|
			ParsedOperationHandler::from(handler.into_inner())
		).collect();
		Self { effect, handlers }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTry<'a> {
	pub(crate) body: Vec<ParsedStatement<'a>>,
	pub(crate) handlers: Vec<ParsedHandler<'a>>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedTry<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let body = source.next().unwrap().into_inner().map(|statement|
			ParsedStatement::from(statement.into_inner().next().unwrap())
		).collect();
		let handlers = source.map(|handler|
			ParsedHandler::from(handler.into_inner())
		).collect();
		Self { body, handlers }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedStatement<'a> {
	Declaration(ParsedDeclaration<'a>),
	TypeDeclaration(ParsedTypeDeclaration<'a>),
 	Reassignment(ParsedReassignment<'a>),
 	If(ParsedIf<'a>),
 	While(ParsedWhile<'a>),
	DoWhile(ParsedDoWhile<'a>),
 	Try(ParsedTry<'a>),
 	Block(Vec<ParsedStatement<'a>>),
 	Expression(ParsedExpression<'a>),
 	Return(ParsedExpression<'a>),
 	Break,
 	Continue
}

impl<'a> From<Pair<'a, Rule>> for ParsedStatement<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::declaration => Self::Declaration(ParsedDeclaration::from(source.into_inner())),
			Rule::type_declaration => Self::TypeDeclaration(ParsedTypeDeclaration::from(source.into_inner())),
			Rule::reassignment => Self::Reassignment(ParsedReassignment::from(source.into_inner())),
			Rule::mirisa_if => Self::If(ParsedIf::from(source.into_inner())),
			Rule::mirisa_while => Self::While(ParsedWhile::from(source.into_inner())),
			Rule::do_while => Self::DoWhile(ParsedDoWhile::from(source.into_inner())),
			Rule::mirisa_try => Self::Try(ParsedTry::from(source.into_inner())),
			Rule::block => Self::Block(source.into_inner().next().unwrap().into_inner().map(|statement|
				ParsedStatement::from(statement.into_inner().next().unwrap())
			).collect()),
			Rule::expression_level_or => Self::Expression(ParsedExpression::from(source.into_inner())),
			Rule::mirisa_return => Self::Return(ParsedExpression::from(source.into_inner().next().unwrap().into_inner())),
			Rule::mirisa_break => Self::Break,
			Rule::mirisa_continue => Self::Continue,
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedOperation<'a> {
	pub(crate) name: &'a str,
	pub(crate) provides_types: Vec<ParsedType<'a>>,
	pub(crate) resume_type: ParsedType<'a>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedOperation<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let name = source.next().unwrap().as_str().trim();
		let provides_types = source.next().unwrap().into_inner().map(|t|
			ParsedType::from(t.into_inner())
		).collect();
		let resume_type = ParsedType::from(source.next().unwrap().into_inner());
		Self { name, provides_types, resume_type }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedEffect<'a> {
	pub(crate) name: &'a str,
	pub(crate) operations: Vec<ParsedOperation<'a>>,
}

impl<'a> From<Pairs<'a, Rule>> for ParsedEffect<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let name = source.next().unwrap().as_str().trim();
		let operations = source.map(|operation|
			ParsedOperation::from(operation.into_inner())
		).collect();
		Self { name, operations }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFunction<'a> {
	name: &'a str,
	arguments: Vec<(&'a str, ParsedType<'a>)>,
	return_type: ParsedType<'a>,
	effect_set: Vec<&'a str>,
	body: Vec<ParsedStatement<'a>>
}

impl<'a> From<Pairs<'a, Rule>> for ParsedFunction<'a> {
	fn from(mut source: Pairs<'a, Rule>) -> Self {
		let name = source.next().unwrap().as_str().trim();
		let mut arguments = Vec::new();
		for mut argument in source.next().unwrap().into_inner().map(Pair::into_inner) {
			let arg_name = argument.next().unwrap().as_str().trim();
			let arg_type = ParsedType::from(argument.next().unwrap().into_inner());
			arguments.push((arg_name, arg_type));
		}
		let arguments = arguments;
		let return_type = ParsedType::from(source.next().unwrap().into_inner());
		let effect_set = source.next().unwrap().into_inner().map(|effect| effect.as_str()).collect();
		let body = source.next().unwrap().into_inner().map(|statement| ParsedStatement::from(statement.into_inner().next().unwrap())).collect();
		Self { name, arguments, return_type, effect_set, body }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedItem<'a> {
	Include(&'a str),
	Declaration(ParsedDeclaration<'a>),
	TypeDeclaration(ParsedTypeDeclaration<'a>),
	Effect(ParsedEffect<'a>),
	Function(ParsedFunction<'a>),
}

impl<'a> From<Pair<'a, Rule>> for ParsedItem<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::include => Self::Include(source.into_inner().next().unwrap().as_str()),
			Rule::declaration => Self::Declaration(ParsedDeclaration::from(source.into_inner())),
			Rule::type_declaration => Self::TypeDeclaration(ParsedTypeDeclaration::from(source.into_inner())),
			Rule::effect => Self::Effect(ParsedEffect::from(source.into_inner())),
			Rule::function => Self::Function(ParsedFunction::from(source.into_inner())),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedProgram<'a>(pub(crate) Vec<ParsedItem<'a>>);

impl<'a> TryFrom<&'a str> for ParsedProgram<'a> {
	type Error = Error<Rule>;

	fn try_from(source: &'a str) -> Result<Self, Self::Error> {
		let parsed = MirisaParser::parse(Rule::program, source)?;
		let mut parsed_items = Vec::new();
		for item in parsed {
			parsed_items.push(ParsedItem::from(item));
		}
		Ok(Self(parsed_items))
	}
}
