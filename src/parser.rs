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
	Structure(Vec<(&'a str, ParsedExpression<'a>)>),
	Union(&'a str, Box<ParsedExpression<'a>>)
}

impl<'a> From<Pair<'a, Rule>> for ParsedPrimaryType<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::primitive_type => Self::Primitive(ParsedPrimitiveType::from(source.as_str())),
			Rule::identifier => Self::Identifier(source.as_str()),
			_ => todo!("ParsedPrimaryType: {:?}", source)
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
				Rule::pointer => modifiers.push(match pair.as_str() {
					"*" => ParsedTypeModifier::Pointer,
					"*const" => ParsedTypeModifier::PointerToConst,
					_ => unreachable!()
				}),
				Rule::array_t => todo!("ParsedType: {:?}", pair),
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
	String(&'a str)
}

impl<'a> From<Pair<'a, Rule>> for ParsedLiteral<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::integer => Self::Integer(source.as_str().parse().unwrap()),
			Rule::float => Self::Float(source.as_str().parse().unwrap()),
			Rule::char => Self::Char(source.as_str().as_bytes()[0] as i8),
			Rule::string => Self::String(source.into_inner().next().unwrap().as_str()),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedPrimaryExpression<'a> {
	Literal(ParsedLiteral<'a>),
	Identifier(&'a str),
	Array(Vec<ParsedExpression<'a>>),
	Expression(Box<ParsedExpression<'a>>),
	Structure(Vec<(&'a str, ParsedType<'a>)>),
	Union(Vec<(&'a str, ParsedType<'a>)>)
}

impl<'a> From<Pair<'a, Rule>> for ParsedPrimaryExpression<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::literal => Self::Literal(ParsedLiteral::from(source.into_inner().next().unwrap())),
			Rule::identifier => Self::Identifier(source.as_str()),
			Rule::array => todo!("ParsedPrimaryExpression: {:?}", source),
			Rule::expression_level_or => todo!("ParsedPrimaryExpression: {:?}", source),
			Rule::structure => todo!("ParsedPrimaryExpression: {:?}", source),
			Rule::union => todo!("ParsedPrimaryExpression: {:?}", source),
			_ => unreachable!()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedExpressionPostfix<'a> {
	Call(Vec<ParsedExpression<'a>>),
	Indexing(Box<ParsedExpression<'a>>),
	FieldAccess(&'a str),
	FieldDereference(&'a str)
}

impl<'a> From<Pair<'a, Rule>> for ParsedExpressionPostfix<'a> {
	fn from(source: Pair<'a, Rule>) -> Self {
		match source.as_rule() {
			Rule::call => Self::Call(source.into_inner().map(|argument| ParsedExpression::from(argument.into_inner())).collect()),
			_ => todo!("ParsedExpressionPostfix: {:?}", source)
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
		return result;
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
				Rule::op_level_prefix => prefixes.push(match pair.as_str() {
					"not" => ParsedOpLevelPrefix::Neg,
					"ref" => ParsedOpLevelPrefix::Ref,
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
			let op = match source.next().unwrap().as_str() {
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
			let op = match source.next().unwrap().as_str() {
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
			let op = match source.next().unwrap().as_str() {
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
		let is_const = match source.next().unwrap().as_str() {
			"const" => true,
			"var" => false,
			_ => unreachable!()
		};
		let name = source.next().unwrap().as_str();
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

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedLexpression<'a> {
	Identifier(&'a str),
	Dereference(Box<ParsedLexpression<'a>>),
	Indexing(Box<ParsedLexpression<'a>>, Box<ParsedExpression<'a>>),
	FieldAccess(Box<ParsedLexpression<'a>>, &'a str),
	FieldDereference(Box<ParsedLexpression<'a>>, &'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedReassignmentOp {
	Comp(ParsedOpLevelComp),
	Add(ParsedOpLevelAdd),
	Mul(ParsedOpLevelMul),
	And,
	Or
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedReassignment<'a> {
	pub(crate) lexpression: ParsedLexpression<'a>,
	pub(crate) reassignment_op: ParsedReassignmentOp,
	pub(crate) value: ParsedExpression<'a>
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
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFor<'a> {
	pub(crate) init: Option<Box<ParsedStatement<'a>>>,
	pub(crate) condition: Option<ParsedExpression<'a>>,
	pub(crate) with: Option<Box<ParsedStatement<'a>>>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedOperationHandler<'a> {
	pub(crate) operation: &'a str,
	pub(crate) arguments: Vec<&'a str>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedHandler<'a> {
	pub(crate) effect: &'a str,
	pub(crate) handlers: Vec<ParsedOperationHandler<'a>>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTry<'a> {
	pub(crate) body: Vec<ParsedStatement<'a>>,
	pub(crate) handlers: Vec<ParsedHandler<'a>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedStatement<'a> {
	Declaration(ParsedDeclaration<'a>),
	TypeDeclaration(ParsedTypeDeclaration<'a>),
 	Reassignment(ParsedReassignment<'a>),
 	If(ParsedIf<'a>),
 	While(ParsedWhile<'a>),
	For(ParsedFor<'a>),
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
			Rule::mirisa_if => Self::If(ParsedIf::from(source.into_inner())),
			Rule::expression_level_or => Self::Expression(ParsedExpression::from(source.into_inner())),
			Rule::mirisa_return => Self::Return(ParsedExpression::from(source.into_inner().next().unwrap().into_inner())),
			_ => todo!("ParsedStatement: {:?}", source)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedOperation<'a> {
	pub(crate) name: &'a str,
	pub(crate) provides_types: Vec<(&'a str, ParsedType<'a>)>,
	pub(crate) resume_type: ParsedType<'a>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedEffect<'a> {
	pub(crate) name: &'a str,
	pub(crate) operations: Vec<ParsedOperation<'a>>,
}

impl<'a> From<Pairs<'a, Rule>> for ParsedEffect<'a> {
	fn from(source: Pairs<'a, Rule>) -> Self {
		todo!("ParsedEffect: {:?}", source)
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
		let name = source.next().unwrap().as_str();
		let mut arguments = Vec::new();
		for mut argument in source.next().unwrap().into_inner().map(Pair::into_inner) {
			let arg_name = argument.next().unwrap().as_str();
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
			Rule::type_declaration => todo!("{:?}: {:?}", source.as_rule(), source.as_str()),
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
