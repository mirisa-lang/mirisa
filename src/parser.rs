// use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MirisaParser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedPrimitiveType {
	I8,
	U8,
	I16,
	U16,
	I32,
	U32,
	F32,
	I128,
	U128,
	F128,
	Char,
	Unit
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedPrimaryType<'a> {
	Primitive(ParsedPrimitiveType),
	Identifier(&'a str),
	Structure(Vec<(&'a str, ParsedExpression<'a>)>),
	Union(&'a str, Box<ParsedExpression<'a>>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedTypeModifier {
	Pointer,
	PointerToConst,
	Array(Option<u64>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedType<'a> {
	pub(crate) modifiers: Vec<ParsedTypeModifier>,
	pub(crate) primary_type: ParsedPrimaryType<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedLiteral<'a> {
	Integer(i128),
	Float(f64),
	Char(i8),
	String(&'a str)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedPrimaryExpression<'a> {
	Literal(ParsedLiteral<'a>),
	Identifier(&'a str),
	Array(Vec<ParsedExpression<'a>>),
	Expression(Box<ParsedExpression<'a>>),
	Structure(Vec<(&'a str, ParsedType<'a>)>),
	Union(Vec<(&'a str, ParsedType<'a>)>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionPostfix<'a> {
	Call(Vec<ParsedLexpression<'a>>),
	Indexing(ParsedLexpression<'a>),
	FieldAccess(&'a str),
	FieldDereference(&'a str)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedExpressionLevelBottom<'a> {
	pub(crate) expression: ParsedPrimaryExpression<'a>,
	pub(crate) postfixes: Vec<ParsedExpressionPostfix<'a>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedOpLevelPrefix {
	Neg,
	Ref
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelPrefix<'a> {
	LowerLevel(ParsedExpressionLevelBottom<'a>),
	Operator(ParsedExpressionLevelBottom<'a>, ParsedExpressionLevelBottom<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelAs<'a> {
	LowerLevel(ParsedExpressionLevelPrefix<'a>),
	Operator(ParsedExpressionLevelPrefix<'a>, ParsedType<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedOpLevelMul {
	Div,
	Mul,
	Rem
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelMul<'a> {
	LowerLevel(ParsedExpressionLevelAs<'a>),
	Operator(ParsedOpLevelMul, ParsedExpressionLevelAs<'a>, ParsedExpressionLevelAs<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedOpLevelAdd {
	Add,
	Sub
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelAdd<'a> {
	LowerLevel(ParsedExpressionLevelMul<'a>),
	Operator(ParsedOpLevelAdd, ParsedExpressionLevelMul<'a>, ParsedExpressionLevelMul<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedOpLevelComp {
	Eq,
	Ne,
	Lt,
	Gt,
	Le,
	Ge
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelComp<'a> {
	LowerLevel(ParsedExpressionLevelAdd<'a>),
	Operator(ParsedOpLevelComp, ParsedExpressionLevelAdd<'a>, ParsedExpressionLevelAdd<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelAnd<'a> {
	LowerLevel(ParsedExpressionLevelAdd<'a>),
	Operator(ParsedExpressionLevelAdd<'a>, ParsedExpressionLevelAdd<'a>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedExpressionLevelOr<'a> {
	LowerLevel(ParsedExpressionLevelAnd<'a>),
	Operator(ParsedExpressionLevelAnd<'a>, ParsedExpressionLevelAnd<'a>)
}

type ParsedExpression<'a> = ParsedExpressionLevelOr<'a>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedDeclaration<'a> {
	pub(crate) is_const: bool,
	pub(crate) name: &'a str,
	pub(crate) declaration_type: ParsedType<'a>,
	pub(crate) value: ParsedExpression<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedTypeDeclaration<'a> {
	pub(crate) name: &'a str,
	pub(crate) declaration_type: ParsedType<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedLexpression<'a> {
	Identifier(&'a str),
	Dereference(Box<ParsedLexpression<'a>>),
	Indexing(Box<(ParsedLexpression<'a>)>, Box<ParsedExpression<'a>>),
	FieldAccess(Box<(ParsedLexpression<'a>)>, &'a str),
	FieldDereference(Box<(ParsedLexpression<'a>)>, &'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedReassignmentOp {
	Comp(ParsedOpLevelComp),
	Add(ParsedOpLevelAdd),
	Mul(ParsedOpLevelMul),
	And,
	Or
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedReassignment<'a> {
	pub(crate) lexpression: ParsedLexpression<'a>,
	pub(crate) reassignment_op: ParsedReassignmentOp,
	pub(crate) value: ParsedExpression<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedIf<'a> {
	pub(crate) condition: Option<ParsedExpression<'a>>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedWhile<'a> {
	pub(crate) condition: Option<ParsedExpression<'a>>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedFor<'a> {
	pub(crate) init: Option<ParsedStatement<'a>>,
	pub(crate) condition: Option<ParsedExpression<'a>>,
	pub(crate) with: Option<ParsedStatement<'a>>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedOperationHandler<'a> {
	pub(crate) operation: &'a str,
	pub(crate) arguments: Vec<&'a str>,
	pub(crate) body: Vec<ParsedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedHandler<'a> {
	pub(crate) effect: &'a str,
	pub(crate) handlers: Vec<ParsedOperationHandler<'a>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedTry<'a> {
	pub(crate) body: Vec<ParsedStatement<'a>>,
	pub(crate) handlers: Vec<ParsedHandler<'a>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedInclude<'a>(pub(crate) &'a str);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedOperation<'a> {
	pub(crate) name: &'a str,
	pub(crate) provides_types: Vec<(&'a str, ParsedType<'a>)>,
	pub(crate) resume_type: ParsedType<'a>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedEffect<'a> {
	pub(crate) name: &'a str,
	pub(crate) operations: Vec<ParsedOperation<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedFunction<'a> {
	name: &'a str,
	arguments: Vec<(&'a str, ParsedType<'a>)>,
	return_type: ParsedType<'a>,
	effect_set: Vec<&'a str>,
	body: Vec<ParsedStatement<'a>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedItem<'a> {
	Include(ParsedInclude<'a>),
	Declaration(ParsedDeclaration<'a>),
	TypeDeclaration(ParsedTypeDeclaration<'a>),
	Effect(ParsedEffect<'a>),
	Function(ParsedFunction<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedProgram<'a>(pub(crate) Vec<ParsedItem<'a>>);
