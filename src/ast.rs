use crate::parser;

#[derive(Debug, Clone, PartialEq)]
pub enum AstType<'a> {
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
	Unit,
	Identifier(&'a str),
	Structure(Vec<(&'a str, AstType<'a>)>),
	Variants(Vec<&'a str>),
	Union(Vec<(&'a str, AstType<'a>)>),
	Function(Vec<AstType<'a>>, Box<AstType<'a>>, Vec<&'a str>),
	Pointer(bool, Box<AstType<'a>>),
	Array(Option<u64>, Box<AstType<'a>>)
}

impl From<parser::ParsedPrimitiveType> for AstType<'_> {
	fn from(cst: parser::ParsedPrimitiveType) -> Self {
		use parser::ParsedPrimitiveType;
		match cst {
			ParsedPrimitiveType::I8 => Self::I8,
			ParsedPrimitiveType::U8 => Self::U8,
			ParsedPrimitiveType::I16 => Self::I16,
			ParsedPrimitiveType::U16 => Self::U16,
			ParsedPrimitiveType::I32 => Self::I32,
			ParsedPrimitiveType::U32 => Self::U32,
			ParsedPrimitiveType::F32 => Self::F32,
			ParsedPrimitiveType::I64 => Self::I64,
			ParsedPrimitiveType::U64 => Self::U64,
			ParsedPrimitiveType::F64 => Self::F64,
			ParsedPrimitiveType::Usize => Self::Usize,
			ParsedPrimitiveType::Char => Self::Char,
			ParsedPrimitiveType::Unit => Self::Unit
		}
	}
}

impl<'a> From<parser::ParsedPrimaryType<'a>> for AstType<'a> {
	fn from(cst: parser::ParsedPrimaryType<'a>) -> Self {
		use parser::ParsedPrimaryType;
		match cst {
			ParsedPrimaryType::Primitive(primitive) => Self::from(primitive),
			ParsedPrimaryType::Identifier(identifier) => Self::Identifier(identifier),
			ParsedPrimaryType::Structure(structure) => Self::Structure(structure.into_iter().map(|(name, r#type)| (name, AstType::from(r#type))).collect()),
			ParsedPrimaryType::Variants(variants) => Self::Variants(variants),
			ParsedPrimaryType::Union(union) => Self::Union(union.into_iter().map(|(name, r#type)| (name, AstType::from(r#type))).collect()),
			ParsedPrimaryType::Function(args, return_type, effect_set) => Self::Function(
				args.into_iter().map(AstType::from).collect(),
				Box::new(AstType::from(*return_type)),
				effect_set
			)
		}
	}
}

impl<'a> From<parser::ParsedType<'a>> for AstType<'a> {
	fn from(cst: parser::ParsedType<'a>) -> Self {
		match cst {
			parser::ParsedType::PrimaryType(primary_type) => Self::from(primary_type),
			parser::ParsedType::WithModifier(modifier, type_inner) => match modifier {
				parser::ParsedTypeModifier::Pointer(is_mut) => Self::Pointer(is_mut, Box::new(Self::from(*type_inner))),
				parser::ParsedTypeModifier::Array(size) => Self::Array(size, Box::new(Self::from(*type_inner)))
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstExpression<'a> {
	Integer(i128),
	Float(f64),
	Char(i8),
	String(&'a str),
	Unit,
	Identifier(&'a str),
	Array(Vec<AstExpression<'a>>),
	Structure(Vec<(&'a str, AstExpression<'a>)>),
	Union(Vec<(&'a str, AstExpression<'a>)>),
	Call(Box<AstExpression<'a>>, Vec<AstExpression<'a>>),
	Indexing(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	FieldAccess(Box<AstExpression<'a>>, &'a str),
	Negation(Box<AstExpression<'a>>),
	Reference(Box<AstExpression<'a>>),
	Or(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	And(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Eq(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Ne(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Lt(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Gt(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Le(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Ge(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Add(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Sub(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Mul(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Div(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	Rem(Box<AstExpression<'a>>, Box<AstExpression<'a>>),
	As(Box<AstExpression<'a>>, AstType<'a>)
}

impl<'a> From<parser::ParsedLiteral<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedLiteral<'a>) -> Self {
		match cst {
			parser::ParsedLiteral::Integer(integer) => Self::Integer(integer),
			parser::ParsedLiteral::Float(float) => Self::Float(float),
			parser::ParsedLiteral::Char(character) => Self::Char(character),
			parser::ParsedLiteral::String(string) => Self::String(string),
			parser::ParsedLiteral::Unit => Self::Unit,
		}
	}
}

impl<'a> From<parser::ParsedPrimaryExpression<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedPrimaryExpression<'a>) -> Self {
		match cst {
			parser::ParsedPrimaryExpression::Literal(literal) => Self::from(literal),
			parser::ParsedPrimaryExpression::Identifier(identifier) => Self::Identifier(identifier),
			parser::ParsedPrimaryExpression::Array(array) => Self::Array(array.into_iter().map(|item| Self::from(item)).collect()),
			parser::ParsedPrimaryExpression::Structure(structure) => Self::Structure(structure.into_iter().map(|(name, item)| (name, Self::from(item))).collect()),
			parser::ParsedPrimaryExpression::Union(union) => Self::Union(union.into_iter().map(|(name, item)| (name, Self::from(item))).collect()),
			parser::ParsedPrimaryExpression::Expression(expression) => Self::from(*expression)
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelBottom<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelBottom<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelBottom::PrimaryExpression(level_bottom) => Self::from(level_bottom),
			parser::ParsedExpressionLevelBottom::Postfix(lhs, op) => match op {
				parser::ParsedExpressionPostfix::Call(args) => Self::Call(
					Box::new(Self::from(*lhs)),
					args.into_iter().map(|arg| Self::from(arg)).collect()
				),
				parser::ParsedExpressionPostfix::Indexing(arg) => Self::Indexing(
					Box::new(Self::from(*lhs)),
					Box::new(Self::from(*arg))
				),
				parser::ParsedExpressionPostfix::FieldAccess(field) => Self::FieldAccess(
					Box::new(Self::from(*lhs)),
					field
				),
			}
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelPrefix<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelPrefix<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelPrefix::LevelBottom(level_bottom) => Self::from(level_bottom),
			parser::ParsedExpressionLevelPrefix::Operator(op, rhs) => match op {
				parser::ParsedOpLevelPrefix::Neg => Self::Negation(Box::new(Self::from(*rhs))),
				parser::ParsedOpLevelPrefix::Ref => Self::Reference(Box::new(Self::from(*rhs))),
			}
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelAs<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelAs<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelAs::LevelPrefix(level_prefix) => Self::from(level_prefix),
			parser::ParsedExpressionLevelAs::Operator(lhs, rhs) => Self::As(Box::new(Self::from(*lhs)), AstType::from(rhs))
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelMul<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelMul<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelMul::LevelAs(level_as) => Self::from(level_as),
			parser::ParsedExpressionLevelMul::Operator(op, lhs, rhs) => match op {
				parser::ParsedOpLevelMul::Mul => Self::Mul(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelMul::Div => Self::Div(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelMul::Rem => Self::Rem(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
			}
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelAdd<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelAdd<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelAdd::LevelMul(level_mul) => Self::from(level_mul),
			parser::ParsedExpressionLevelAdd::Operator(op, lhs, rhs) => match op {
				parser::ParsedOpLevelAdd::Add => Self::Add(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelAdd::Sub => Self::Sub(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
			}
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelComp<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelComp<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelComp::LevelAdd(level_add) => Self::from(level_add),
			parser::ParsedExpressionLevelComp::Operator(op, lhs, rhs) => match op {
				parser::ParsedOpLevelComp::Eq => Self::Eq(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelComp::Ne => Self::Ne(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelComp::Lt => Self::Lt(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelComp::Gt => Self::Gt(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelComp::Le => Self::Le(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
				parser::ParsedOpLevelComp::Ge => Self::Ge(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs))),
			}
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelAnd<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelAnd<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelAnd::LevelComp(level_comp) => Self::from(level_comp),
			parser::ParsedExpressionLevelAnd::Operator(lhs, rhs) => Self::And(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs)))
		}
	}
}

impl<'a> From<parser::ParsedExpressionLevelOr<'a>> for AstExpression<'a> {
	fn from(cst: parser::ParsedExpressionLevelOr<'a>) -> Self {
		match cst {
			parser::ParsedExpressionLevelOr::LevelAnd(level_and) => Self::from(level_and),
			parser::ParsedExpressionLevelOr::Operator(lhs, rhs) => Self::Or(Box::new(Self::from(*lhs)), Box::new(Self::from(rhs)))
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstLexpression<'a> {
	Identifier(&'a str),
	Dereference(Box<AstLexpression<'a>>),
	Indexing(Box<AstLexpression<'a>>, Box<AstExpression<'a>>),
	FieldAccess(Box<AstLexpression<'a>>, &'a str)
}

impl<'a> From<parser::ParsedLexpression<'a>> for AstLexpression<'a> {
	fn from(cst: parser::ParsedLexpression<'a>) -> Self {
		match cst {
			parser::ParsedLexpression::Identifier(identifier) => Self::Identifier(identifier),
			parser::ParsedLexpression::Dereference(lexpression) => Self::Dereference(Box::new(Self::from(*lexpression))),
			parser::ParsedLexpression::Indexing(lexpression, expression) => Self::Indexing(Box::new(Self::from(*lexpression)), Box::new(AstExpression::from(*expression))),
			parser::ParsedLexpression::FieldAccess(lexpression, field) => Self::FieldAccess(Box::new(Self::from(*lexpression)), field)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstReassignmentOp {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	None
}

impl<'a> From<parser::ParsedReassignmentOp> for AstReassignmentOp {
	fn from(cst: parser::ParsedReassignmentOp) -> Self {
		match cst {
			parser::ParsedReassignmentOp::Add => Self::Add,
			parser::ParsedReassignmentOp::Sub => Self::Sub,
			parser::ParsedReassignmentOp::Mul => Self::Mul,
			parser::ParsedReassignmentOp::Div => Self::Div,
			parser::ParsedReassignmentOp::Rem => Self::Rem,
			parser::ParsedReassignmentOp::None => Self::None,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstOperationHandler<'a> {
	pub operation: &'a str,
	pub arguments: Vec<(bool, &'a str)>,
	pub body: Vec<AstStatement<'a>>,
}

impl<'a> From<parser::ParsedOperationHandler<'a>> for AstOperationHandler<'a> {
	fn from(cst: parser::ParsedOperationHandler<'a>) -> Self {
		Self {
			operation: cst.operation,
			arguments: cst.arguments,
			body: cst.body.into_iter().map(AstStatement::from).collect()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstHandler<'a> {
	pub effect: &'a str,
	pub handlers: Vec<AstOperationHandler<'a>>
}

impl<'a> From<parser::ParsedHandler<'a>> for AstHandler<'a> {
	fn from(cst: parser::ParsedHandler<'a>) -> Self {
		Self { effect: cst.effect, handlers: cst.handlers.into_iter().map(AstOperationHandler::from).collect() }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstDeclaration<'a> {
	pub is_mut: bool,
	pub name: &'a str,
	pub alias_type: AstType<'a>,
	pub value: Option<AstExpression<'a>>
}

impl<'a> From<parser::ParsedDeclaration<'a>> for AstDeclaration<'a> {
	fn from(cst: parser::ParsedDeclaration<'a>) -> Self {
		Self { is_mut: cst.is_mut, name: cst.name, alias_type: AstType::from(cst.alias_type), value: cst.value.map(AstExpression::from) }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstTypeAlias<'a> {
	name: &'a str,
	alias_type: AstType<'a>
}

impl<'a> From<parser::ParsedTypeAlias<'a>> for AstTypeAlias<'a> {
	fn from(cst: parser::ParsedTypeAlias<'a>) -> Self {
		Self { name: cst.name, alias_type: AstType::from(cst.alias_type) }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstStatement<'a> {
	Declaration(AstDeclaration<'a>),
	TypeAlias(AstTypeAlias<'a>),
	Reassignment{
		lexpression: AstLexpression<'a>,
		reassignment_op: AstReassignmentOp,
		value: AstExpression<'a>
	},
	If {
		condition: AstExpression<'a>,
		then: Vec<AstStatement<'a>>,
		elseifs: Vec<(AstExpression<'a>, Vec<AstStatement<'a>>)>,
		else_body: Option<Vec<AstStatement<'a>>>,
	},
	While {
		condition: AstExpression<'a>,
		body: Vec<AstStatement<'a>>
	},
	DoWhile {
		body: Vec<AstStatement<'a>>,
		condition: AstExpression<'a>
	},
	Try {
		body: Vec<AstStatement<'a>>,
		handlers: Vec<AstHandler<'a>>
	},
	Block(Vec<AstStatement<'a>>),
	Expression(AstExpression<'a>),
	Return(AstExpression<'a>),
	Break,
	Continue
}

impl<'a> From<parser::ParsedStatement<'a>> for AstStatement<'a> {
	fn from(cst: parser::ParsedStatement<'a>) -> Self {
		match cst {
			parser::ParsedStatement::Declaration(statement) => Self::Declaration(AstDeclaration::from(statement)),
			parser::ParsedStatement::TypeAlias(statement) => Self::TypeAlias(AstTypeAlias::from(statement)),
			parser::ParsedStatement::Reassignment(statement) => Self::Reassignment {
				lexpression: AstLexpression::from(statement.lexpression),
				reassignment_op: AstReassignmentOp::from(statement.reassignment_op),
				value: AstExpression::from(statement.value)
			},
			parser::ParsedStatement::If(statement) => Self::If {
				condition: AstExpression::from(statement.condition),
				then: statement.then.into_iter().map(|inner| AstStatement::from(inner)).collect(),
				elseifs: statement.elseifs.into_iter().map(|(condition, body)| (AstExpression::from(condition), body.into_iter().map(|inner| AstStatement::from(inner)).collect())).collect(),
				else_body: statement.else_body.map(|body| body.into_iter().map(|inner| AstStatement::from(inner)).collect())
			},
			parser::ParsedStatement::While(statement) => Self::While {
				condition: AstExpression::from(statement.condition),
				body: statement.body.into_iter().map(|inner| AstStatement::from(inner)).collect()
			},
			parser::ParsedStatement::DoWhile(statement) => Self::DoWhile {
				body: statement.body.into_iter().map(|inner| AstStatement::from(inner)).collect(),
				condition: AstExpression::from(statement.condition)
			},
			parser::ParsedStatement::Try(statement) => Self::Try {
				body: statement.body.into_iter().map(|inner| AstStatement::from(inner)).collect(),
				handlers: statement.handlers.into_iter().map(|inner| AstHandler::from(inner)).collect(),
			},
			parser::ParsedStatement::Block(statement) => Self::Block(statement.into_iter().map(AstStatement::from).collect()),
			parser::ParsedStatement::Expression(statement) => Self::Expression(AstExpression::from(statement)),
			parser::ParsedStatement::Return(statement) => Self::Return(AstExpression::from(statement)),
			parser::ParsedStatement::Break => Self::Break,
			parser::ParsedStatement::Continue => Self::Continue
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstOperation<'a> {
	pub name: &'a str,
	pub provides_types: Vec<AstType<'a>>,
	pub resume_type: AstType<'a>
}

impl<'a> From<parser::ParsedOperation<'a>> for AstOperation<'a> {
	fn from(cst: parser::ParsedOperation<'a>) -> Self {
		Self {
			name: cst.name,
			provides_types: cst.provides_types.into_iter().map(AstType::from).collect(),
			resume_type: AstType::from(cst.resume_type)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstEffect<'a> {
	pub name: &'a str,
	pub operations: Vec<AstOperation<'a>>,
}

impl<'a> From<parser::ParsedEffect<'a>> for AstEffect<'a> {
	fn from(cst: parser::ParsedEffect<'a>) -> Self {
		Self { name: cst.name, operations: cst.operations.into_iter().map(AstOperation::from).collect() }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstFunction<'a> {
	name: &'a str,
	arguments: Vec<(bool, &'a str, AstType<'a>)>,
	return_type: AstType<'a>,
	effect_set: Vec<&'a str>,
	body: Vec<AstStatement<'a>>
}

impl<'a> From<parser::ParsedFunction<'a>> for AstFunction<'a> {
	fn from(cst: parser::ParsedFunction<'a>) -> Self {
		Self {
			name: cst.name,
			arguments: cst.arguments.into_iter().map(|(arg_is_mut, arg_name, arg_type)| (arg_is_mut, arg_name, AstType::from(arg_type))).collect(),
			return_type: AstType::from(cst.return_type),
			effect_set: cst.effect_set,
			body: cst.body.into_iter().map(AstStatement::from).collect()
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstProgram<'a> {
	pub includes: Vec<&'a str>,
	pub extern_declarations: Vec<(&'a str, AstType<'a>)>,
	pub declarations: Vec<AstDeclaration<'a>>,
	pub type_aliases: Vec<AstTypeAlias<'a>>,
	pub effects: Vec<AstEffect<'a>>,
	pub functions: Vec<AstFunction<'a>>
}

impl<'a> From<parser::ParsedProgram<'a>> for AstProgram<'a> {
	fn from(cst: parser::ParsedProgram<'a>) -> Self {
		let mut includes = Vec::new();
		let mut extern_declarations = Vec::new();
		let mut declarations = Vec::new();
		let mut type_aliases = Vec::new();
		let mut effects = Vec::new();
		let mut functions = Vec::new();
		for item in cst.0 {
			use parser::ParsedItem;
			match item {
				ParsedItem::Include(path) => includes.push(path),
				ParsedItem::ExternDeclaration(name, r#type) => extern_declarations.push((name, AstType::from(r#type))),
				ParsedItem::Declaration(declaration) => declarations.push(AstDeclaration::from(declaration)),
				ParsedItem::TypeAlias(declaration) => type_aliases.push(AstTypeAlias::from(declaration)),
				ParsedItem::Effect(effect) => effects.push(AstEffect::from(effect)),
				ParsedItem::Function(function) => functions.push(AstFunction::from(function)),
			};
		}
		Self { includes, extern_declarations, declarations, type_aliases, effects, functions }
	}
}
