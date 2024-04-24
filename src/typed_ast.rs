use std::collections::{HashSet, HashMap};
use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError<'a> {
	UndefinedType(&'a str),
	UndefinedName(&'a str),
	RedefinitionOfName(&'a str),
	TypeMismatch(TypedType<'a>, TypedType<'a>),
	EffectMismatch(HashSet<&'a str>, HashSet<&'a str>),
	CallingNotAFunction
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedType<'a> {
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
	Boolean,
	Unit,
	Never,
	Identifier(&'a str),
	Function(Vec<TypedType<'a>>, Box<TypedType<'a>>, HashSet<&'a str>),
	Pointer(bool, Box<TypedType<'a>>),
	Array(bool, Option<u64>, Box<TypedType<'a>>)
}

impl<'a> From<ast::AstType<'a>> for TypedType<'a> {
	fn from(ast: ast::AstType<'a>) -> Self {
		match ast {
			ast::AstType::I8 => Self::I8,
			ast::AstType::U8 => Self::U8,
			ast::AstType::I16 => Self::I16,
			ast::AstType::U16 => Self::U16,
			ast::AstType::I32 => Self::I32,
			ast::AstType::U32 => Self::U32,
			ast::AstType::F32 => Self::F32,
			ast::AstType::I64 => Self::I64,
			ast::AstType::U64 => Self::U64,
			ast::AstType::F64 => Self::F64,
			ast::AstType::Usize => Self::Usize,
			ast::AstType::Boolean => Self::Boolean,
			ast::AstType::Unit => Self::Unit,
			ast::AstType::Never => Self::Never,
			ast::AstType::Identifier(name) => Self::Identifier(name),
			ast::AstType::Function(args, ret, effect) => Self::Function(
				args.into_iter().map(TypedType::from).collect(),
				Box::new(TypedType::from(*ret)),
				HashSet::from_iter(effect.into_iter())
			),
			ast::AstType::Pointer(is_mut, r#type) => Self::Pointer(is_mut, Box::new(TypedType::from(*r#type))),
			ast::AstType::Array(is_mut, size, r#type) => Self::Array(is_mut, size, Box::new(TypedType::from(*r#type)))
		}
	}
}

impl<'a> TypedType<'a> {
	fn is_sub_of(&self, other: &Self) -> bool {
		match (self, other) {
			(s, o) if s == o => true,
			(&TypedType::Never, _) => true,
			(TypedType::Function(s_args, s_ret, s_effect), TypedType::Function(o_args, o_ret, o_effect)) =>
				s_args.into_iter().zip(o_args).all(|(s_arg, o_arg)| o_arg.is_sub_of(s_arg))
				&& s_ret.is_sub_of(o_ret)
				&& s_effect.is_subset(o_effect),
			(TypedType::Pointer(s_is_mut, s_inner), TypedType::Pointer(o_is_mut, o_inner)) => if !*o_is_mut || *s_is_mut == *o_is_mut {
				if *s_is_mut {
					**s_inner == **o_inner
				} else {
					s_inner.is_sub_of(o_inner.as_ref())
				}
			} else { false },
			(TypedType::Array(s_is_mut, s_size, s_inner), TypedType::Array(o_is_mut, o_size, o_inner)) => if *s_is_mut == *o_is_mut && *s_size == *o_size {
				if *s_is_mut {
					**s_inner == **o_inner
				} else {
					s_inner.is_sub_of(o_inner.as_ref())
				}
			} else { false },
			_ => false
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpressionInner<'a> {
	Integer(i128),
	Float(f64),
	Boolean(bool),
	String(&'a str),
	Unit,
	Identifier(&'a str),
	Array(Vec<TypedExpression<'a>>),
	Structure(Vec<(&'a str, TypedExpression<'a>)>),
	Call(Box<TypedExpression<'a>>, Vec<TypedExpression<'a>>),
	Indexing(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	FieldAccess(Box<TypedExpression<'a>>, &'a str),
	Negation(Box<TypedExpression<'a>>),
	Reference(Box<TypedExpression<'a>>),
	Or(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	And(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Eq(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Ne(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Lt(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Gt(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Le(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Ge(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Add(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Sub(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Mul(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Div(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	Rem(Box<TypedExpression<'a>>, Box<TypedExpression<'a>>),
	As(Box<TypedExpression<'a>>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedExpression<'a> {
	pub inner: TypedExpressionInner<'a>,
	r#type: TypedType<'a>,
	effect: HashSet<&'a str>
}

impl<'a> TryFrom<(&mut TypingContext<'a>, ast::AstExpression<'a>)> for TypedExpression<'a> {
	type Error = TypeError<'a>;

	fn try_from((ctx, ast): (&mut TypingContext<'a>, ast::AstExpression<'a>)) -> Result<Self, TypeError<'a>> {
		match ast {
			ast::AstExpression::Integer(n) => Ok(Self {
				inner: TypedExpressionInner::Integer(n),
				r#type: TypedType::I32,
				effect: HashSet::new()
			}),
			ast::AstExpression::String(s) => Ok(Self {
				inner: TypedExpressionInner::String(s),
				r#type: TypedType::Array(false, None, Box::new(TypedType::U8)),
				effect: HashSet::new()
			}),
			ast::AstExpression::Identifier(id) => Ok(Self {
				inner: TypedExpressionInner::Identifier(id),
				r#type: ctx.get_type(id)?,
				effect: HashSet::new()
			}),
			ast::AstExpression::Call(function, arguments) => {
				let mut ctx1 = ctx.clone();
				let typed_function = TypedExpression::try_from((&mut ctx1, *function))?;
				let (mut effect, argument_types, return_type) = match typed_function.clone().r#type {
					TypedType::Function(typed_arguments, return_type, function_effect) => {
						(function_effect, typed_arguments, *return_type)
					}
					_ => return Err(TypeError::CallingNotAFunction)
				};
				let mut typed_arguments = Vec::new();
				for (argument, needed_type) in arguments.into_iter().zip(argument_types) {
					let typed_argument = TypedExpression::try_from((&mut ctx1, argument))?;
					if !typed_argument.r#type.is_sub_of(&needed_type) {
						return Err(TypeError::TypeMismatch(needed_type, typed_argument.r#type));
					}
					effect.extend(&typed_argument.effect);
					typed_arguments.push(typed_argument);
				}
				*ctx = ctx1;
				Ok(Self {
					inner: TypedExpressionInner::Call(Box::new(typed_function), typed_arguments),
					r#type: return_type,
					effect
				})
			},
			_ => todo!("{ast:?}")
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedLexpression<'a> {
	Identifier(&'a str),
	Dereference(Box<TypedLexpression<'a>>),
	Indexing(Box<TypedLexpression<'a>>, Box<TypedExpression<'a>>),
	FieldAccess(Box<TypedLexpression<'a>>, &'a str)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedReassignmentOp {
	Add,
	Sub,
	Mul,
	Div,
	Rem,
	None
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedOperationHandler<'a> {
	pub operation: &'a str,
	pub arguments: Vec<(bool, &'a str)>,
	pub body: Vec<TypedStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedHandler<'a> {
	pub effect: &'a str,
	pub handlers: Vec<TypedOperationHandler<'a>>
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedDeclaration<'a> {
	pub is_mut: bool,
	pub name: &'a str,
	pub declaration_type: TypedType<'a>,
	pub value: Option<TypedExpression<'a>>
}

impl<'a> TryFrom<(TypingContext<'a>, ast::AstDeclaration<'a>)> for TypedDeclaration<'a> {
	type Error = TypeError<'a>;

	fn try_from((mut ctx, ast): (TypingContext<'a>, ast::AstDeclaration<'a>)) -> Result<Self, TypeError<'a>> {
		let declaration_type = TypedType::from(ast.declaration_type);
		let value = ast.value.map(|term| TypedExpression::try_from((&mut ctx, term))).transpose()?;
		if let Some(v) = &value { if !v.r#type.is_sub_of(&declaration_type) {
			return Err(TypeError::TypeMismatch(declaration_type, v.r#type.clone()));
		}}
		Ok(Self {
			is_mut: ast.is_mut,
			name: ast.name,
			declaration_type,
			value
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedStatementInner<'a> {
	Declaration(TypedDeclaration<'a>),
	Reassignment{
		lexpression: TypedLexpression<'a>,
		reassignment_op: TypedReassignmentOp,
		value: TypedExpression<'a>
	},
	If {
		condition: TypedExpression<'a>,
		then: Vec<TypedStatement<'a>>,
		elseifs: Vec<(TypedExpression<'a>, Vec<TypedStatement<'a>>)>,
		else_body: Option<Vec<TypedStatement<'a>>>,
	},
	While {
		condition: TypedExpression<'a>,
		body: Vec<TypedStatement<'a>>
	},
	DoWhile {
		body: Vec<TypedStatement<'a>>,
		condition: TypedExpression<'a>
	},
	Try {
		body: Vec<TypedStatement<'a>>,
		handlers: Vec<TypedHandler<'a>>
	},
	Block(Vec<TypedStatement<'a>>),
	Expression(TypedExpression<'a>),
	Return(TypedExpression<'a>),
	Break,
	Continue
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedStatement<'a> {
	pub inner: TypedStatementInner<'a>,
	pub effect: HashSet<&'a str>
}

impl<'a> TryFrom<(&mut TypingContext<'a>, ast::AstStatement<'a>)> for TypedStatement<'a> {
	type Error = TypeError<'a>;

	fn try_from((ctx, ast): (&mut TypingContext<'a>, ast::AstStatement<'a>)) -> Result<Self, TypeError<'a>> {
		match ast {
			ast::AstStatement::Expression(expression) => {
				let typed_expression = TypedExpression::try_from((ctx, expression))?;
				if !typed_expression.r#type.is_sub_of(&TypedType::Unit) {
					return Err(TypeError::TypeMismatch(TypedType::Unit, typed_expression.r#type))
				}
				Ok(Self {
					inner: TypedStatementInner::Expression(typed_expression.clone()),
					effect: typed_expression.effect,
				})
			},
			ast::AstStatement::Return(expression) => {
				let typed_expression = TypedExpression::try_from((ctx, expression))?;
				Ok(Self {
					inner: TypedStatementInner::Return(typed_expression.clone()),
					effect: typed_expression.effect,
				})
			},
			_ => todo!("{:#?}", (ctx, ast))
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedOperation<'a> {
	pub name: &'a str,
	pub provides_types: Vec<TypedType<'a>>,
	pub resume_type: TypedType<'a>
}

impl<'a> From<ast::AstOperation<'a>> for TypedOperation<'a> {
	fn from(ast: ast::AstOperation<'a>) -> Self {
		Self {
			name: ast.name,
			provides_types: ast.provides_types.into_iter().map(TypedType::from).collect(),
			resume_type: TypedType::from(ast.resume_type)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedEffect<'a> {
	pub name: &'a str,
	pub operations: Vec<TypedOperation<'a>>,
}

impl<'a> From<ast::AstEffect<'a>> for TypedEffect<'a> {
	fn from(ast: ast::AstEffect<'a>) -> Self {
		Self {name: ast.name, operations: ast.operations.into_iter().map(TypedOperation::from).collect()}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedFunction<'a> {
	pub name: &'a str,
	pub arguments: Vec<(bool, &'a str, TypedType<'a>)>,
	pub return_type: TypedType<'a>,
	pub effect_set: HashSet<&'a str>,
	pub body: Vec<TypedStatement<'a>>
}

impl<'a> TryFrom<(TypingContext<'a>, ast::AstFunction<'a>)> for TypedFunction<'a> {
	type Error = TypeError<'a>;

	fn try_from((mut ctx, ast): (TypingContext<'a>, ast::AstFunction<'a>)) -> Result<Self, TypeError<'a>> {
		let mut arguments = Vec::new();
		for (is_mut, name, r#type) in ast.arguments {
			let argument_type = TypedType::from(r#type);
			ctx.add_type(name, argument_type.clone());
			arguments.push((is_mut, name, argument_type));
		}
		let return_type = TypedType::from(ast.return_type);
		let effect_set = HashSet::from_iter(ast.effect_set);
		let mut effect = HashSet::new();
		let mut body = Vec::new();
		for statement in ast.body {
			body.push(TypedStatement::try_from((&mut ctx, statement))?);
			let last = &body.last().clone().unwrap();
			effect.extend(&last.effect);
			match &last.inner {
				TypedStatementInner::Return(expression) => if !expression.r#type.is_sub_of(&return_type) {
					return Err(TypeError::TypeMismatch(return_type, expression.r#type.clone()));
				},
				_ => ()
			};
		}
		if effect != effect_set {
			return Err(TypeError::EffectMismatch(effect_set, effect));
		}
		Ok(Self {
			name: ast.name,
			arguments,
			return_type,
			effect_set,
			body
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Adt<'a> {
	Record(Vec<(&'a str, TypedType<'a>)>),
	Variants(Vec<&'a str>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Adts<'a>(pub HashMap<&'a str, Adt<'a>>);

impl<'a> Adts<'a> {
	fn new() -> Self {
		Self(HashMap::new())
	}

	fn add_record(&mut self, name: &'a str, record: Vec<(&'a str, TypedType<'a>)>) {
		self.0.insert(name, Adt::Record(record));
	}

	fn add_variants(&mut self, name: &'a str, variants: Vec<&'a str>) {
		self.0.insert(name, Adt::Variants(variants));
	}

	fn get(&self, name: &'a str) -> Result<Adt<'a>, TypeError<'a>> {
		if let Some(adt) = self.0.get(name) {
			Ok(adt.clone())
		} else { Err(TypeError::UndefinedType(name)) }
	}

	fn get_records(&self) -> Vec<(&'a str, Vec<(&'a str, TypedType<'a>)>)> {
		let mut records = Vec::new();
		for (name, adt) in &self.0 {
			match adt {
				Adt::Record(record) => records.push((*name, record.clone())),
				_ => ()
			};
		}
		records
	}

	fn get_variants(&self) -> Vec<(&'a str, Vec<&'a str>)> {
		let mut variants = Vec::new();
		for (name, adt) in &self.0 {
			match adt {
				Adt::Variants(variants_) => variants.push((*name, variants_.clone())),
				_ => ()
			};
		}
		variants
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypingContext<'a>(
	pub HashMap<&'a str, TypedType<'a>>,
	pub HashMap<&'a str, Vec<TypedOperation<'a>>>
);

impl<'a> TypingContext<'a> {
	fn new() -> Self {
		Self(HashMap::new(), HashMap::new())
	}

	fn add_type(&mut self, name: &'a str, r#type: TypedType<'a>) {
		self.0.insert(name, r#type);
	}

	fn add_effect(&mut self, name: &'a str, effect: Vec<TypedOperation<'a>>) {
		self.1.insert(name, effect);
	}

	fn get_type(&self, name: &'a str) -> Result<TypedType<'a>, TypeError<'a>> {
		if let Some(r#type) = self.0.get(name) {
			Ok(r#type.clone())
		} else { Err(TypeError::UndefinedName(name)) }
	}

	fn get_effect(&self, name: &'a str) -> Result<Vec<TypedOperation<'a>>, TypeError<'a>> {
		if let Some(effect) = self.1.get(name) {
			Ok(effect.clone())
		} else { Err(TypeError::UndefinedName(name)) }
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedProgram<'a> {
	pub effects: Vec<TypedEffect<'a>>,
	pub records: Vec<(&'a str, Vec<(&'a str, TypedType<'a>)>)>,
	pub variants: Vec<(&'a str, Vec<&'a str>)>,
	pub extern_declarations: Vec<(&'a str, TypedType<'a>)>,
	pub declarations: Vec<TypedDeclaration<'a>>,
	pub functions: Vec<TypedFunction<'a>>
}

impl<'a> TryFrom<ast::AstProgram<'a>> for TypedProgram<'a> {
	type Error = TypeError<'a>;

	fn try_from(ast: ast::AstProgram<'a>) -> Result<Self, TypeError<'a>> {
		let mut typing_context = TypingContext::new();
		let mut effects = Vec::new();
		effects.push(TypedEffect {
			name: "consoleIo",
			operations: vec![
				TypedOperation {
					name: "print_char",
					provides_types: vec![TypedType::U8],
					resume_type: TypedType::Unit
				},
				TypedOperation {
					name: "print",
					provides_types: vec![TypedType::Array(false, None, Box::new(TypedType::U8))],
					resume_type: TypedType::Unit
				},
				TypedOperation {
					name: "println",
					provides_types: vec![TypedType::Array(false, None, Box::new(TypedType::U8))],
					resume_type: TypedType::Unit
				}
			]
		});
		effects.extend(ast.effects.into_iter().map(TypedEffect::from));
		let effects = effects;
		for effect in &effects {
			for op in &effect.operations {
				if matches!(typing_context.get_type(op.name), Ok(_)) {
					return Err(TypeError::RedefinitionOfName(op.name))
				}
				typing_context.add_type(op.name, TypedType::Function(
					op.provides_types.clone(),
					Box::new(op.resume_type.clone()),
					HashSet::from([effect.name])
				))
			}
			if matches!(typing_context.get_effect(effect.name), Ok(_)) {
				return Err(TypeError::RedefinitionOfName(effect.name))
			}
			typing_context.add_effect(effect.name, effect.operations.clone());
		}
		let mut adts = Adts::new();
		for record in ast.structures {
			if matches!(adts.get(record.0), Ok(_)) {
				return Err(TypeError::RedefinitionOfName(record.0))
			}
			adts.add_record(record.0, record.1.into_iter().map(|(name, r#type)| (name, TypedType::from(r#type))).collect());
		}
		for variants in ast.variants {
			if matches!(adts.get(variants.0), Ok(_)) {
				return Err(TypeError::RedefinitionOfName(variants.0))
			}
			adts.add_variants(variants.0, variants.1);
		}
		let adts = adts;
		for (name, variants) in adts.get_variants() {
			for variant_name in variants {
				if matches!(typing_context.get_type(variant_name), Ok(_)) {
					return Err(TypeError::RedefinitionOfName(variant_name))
				}
				typing_context.add_type(variant_name, TypedType::Identifier(name));
			}
		}
		let extern_declarations: Vec<(&str, TypedType)> = ast.extern_declarations.into_iter().map(|(name, r#type)| (name, TypedType::from(r#type))).collect();
		for (name, r#type) in &extern_declarations {
			typing_context.add_type(*name, r#type.clone());
		}
		let declarations = {
			let mut declarations = Vec::new();
			for declaration in ast.declarations {
				let typed_declaration = TypedDeclaration::try_from((typing_context.clone(), declaration))?;
				typing_context.add_type(typed_declaration.name, typed_declaration.declaration_type.clone());
				declarations.push(typed_declaration);
			}
			declarations
		};
		let functions = {
			let mut functions: Vec<TypedFunction<'a>> = Vec::new();
			for function in ast.functions {
				functions.push(TypedFunction::try_from((typing_context.clone(), function))?);
			}
			functions
		};
		Ok(Self {
			effects,
			records: adts.get_records(),
			variants: adts.get_variants(),
			extern_declarations,
			declarations,
			functions
		})
	}
}
