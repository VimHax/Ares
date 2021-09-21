use std::{
	marker::PhantomData,
	ops::{Add, Sub},
};

use ares_core::Span;

use super::{ty::TyRef, TypeError};

/// An AST node.
pub trait Node {
	fn span(&self) -> &Span;
}

/// Has this node been analyzed?
pub trait IsAnalyzed {}

/// An analyzed AST node.
#[derive(Debug, Clone)]
pub struct Analyzed;
impl IsAnalyzed for Analyzed {}

/// An AST node yet to be analyzed.
#[derive(Debug, Clone)]
pub struct NotAnalyzed;
impl IsAnalyzed for NotAnalyzed {}

/// Will this code do `X`?
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Status {
	Never,
	Might,
	Will,
}

impl Add for Status {
	type Output = Status;

	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Status::Never, Status::Never) => Status::Never,
			(Status::Never, Status::Might) => Status::Might,
			(Status::Might, Status::Never) => Status::Might,
			(Status::Might, Status::Might) => Status::Might,
			(_, _) => Status::Will,
		}
	}
}

impl Sub for Status {
	type Output = Status;

	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Status::Never, Status::Never) => Status::Never,
			(Status::Will, Status::Will) => Status::Will,
			(_, _) => Status::Might,
		}
	}
}

/// Could this code execute `break`, `return` or `loop` indefinitely?
#[derive(Debug, Clone)]
pub struct ExitStatus {
	will_exit: bool,
	return_status: Status,
	break_status: Status,
	infinite_loop_status: Status,
}

impl ExitStatus {
	pub fn new() -> Self {
		Self {
			will_exit: false,
			return_status: Status::Never,
			break_status: Status::Never,
			infinite_loop_status: Status::Never,
		}
	}

	/// Could this code execute `return`?
	pub fn return_status(&self) -> Status {
		self.return_status
	}

	/// Could this code execute `break`?
	pub fn break_status(&self) -> Status {
		self.break_status
	}

	/// Could this code `loop` indefinitely?
	pub fn infinite_loop_status(&self) -> Status {
		self.infinite_loop_status
	}

	pub fn set_return(&mut self, value: Status) {
		self.return_status = value;
	}

	pub fn set_break(&mut self, value: Status) {
		self.break_status = value;
	}

	pub fn set_infinite_loop(&mut self, value: Status) {
		self.infinite_loop_status = value;
	}

	pub fn set_will_exit(&mut self, value: bool) {
		self.will_exit = value;
	}

	/// Will any code following this code get executed?
	pub fn will_exit(&self) -> bool {
		self.will_exit
			|| self.return_status == Status::Will
			|| self.break_status == Status::Will
			|| self.infinite_loop_status == Status::Will
	}

	/// Could this code and the provided code
	/// exit if both are executed?
	pub fn and(&mut self, status: &Self) {
		self.will_exit |= status.will_exit;
		self.return_status = self.return_status + status.return_status;
		self.break_status = self.break_status + status.break_status;
		self.infinite_loop_status = self.infinite_loop_status + status.infinite_loop_status;
	}

	/// Could this code and the provided code
	/// exit if both aren't always executed?
	pub fn or(&mut self, status: &Self) {
		self.will_exit &= status.will_exit;
		self.return_status = self.return_status - status.return_status;
		self.break_status = self.break_status - status.break_status;
		self.infinite_loop_status = self.infinite_loop_status - status.infinite_loop_status;
	}
}

// Parenthesis //

#[derive(Debug)]
pub struct Parenthesis<T: IsAnalyzed> {
	expr: Box<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Parenthesis<T> {
	pub fn expression(&self) -> &Expression<T> {
		&self.expr
	}
}

impl Parenthesis<NotAnalyzed> {
	pub fn new(expr: Expression<NotAnalyzed>, span: Span) -> Self {
		Self {
			expr: Box::new(expr),
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
		) -> Result<(Expression<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Parenthesis<Analyzed>, TypeError> {
		let (expr, datatype, exit_status) = analyze(*self.expr)?;
		Ok(Parenthesis {
			expr: Box::new(expr),
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl Parenthesis<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Parenthesis<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Variable //

#[derive(Debug)]
pub struct Variable<T: IsAnalyzed> {
	name: String,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Variable<T> {
	pub fn name(&self) -> &String {
		&self.name
	}
}

impl Variable<NotAnalyzed> {
	pub fn new(name: String, span: Span) -> Self {
		Self {
			name,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze(self, ty: TyRef, exit_status: ExitStatus) -> Variable<Analyzed> {
		Variable {
			name: self.name,
			span: self.span,
			ty: Some(ty),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		}
	}
}

impl Variable<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Variable<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Literal //

#[derive(Debug)]
pub enum LiteralValue {
	Int(i64),
	Float(f64),
	Boolean(bool),
	String(String),
}

#[derive(Debug)]
pub struct Literal<T: IsAnalyzed> {
	value: LiteralValue,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Literal<T> {
	pub fn value(&self) -> &LiteralValue {
		&self.value
	}
}

impl Literal<NotAnalyzed> {
	pub fn new(value: LiteralValue, span: Span) -> Self {
		Self {
			value,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze(self, ty: TyRef, exit_status: ExitStatus) -> Literal<Analyzed> {
		Literal {
			value: self.value,
			span: self.span,
			ty: Some(ty),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		}
	}
}

impl Literal<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Literal<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Array //

#[derive(Debug)]
pub struct Array<T: IsAnalyzed> {
	elements: Vec<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Array<T> {
	pub fn elements(&self) -> &Vec<Expression<T>> {
		&self.elements
	}
}

impl Array<NotAnalyzed> {
	pub fn new(elements: Vec<Expression<NotAnalyzed>>, span: Span) -> Self {
		Self {
			elements,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Vec<Expression<NotAnalyzed>>,
		) -> Result<(Vec<Expression<Analyzed>>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Array<Analyzed>, TypeError> {
		let (elements, datatype, exit_status) = analyze(self.elements)?;
		Ok(Array {
			elements,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl Array<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Array<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// IndexOf //

#[derive(Debug)]
pub struct IndexOf<T: IsAnalyzed> {
	expr: Box<Expression<T>>,
	index: Box<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> IndexOf<T> {
	pub fn expression(&self) -> &Expression<T> {
		&self.expr
	}

	pub fn index(&self) -> &Expression<T> {
		&self.index
	}
}

impl IndexOf<NotAnalyzed> {
	pub fn new(expr: Expression<NotAnalyzed>, index: Expression<NotAnalyzed>, span: Span) -> Self {
		Self {
			expr: Box::new(expr),
			index: Box::new(index),
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
			Expression<NotAnalyzed>,
		) -> Result<
			(
				Expression<Analyzed>,
				Expression<Analyzed>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<IndexOf<Analyzed>, TypeError> {
		let (expr, index, datatype, exit_status) = analyze(*self.expr, *self.index)?;
		Ok(IndexOf {
			expr: Box::new(expr),
			index: Box::new(index),
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl IndexOf<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for IndexOf<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// PropertyOf //

#[derive(Debug)]
pub struct PropertyOf<T: IsAnalyzed> {
	expr: Box<Expression<T>>,
	name: String,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> PropertyOf<T> {
	pub fn expression(&self) -> &Expression<T> {
		&self.expr
	}

	pub fn name(&self) -> &String {
		&self.name
	}
}

impl PropertyOf<NotAnalyzed> {
	pub fn new(expr: Expression<NotAnalyzed>, name: String, span: Span) -> PropertyOf<NotAnalyzed> {
		Self {
			expr: Box::new(expr),
			name,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
		) -> Result<(Expression<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<PropertyOf<Analyzed>, TypeError> {
		let (expr, datatype, exit_status) = analyze(*self.expr)?;
		Ok(PropertyOf {
			expr: Box::new(expr),
			name: self.name,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl PropertyOf<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for PropertyOf<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Call //

#[derive(Debug)]
pub struct Call<T: IsAnalyzed> {
	expr: Box<Expression<T>>,
	args: Vec<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Call<T> {
	pub fn expression(&self) -> &Expression<T> {
		&self.expr
	}

	pub fn arguments(&self) -> &Vec<Expression<T>> {
		&self.args
	}
}

impl Call<NotAnalyzed> {
	pub fn new(
		expr: Expression<NotAnalyzed>,
		args: Vec<Expression<NotAnalyzed>>,
		span: Span,
	) -> Self {
		Self {
			expr: Box::new(expr),
			args,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
			Vec<Expression<NotAnalyzed>>,
		) -> Result<
			(
				Expression<Analyzed>,
				Vec<Expression<Analyzed>>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<Call<Analyzed>, TypeError> {
		let (expr, args, datatype, exit_status) = analyze(*self.expr, self.args)?;
		Ok(Call {
			expr: Box::new(expr),
			args,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl Call<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Call<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// NamedCall //

#[derive(Debug)]
pub struct NamedCall<T: IsAnalyzed> {
	expr: Box<Expression<T>>,
	name: String,
	args: Vec<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> NamedCall<T> {
	pub fn expression(&self) -> &Expression<T> {
		self.expr.as_ref()
	}

	pub fn name(&self) -> &String {
		&self.name
	}

	pub fn arguments(&self) -> &Vec<Expression<T>> {
		&self.args
	}
}

impl NamedCall<NotAnalyzed> {
	pub fn new(
		expr: Expression<NotAnalyzed>,
		name: String,
		args: Vec<Expression<NotAnalyzed>>,
		span: Span,
	) -> Self {
		Self {
			expr: Box::new(expr),
			name,
			args,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
			Vec<Expression<NotAnalyzed>>,
		) -> Result<
			(
				Expression<Analyzed>,
				Vec<Expression<Analyzed>>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<NamedCall<Analyzed>, TypeError> {
		let (expr, args, datatype, exit_status) = analyze(*self.expr, self.args)?;
		Ok(NamedCall {
			expr: Box::new(expr),
			name: self.name,
			args,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl NamedCall<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for NamedCall<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Unary //

#[derive(Debug, Clone)]
pub enum UnaryOperator {
	Minus,
	Not,
}

#[derive(Debug)]
pub struct Unary<T: IsAnalyzed> {
	operator: UnaryOperator,
	operand: Box<Expression<T>>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Unary<T> {
	pub fn operator(&self) -> &UnaryOperator {
		&self.operator
	}

	pub fn operand(&self) -> &Expression<T> {
		&self.operand
	}
}

impl Unary<NotAnalyzed> {
	pub fn new(operator: UnaryOperator, operand: Expression<NotAnalyzed>, span: Span) -> Self {
		Self {
			operator,
			operand: Box::new(operand),
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
		) -> Result<(Expression<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Unary<Analyzed>, TypeError> {
		let (operand, datatype, exit_status) = analyze(*self.operand)?;
		Ok(Unary {
			operator: self.operator,
			operand: Box::new(operand),
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl Unary<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Unary<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Binary //

#[derive(Debug, Clone)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide,
	Exponentiate,
	Mod,
	And,
	Or,
	Equal,
	NotEqual,
	GreaterThan,
	GreaterThanOrEqual,
	LessThan,
	LessThanOrEqual,
}

#[derive(Debug)]
pub struct Binary<T: IsAnalyzed> {
	operator: BinaryOperator,
	operands: Box<(Expression<T>, Expression<T>)>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> Binary<T> {
	pub fn operator(&self) -> &BinaryOperator {
		&self.operator
	}

	pub fn operands(&self) -> &(Expression<T>, Expression<T>) {
		&self.operands
	}
}

impl Binary<NotAnalyzed> {
	pub fn new(
		operator: BinaryOperator,
		operands: (Expression<NotAnalyzed>, Expression<NotAnalyzed>),
		span: Span,
	) -> Self {
		Self {
			operator,
			operands: Box::new(operands),
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Expression<NotAnalyzed>,
			Expression<NotAnalyzed>,
		) -> Result<
			(
				Expression<Analyzed>,
				Expression<Analyzed>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<Binary<Analyzed>, TypeError> {
		let operands = *self.operands;
		let (operand1, operand2, datatype, exit_status) = analyze(operands.0, operands.1)?;
		Ok(Binary {
			operator: self.operator,
			operands: Box::new((operand1, operand2)),
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl Binary<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Binary<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Expression //

#[derive(Debug)]
pub enum Expression<T: IsAnalyzed> {
	Statement(Box<Statement<T>>),
	Parenthesis(Parenthesis<T>),
	Literal(Literal<T>),
	Array(Array<T>),
	Variable(Variable<T>),
	IndexOf(IndexOf<T>),
	PropertyOf(PropertyOf<T>),
	Call(Call<T>),
	NamedCall(NamedCall<T>),
	Unary(Unary<T>),
	Binary(Binary<T>),
}

impl Expression<Analyzed> {
	pub fn ty(&self) -> TyRef {
		match self {
			Expression::Statement(n) => n.ty(),
			Expression::Parenthesis(n) => n.ty(),
			Expression::Literal(n) => n.ty(),
			Expression::Array(n) => n.ty(),
			Expression::Variable(n) => n.ty(),
			Expression::IndexOf(n) => n.ty(),
			Expression::PropertyOf(n) => n.ty(),
			Expression::Call(n) => n.ty(),
			Expression::NamedCall(n) => n.ty(),
			Expression::Unary(n) => n.ty(),
			Expression::Binary(n) => n.ty(),
		}
	}

	pub fn exit_status(&self) -> &ExitStatus {
		match self {
			Expression::Statement(n) => n.exit_status(),
			Expression::Parenthesis(n) => n.exit_status(),
			Expression::Literal(n) => n.exit_status(),
			Expression::Array(n) => n.exit_status(),
			Expression::Variable(n) => n.exit_status(),
			Expression::IndexOf(n) => n.exit_status(),
			Expression::PropertyOf(n) => n.exit_status(),
			Expression::Call(n) => n.exit_status(),
			Expression::NamedCall(n) => n.exit_status(),
			Expression::Unary(n) => n.exit_status(),
			Expression::Binary(n) => n.exit_status(),
		}
	}
}

impl<T: IsAnalyzed> Node for Expression<T> {
	fn span(&self) -> &Span {
		match &self {
			Expression::Statement(n) => n.span(),
			Expression::Parenthesis(n) => n.span(),
			Expression::Literal(n) => n.span(),
			Expression::Array(n) => n.span(),
			Expression::Variable(n) => n.span(),
			Expression::IndexOf(n) => n.span(),
			Expression::PropertyOf(n) => n.span(),
			Expression::Call(n) => n.span(),
			Expression::NamedCall(n) => n.span(),
			Expression::Unary(n) => n.span(),
			Expression::Binary(n) => n.span(),
		}
	}
}

// DT //

#[derive(Debug, Clone)]
pub enum DTKind<T: IsAnalyzed> {
	Void(Span),
	Int(Span),
	Float(Span),
	String(Span),
	Boolean(Span),
	Array(Box<DT<T>>, Span),
}

impl<T: IsAnalyzed> DTKind<T> {
	pub fn span(&self) -> &Span {
		match self {
			DTKind::Void(s) => s,
			DTKind::Int(s) => s,
			DTKind::Float(s) => s,
			DTKind::String(s) => s,
			DTKind::Boolean(s) => s,
			DTKind::Array(_, s) => s,
		}
	}
}

#[derive(Debug, Clone)]
pub struct DT<T: IsAnalyzed> {
	kind: DTKind<T>,
	ty: Option<TyRef>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> DT<T> {
	pub fn kind(&self) -> &DTKind<T> {
		&self.kind
	}
}

impl DT<NotAnalyzed> {
	pub fn new(kind: DTKind<NotAnalyzed>) -> Self {
		Self {
			kind,
			ty: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(DTKind<NotAnalyzed>) -> Result<(TyRef, DTKind<Analyzed>), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<DT<Analyzed>, TypeError> {
		let (ty, kind) = analyze(self.kind)?;
		Ok(DT {
			ty: Some(ty),
			kind,
			phantom: PhantomData,
		})
	}
}

impl DT<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for DT<T> {
	fn span(&self) -> &Span {
		self.kind.span()
	}
}

// Return //

#[derive(Debug)]
pub struct Return<T: IsAnalyzed> {
	expression: Option<Expression<T>>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Return<T> {
	pub fn expression(&self) -> &Option<Expression<T>> {
		&self.expression
	}
}

impl Return<NotAnalyzed> {
	pub fn new(expression: Option<Expression<NotAnalyzed>>, span: Span) -> Self {
		Self {
			expression,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Option<Expression<NotAnalyzed>>,
		) -> Result<(Option<Expression<Analyzed>>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Return<Analyzed>, TypeError> {
		let (expression, datatype, exit_status) = analyze(self.expression)?;
		Ok(Return {
			expression,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Return<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Return<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Break //

#[derive(Debug)]
pub struct Break<T: IsAnalyzed> {
	expression: Option<Expression<T>>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Break<T> {
	pub fn expression(&self) -> &Option<Expression<T>> {
		&self.expression
	}
}

impl Break<NotAnalyzed> {
	pub fn new(expression: Option<Expression<NotAnalyzed>>, span: Span) -> Self {
		Self {
			expression,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Option<Expression<NotAnalyzed>>,
		) -> Result<(Option<Expression<Analyzed>>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Break<Analyzed>, TypeError> {
		let (expression, datatype, exit_status) = analyze(self.expression)?;
		Ok(Break {
			expression,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Break<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Break<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// LVariable //

#[derive(Debug)]
pub struct LVariable<T: IsAnalyzed> {
	name: String,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> LVariable<T> {
	pub fn name(&self) -> &String {
		&self.name
	}
}

impl LVariable<NotAnalyzed> {
	pub fn new(name: String, span: Span) -> Self {
		Self {
			name,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze(self, ty: TyRef, exit_status: ExitStatus) -> LVariable<Analyzed> {
		LVariable {
			name: self.name,
			span: self.span,
			ty: Some(ty),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		}
	}
}

impl LVariable<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for LVariable<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// LIndexOf //

#[derive(Debug)]
pub struct LIndexOf<T: IsAnalyzed> {
	lvalue: Box<LValue<T>>,
	index: Expression<T>,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> LIndexOf<T> {
	pub fn lvalue(&self) -> &LValue<T> {
		&self.lvalue
	}

	pub fn index(&self) -> &Expression<T> {
		&self.index
	}
}

impl LIndexOf<NotAnalyzed> {
	pub fn new(lvalue: LValue<NotAnalyzed>, index: Expression<NotAnalyzed>, span: Span) -> Self {
		Self {
			lvalue: Box::new(lvalue),
			index,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(
			LValue<NotAnalyzed>,
			Expression<NotAnalyzed>,
		) -> Result<(LValue<Analyzed>, Expression<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<LIndexOf<Analyzed>, TypeError> {
		let (lvalue, index, datatype, exit_status) = analyze(*self.lvalue, self.index)?;
		Ok(LIndexOf {
			lvalue: Box::new(lvalue),
			index,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl LIndexOf<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for LIndexOf<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// LPropertyOf //

#[derive(Debug)]
pub struct LPropertyOf<T: IsAnalyzed> {
	lvalue: Box<LValue<T>>,
	name: String,
	span: Span,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	phantom: PhantomData<T>,
}

impl<T: IsAnalyzed> LPropertyOf<T> {
	pub fn lvalue(&self) -> &LValue<T> {
		&self.lvalue
	}

	pub fn name(&self) -> &String {
		&self.name
	}
}

impl LPropertyOf<NotAnalyzed> {
	pub fn new(lvalue: LValue<NotAnalyzed>, name: String, span: Span) -> LPropertyOf<NotAnalyzed> {
		Self {
			lvalue: Box::new(lvalue),
			name,
			span,
			ty: None,
			exit_status: None,
			phantom: PhantomData,
		}
	}

	pub fn analyze<
		T: FnOnce(LValue<NotAnalyzed>) -> Result<(LValue<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<LPropertyOf<Analyzed>, TypeError> {
		let (lvalue, datatype, exit_status) = analyze(*self.lvalue)?;
		Ok(LPropertyOf {
			lvalue: Box::new(lvalue),
			name: self.name,
			span: self.span,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			phantom: PhantomData,
		})
	}
}

impl LPropertyOf<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for LPropertyOf<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// LValue //

#[derive(Debug)]
pub enum LValue<T: IsAnalyzed> {
	Variable(LVariable<T>),
	IndexOf(LIndexOf<T>),
	PropertyOf(LPropertyOf<T>),
}

impl LValue<Analyzed> {
	pub fn ty(&self) -> TyRef {
		match self {
			LValue::Variable(l) => l.ty(),
			LValue::IndexOf(l) => l.ty(),
			LValue::PropertyOf(l) => l.ty(),
		}
	}

	pub fn exit_status(&self) -> &ExitStatus {
		match self {
			LValue::Variable(l) => l.exit_status(),
			LValue::IndexOf(l) => l.exit_status(),
			LValue::PropertyOf(l) => l.exit_status(),
		}
	}
}

impl<T: IsAnalyzed> Node for LValue<T> {
	fn span(&self) -> &Span {
		match self {
			LValue::Variable(l) => l.span(),
			LValue::IndexOf(l) => l.span(),
			LValue::PropertyOf(l) => l.span(),
		}
	}
}

// Let //

#[derive(Debug)]
pub struct Let<T: IsAnalyzed> {
	mutable: bool,
	name: String,
	dt: Option<DT<T>>,
	value: Expression<T>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Let<T> {
	pub fn mutable(&self) -> bool {
		self.mutable
	}

	pub fn name(&self) -> &String {
		&self.name
	}

	pub fn dt(&self) -> &Option<DT<T>> {
		&self.dt
	}

	pub fn value(&self) -> &Expression<T> {
		&self.value
	}
}

impl Let<NotAnalyzed> {
	pub fn new(
		mutable: bool,
		name: String,
		dt: Option<DT<NotAnalyzed>>,
		value: Expression<NotAnalyzed>,
		span: Span,
	) -> Self {
		Self {
			mutable,
			name,
			dt,
			value,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Option<DT<NotAnalyzed>>,
			Expression<NotAnalyzed>,
		) -> Result<
			(
				Option<DT<Analyzed>>,
				Expression<Analyzed>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<Let<Analyzed>, TypeError> {
		let (dt, value, datatype, exit_status) = analyze(self.dt, self.value)?;
		Ok(Let {
			mutable: self.mutable,
			name: self.name,
			dt,
			value,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Let<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Let<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Assign //

#[derive(Debug)]
pub struct Assign<T: IsAnalyzed> {
	lvalue: LValue<T>,
	value: Expression<T>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Assign<T> {
	pub fn lvalue(&self) -> &LValue<T> {
		&self.lvalue
	}

	pub fn value(&self) -> &Expression<T> {
		&self.value
	}
}

impl Assign<NotAnalyzed> {
	pub fn new(lvalue: LValue<NotAnalyzed>, value: Expression<NotAnalyzed>, span: Span) -> Self {
		Self {
			lvalue,
			value,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			LValue<NotAnalyzed>,
			Expression<NotAnalyzed>,
		) -> Result<(LValue<Analyzed>, Expression<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Assign<Analyzed>, TypeError> {
		let (lvalue, value, datatype, exit_status) = analyze(self.lvalue, self.value)?;
		Ok(Assign {
			lvalue,
			value,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Assign<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Assign<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// If //

#[derive(Debug)]
pub struct If<T: IsAnalyzed> {
	if_branch: (Expression<T>, Block<T>, Span),
	else_if_branch: Vec<(Expression<T>, Block<T>, Span)>,
	else_branch: Option<(Block<T>, Span)>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> If<T> {
	pub fn if_branch(&self) -> &(Expression<T>, Block<T>, Span) {
		&self.if_branch
	}

	pub fn else_if_branch(&self) -> &Vec<(Expression<T>, Block<T>, Span)> {
		&self.else_if_branch
	}

	pub fn else_branch(&self) -> &Option<(Block<T>, Span)> {
		&self.else_branch
	}
}

impl If<NotAnalyzed> {
	pub fn new(
		if_branch: (Expression<NotAnalyzed>, Block<NotAnalyzed>, Span),
		else_if_branch: Vec<(Expression<NotAnalyzed>, Block<NotAnalyzed>, Span)>,
		else_branch: Option<(Block<NotAnalyzed>, Span)>,
		span: Span,
	) -> Self {
		Self {
			if_branch,
			else_if_branch,
			else_branch,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			(Expression<NotAnalyzed>, Block<NotAnalyzed>),
			Vec<(Expression<NotAnalyzed>, Block<NotAnalyzed>)>,
			Option<Block<NotAnalyzed>>,
		) -> Result<
			(
				(Expression<Analyzed>, Block<Analyzed>),
				Vec<(Expression<Analyzed>, Block<Analyzed>)>,
				Option<Block<Analyzed>>,
				TyRef,
				ExitStatus,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<If<Analyzed>, TypeError> {
		let else_if_spans;
		let else_span;
		let (if_branch, mut else_if_branch, else_branch, datatype, exit_status) = analyze(
			(self.if_branch.0, self.if_branch.1),
			{
				let mut v = Vec::with_capacity(self.else_if_branch.len());
				let mut spans = Vec::with_capacity(self.else_if_branch.len());
				for branch in self.else_if_branch {
					v.push((branch.0, branch.1));
					spans.push(branch.2);
				}
				else_if_spans = spans;
				v
			},
			if let Some(else_branch) = self.else_branch {
				else_span = Some(else_branch.1);
				Some(else_branch.0)
			} else {
				else_span = None;
				None
			},
		)?;
		Ok(If {
			if_branch: (if_branch.0, if_branch.1, self.if_branch.2),
			else_if_branch: {
				let mut v = Vec::with_capacity(else_if_spans.len());
				for span in else_if_spans {
					let branch = else_if_branch.remove(0);
					v.push((branch.0, branch.1, span));
				}
				v
			},
			else_branch: if let Some(branch) = else_branch {
				Some((branch, else_span.unwrap()))
			} else {
				None
			},
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl If<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for If<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Loop //

#[derive(Debug)]
pub struct Loop<T: IsAnalyzed> {
	block: Block<T>,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Loop<T> {
	pub fn block(&self) -> &Block<T> {
		&self.block
	}
}

impl Loop<NotAnalyzed> {
	pub fn new(block: Block<NotAnalyzed>, span: Span) -> Self {
		Self {
			block,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(Block<NotAnalyzed>) -> Result<(Block<Analyzed>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Loop<Analyzed>, TypeError> {
		let (block, datatype, exit_status) = analyze(self.block)?;
		Ok(Loop {
			block,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Loop<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Loop<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Block //

#[derive(Debug)]
pub struct Block<T: IsAnalyzed> {
	statements: Vec<Statement<T>>,
	does_eval: bool,
	ty: Option<TyRef>,
	exit_status: Option<ExitStatus>,
	span: Span,
}

impl<T: IsAnalyzed> Block<T> {
	pub fn statements(&self) -> &Vec<Statement<T>> {
		&self.statements
	}

	pub fn does_eval(&self) -> bool {
		self.does_eval
	}
}

impl Block<NotAnalyzed> {
	pub fn new(statements: Vec<Statement<NotAnalyzed>>, does_eval: bool, span: Span) -> Self {
		Self {
			statements,
			does_eval,
			ty: None,
			exit_status: None,
			span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Vec<Statement<NotAnalyzed>>,
		) -> Result<(Vec<Statement<Analyzed>>, TyRef, ExitStatus), TypeError>,
	>(
		self,
		analyze: T,
	) -> Result<Block<Analyzed>, TypeError> {
		let (stmts, datatype, exit_status) = analyze(self.statements)?;
		Ok(Block {
			statements: stmts,
			does_eval: self.does_eval,
			ty: Some(datatype),
			exit_status: Some(exit_status),
			span: self.span,
		})
	}
}

impl Block<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}

	pub fn exit_status(&self) -> &ExitStatus {
		self.exit_status.as_ref().clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Block<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// Statement //

#[derive(Debug)]
pub enum Statement<T: IsAnalyzed> {
	Block(Block<T>),
	Expression(Expression<T>),
	Return(Return<T>),
	Break(Break<T>),
	Let(Let<T>),
	Assign(Assign<T>),
	If(If<T>),
	// Match(Match<T>),
	Loop(Loop<T>),
	// While(While<T>),
}

impl Statement<Analyzed> {
	pub fn ty(&self) -> TyRef {
		match self {
			Statement::Block(s) => s.ty(),
			Statement::Expression(s) => s.ty(),
			Statement::Return(s) => s.ty(),
			Statement::Break(s) => s.ty(),
			Statement::Let(s) => s.ty(),
			Statement::Assign(s) => s.ty(),
			Statement::If(s) => s.ty(),
			Statement::Loop(s) => s.ty(),
		}
	}

	pub fn exit_status(&self) -> &ExitStatus {
		match self {
			Statement::Block(s) => s.exit_status(),
			Statement::Expression(s) => s.exit_status(),
			Statement::Return(s) => s.exit_status(),
			Statement::Break(s) => s.exit_status(),
			Statement::Let(s) => s.exit_status(),
			Statement::Assign(s) => s.exit_status(),
			Statement::If(s) => s.exit_status(),
			Statement::Loop(s) => s.exit_status(),
		}
	}
}

impl<T: IsAnalyzed> Node for Statement<T> {
	fn span(&self) -> &Span {
		match self {
			Statement::Block(s) => s.span(),
			Statement::Expression(s) => s.span(),
			Statement::Return(s) => s.span(),
			Statement::Break(s) => s.span(),
			Statement::Let(s) => s.span(),
			Statement::Assign(s) => s.span(),
			Statement::If(s) => s.span(),
			Statement::Loop(s) => s.span(),
		}
	}
}

// Function //

#[derive(Debug)]
pub struct Function<T: IsAnalyzed> {
	name: String,
	parameters: Vec<(String, DT<T>)>,
	return_dt: Option<DT<T>>,
	body: Block<T>,
	ty: Option<TyRef>,
	span: Span,
	signature_span: Span,
}

impl<T: IsAnalyzed> Function<T> {
	pub fn name(&self) -> &String {
		&self.name
	}

	pub fn parameters(&self) -> &Vec<(String, DT<T>)> {
		&self.parameters
	}

	pub fn return_dt(&self) -> Option<&DT<T>> {
		self.return_dt.as_ref().clone()
	}

	pub fn body(&self) -> &Block<T> {
		&self.body
	}

	pub fn signature_span(&self) -> &Span {
		&self.signature_span
	}
}

impl Function<NotAnalyzed> {
	pub fn new(
		name: String,
		parameters: Vec<(String, DT<NotAnalyzed>)>,
		return_dt: Option<DT<NotAnalyzed>>,
		body: Block<NotAnalyzed>,
		span: Span,
		signature_span: Span,
	) -> Self {
		Self {
			name,
			parameters,
			return_dt,
			body,
			ty: None,
			span,
			signature_span,
		}
	}

	pub fn analyze<
		T: FnOnce(
			Vec<(String, DT<NotAnalyzed>)>,
			Option<DT<NotAnalyzed>>,
			Block<NotAnalyzed>,
		) -> Result<
			(
				Vec<(String, DT<Analyzed>)>,
				Option<DT<Analyzed>>,
				Block<Analyzed>,
				TyRef,
			),
			TypeError,
		>,
	>(
		self,
		analyze: T,
	) -> Result<Function<Analyzed>, TypeError> {
		let (parameters, return_dt, body, datatype) =
			analyze(self.parameters, self.return_dt, self.body)?;
		Ok(Function {
			name: self.name,
			parameters,
			return_dt,
			body,
			ty: Some(datatype),
			span: self.span,
			signature_span: self.signature_span,
		})
	}
}

impl Function<Analyzed> {
	pub fn ty(&self) -> TyRef {
		self.ty.clone().unwrap()
	}
}

impl<T: IsAnalyzed> Node for Function<T> {
	fn span(&self) -> &Span {
		&self.span
	}
}

// TopLevel //

#[derive(Debug)]
pub enum TopLevel<T: IsAnalyzed> {
	Function(Function<T>),
}

impl TopLevel<Analyzed> {
	pub fn ty(&self) -> TyRef {
		match self {
			TopLevel::Function(t) => t.ty(),
		}
	}
}

impl<T: IsAnalyzed> Node for TopLevel<T> {
	fn span(&self) -> &Span {
		match self {
			TopLevel::Function(t) => t.span(),
		}
	}
}

// AST //

/// Encodes all the user' Ares source code
/// in an accessible way.
pub type AST<T> = Vec<TopLevel<T>>;
