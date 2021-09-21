use std::collections::HashMap;

use ares_core::Span;

use super::TypeError;

/*
	Data in Ares can come in various forms, such as numbers, text,
	booleans etc. This fact is formally acknowledged with the concept
	of data types. The `Ty` enumerator defined below contains all the
	data types in Ares. During analysis types are assigned to expressions.
	In some cases the data type of an expression can be obvious but in times
	where it's not inference is used. Uncertainty about a data type can be modelled
	using the data structures defined below and this information can then be used
	to attempt to infer data types for places where the data type is unknown.
	Certain data types are those where the data type is fully known. The goal of
	analysis is to, using inference, convert all uncertain data types into certain
	data types. Analyses are within analyses contexts, `Ctx`.
*/

/// A reference to a type in the context.
#[derive(Debug, Clone)]
pub struct TyRef(usize);

impl TyRef {
	/// ID of the type being referenced in the context.
	pub fn id(&self) -> usize {
		self.0
	}
}

#[derive(Debug, Clone)]
pub enum Ty {
	/// `Int`, primitive integer type.
	/// Positive and negative whole numbers. (`i64`)
	Int(Span),
	/// `Float`, primitive float type.
	/// Positive and negative floating point numbers. (`f64`)
	Float(Span),
	/// `String`, primitive string type.
	/// A chain of characters. (`String`)
	String(Span),
	/// `Boolean`, primitive boolean type.
	/// `true` or `false`.
	Boolean(Span),
	/// `Void`, primitive void type.
	/// Represents the lack of data.
	Void(Span),
	/// `Never`, the never type marks values which will never be evaluated.
	/// Since the associated data never gets evaluated this type should
	/// never get compared to any other type.
	Never(Span),
	/// `?`, the unknown type is a placeholder for a certain type which
	/// is currently unknown. During analysis when enough information
	/// is gained these unknowns will be replaced with the true certain type.
	/// Failure to do so will result in a type error due to uncertainty.
	/// An ID is associated with every unknown to be able to refer to
	/// a conceptually single unknown in multiple places.
	Unknown(usize, Span),
	/// `Ref(T)`, A reference to another type in the current context. This type
	/// is used to communicate a link between 2 types. This type is
	/// just a placeholder and should be resolved away during analysis.
	/// References can always be effectively substituted with the
	/// referenced type.
	TyRef(TyRef),
	/// `Array<T>`, non-primitive array type.
	/// Can store multiple values of the same type.
	Array(Box<Ty>, Span),
	/// `T || U || V || ...`, This type is a step up from the unknown type.
	/// This type gives multiple possible types to it's true underlying type.
	/// During analysis, like with unknown, this type will be reduced away
	/// into one certain type and failure to do so will cause an error.
	Possibility(usize, Vec<(Vec<usize>, Ty)>, Span),
	/// `T | U | V | ...`, non-primitive union type.
	/// This type communicates that the associated value will
	/// be one of the unions' variants and which variant it is, is by design,
	/// unknown. This slight difference makes this type not reducible
	/// as the data being referred can inherently be any one of the variants.
	/// This type isn't treated as uncertain as their uncertainty is to be
	/// embraced.
	Union(Vec<Ty>, Span),
	/// `Fn(T, U, V, ...) -> X`, non-primitive function type.
	/// This type contains all the parameters and the return type information
	/// of a function.
	Fn(Vec<Ty>, Box<Ty>, Span),
}

impl Ty {
	pub fn span(&self) -> &Span {
		match self {
			Ty::Int(s) => s,
			Ty::Float(s) => s,
			Ty::String(s) => s,
			Ty::Boolean(s) => s,
			Ty::Void(s) => s,
			Ty::Never(s) => s,
			Ty::Unknown(_, s) => s,
			Ty::TyRef(_) => panic!("Cannot find span of a type reference."),
			Ty::Array(_, s) => s,
			Ty::Possibility(_, _, s) => s,
			Ty::Union(_, s) => s,
			Ty::Fn(_, _, s) => s,
		}
	}

	pub fn to_string(&self) -> String {
		match self {
			Ty::Int(_) => "Int".to_string(),
			Ty::Float(_) => "Float".to_string(),
			Ty::String(_) => "String".to_string(),
			Ty::Boolean(_) => "Boolean".to_string(),
			Ty::Void(_) => "Void".to_string(),
			Ty::Never(_) => "Never".to_string(),
			Ty::Unknown(_, _) => "?".to_string(),
			Ty::TyRef(_) => panic!("Cannot convert a type reference to a string."),
			Ty::Array(ty, _) => format!("Array<{}>", ty.to_string()),
			Ty::Possibility(_, v, _) => v
				.iter()
				.map(|(_, ty)| ty.to_string())
				.collect::<Vec<_>>()
				.join(" || "),
			Ty::Union(v, _) => v
				.iter()
				.map(|ty| ty.to_string())
				.collect::<Vec<_>>()
				.join(" | "),
			Ty::Fn(p, r, _) => format!(
				"Fn({}) -> {}",
				p.iter()
					.map(|ty| ty.to_string())
					.collect::<Vec<_>>()
					.join(", "),
				r.to_string()
			),
		}
	}

	/// Checks whether this type is certain.
	/// (Type references are considered uncertain.)
	pub fn is_certain(&self) -> bool {
		match self {
			Ty::Int(_) => true,
			Ty::Float(_) => true,
			Ty::String(_) => true,
			Ty::Boolean(_) => true,
			Ty::Void(_) => true,
			Ty::Never(_) => true,
			Ty::Unknown(_, _) => false,
			Ty::TyRef(_) => false,
			Ty::Array(t, _) => t.is_certain(),
			Ty::Possibility(_, _, _) => false,
			Ty::Union(ts, _) => ts.iter().all(|t| t.is_certain()),
			Ty::Fn(ts, t, _) => t.is_certain() && ts.iter().all(|t| t.is_certain()),
		}
	}

	/// Remove possibilities with the provided ids.
	pub fn remove_possibilities<'a>(self, ids: &[usize]) -> Result<Self, TypeError> {
		match self {
			Ty::Int(_) => Ok(self),
			Ty::Float(_) => Ok(self),
			Ty::String(_) => Ok(self),
			Ty::Boolean(_) => Ok(self),
			Ty::Void(_) => Ok(self),
			Ty::Never(_) => Ok(self),
			Ty::Unknown(_, _) => Ok(self),
			Ty::TyRef(_) => Ok(self),
			Ty::Array(t, s) => Ok(Ty::Array(Box::new(t.remove_possibilities(ids)?), s)),
			Ty::Possibility(pid, ts, s) => Ok(Ty::Possibility(
				pid,
				{
					// Filter out all the variants whose IDs contain the ID provided.
					let ts: Vec<_> = ts
						.into_iter()
						.filter(|(id, _)| !ids.iter().any(|x| id.contains(x)))
						.collect();
					// If all the variants were filtered out return an error.
					if ts.len() == 0 {
						return Err(TypeError::Unsolvable(s));
					}
					let mut ts_final = Vec::with_capacity(ts.len());
					for (id, t) in ts {
						ts_final.push((id, t.remove_possibilities(ids)?));
					}
					ts_final
				},
				s,
			)),
			Ty::Union(ts, s) => Ok(Ty::Union(
				{
					let mut ts_final = Vec::with_capacity(ts.len());
					for t in ts {
						ts_final.push(t.remove_possibilities(ids)?);
					}
					ts_final
				},
				s,
			)),
			Ty::Fn(ts, t, s) => Ok(Ty::Fn(
				{
					let mut ts_final = Vec::with_capacity(ts.len());
					for t in ts {
						ts_final.push(t.remove_possibilities(ids)?);
					}
					ts_final
				},
				Box::new(t.remove_possibilities(ids)?),
				s,
			)),
		}
	}

	/// Replace all unknowns with a provided type.
	pub fn resolve_unknown(self, id: usize, ty: Ty) -> Self {
		match self {
			Ty::Int(_) => self,
			Ty::Float(_) => self,
			Ty::String(_) => self,
			Ty::Boolean(_) => self,
			Ty::Void(_) => self,
			Ty::Never(_) => self,
			Ty::Unknown(x, _) => {
				if id == x {
					ty
				} else {
					self
				}
			}
			Ty::TyRef(_) => self,
			Ty::Array(t, s) => Ty::Array(Box::new(t.resolve_unknown(id, ty)), s),
			Ty::Possibility(pid, ts, s) => Ty::Possibility(
				pid,
				ts.into_iter()
					.map(|(ids, t)| (ids, t.resolve_unknown(id, ty.clone())))
					.collect(),
				s,
			),
			Ty::Union(ts, s) => Ty::Union(
				ts.into_iter()
					.map(|t| t.resolve_unknown(id, ty.clone()))
					.collect(),
				s,
			),
			Ty::Fn(ts, t, s) => Ty::Fn(
				ts.into_iter()
					.map(|t| t.resolve_unknown(id, ty.clone()))
					.collect(),
				Box::new(t.resolve_unknown(id, ty)),
				s,
			),
		}
	}
}

impl PartialEq for Ty {
	/// Checks whether 2 reference-free types are identical.
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(_, Ty::TyRef(_)) | (Ty::TyRef(_), _) => panic!("cannot equate reference types"),
			(_, Ty::Never(_)) | (Ty::Never(_), _) => panic!("cannot equate never types"),
			(Ty::Unknown(id1, _), Ty::Unknown(id2, _)) => id1 == id2,
			(Ty::Possibility(pid1, _, _), Ty::Possibility(pid2, _, _)) => pid1 == pid2,
			(Ty::Int(_), Ty::Int(_)) => true,
			(Ty::Float(_), Ty::Float(_)) => true,
			(Ty::String(_), Ty::String(_)) => true,
			(Ty::Boolean(_), Ty::Boolean(_)) => true,
			(Ty::Void(_), Ty::Void(_)) => true,
			(Ty::Array(t1, _), Ty::Array(t2, _)) => t1 == t2,
			(Ty::Fn(ts1, t1, _), Ty::Fn(ts2, t2, _)) => {
				t1 == t2 && ts1.len() == ts2.len() && ts1.iter().zip(ts2).all(|(t1, t2)| t1 == t2)
			}
			(Ty::Union(ts1, _), Ty::Union(ts2, _)) => {
				ts1.iter().all(|t1| ts2.iter().any(|t2| t1 == t2))
					&& ts2.iter().all(|t2| ts1.iter().any(|t1| t1 == t2))
			}
			(_, Ty::Possibility(_, _, _))
			| (Ty::Possibility(_, _, _), _)
			| (_, Ty::Unknown(_, _))
			| (Ty::Unknown(_, _), _)
			| (_, Ty::Int(_))
			| (Ty::Int(_), _)
			| (_, Ty::Float(_))
			| (Ty::Float(_), _)
			| (_, Ty::String(_))
			| (Ty::String(_), _)
			| (_, Ty::Boolean(_))
			| (Ty::Boolean(_), _)
			| (_, Ty::Void(_))
			| (Ty::Void(_), _)
			| (_, Ty::Array(_, _))
			| (Ty::Array(_, _), _)
			| (_, Ty::Union(_, _))
			| (Ty::Union(_, _), _) => false,
		}
	}
}

/// An analysis context.
/// Keeps track of all the data types of all the data
/// and keeps track of all the constraints that must
/// be obeyed.
#[derive(Debug)]
pub struct Ctx {
	tys: Vec<Option<Ty>>,
	constraints: Option<Vec<((Ty, Ty), Span)>>,
	unknowns: usize,
	possibilities: usize,
}

impl Ctx {
	pub fn new() -> Self {
		Self {
			tys: vec![],
			constraints: Some(vec![]),
			unknowns: 0,
			possibilities: 0,
		}
	}

	/// Take ownership of all the constraints.
	pub fn take_constraints(&mut self) -> Vec<((Ty, Ty), Span)> {
		self.constraints.take().unwrap()
	}

	/// Add a new type to the context.
	pub fn new_ty(&mut self, ty: Ty) -> TyRef {
		self.tys.push(Some(ty));
		TyRef(self.tys.len() - 1)
	}

	/// Add a new unknown to the context.
	pub fn new_unknown(&mut self, span: Span) -> Ty {
		println!("Unknown Created: {}, {:?}", self.unknowns, span);
		self.unknowns += 1;
		Ty::Unknown(self.unknowns - 1, span)
	}

	/// Add a new possibility to the context.
	pub fn new_possibility(&mut self, variants: Vec<Ty>, span: Span) -> Ty {
		let variants = variants
			.into_iter()
			.map(|variant| {
				self.possibilities += 1;
				(vec![self.possibilities - 1], variant)
			})
			.collect();
		self.possibilities += 1;
		Ty::Possibility(self.possibilities - 1, variants, span)
	}

	/// Add a constraint which requires the 2 types provided
	/// to be equal to satisfy, `(lhs, rhs)`.
	pub fn add_constraint(&mut self, constraint: (Ty, Ty), span: Span) {
		let lhs = self.new_ty(constraint.0);
		let rhs = self.new_ty(constraint.1);
		self.constraints
			.as_mut()
			.unwrap()
			.push(((Ty::TyRef(lhs), Ty::TyRef(rhs)), span));
	}

	/// Add a new possibility with pre-existing ids to the context.
	fn new_possibility_ext(&mut self, variants: Vec<(Vec<usize>, Ty)>, span: Span) -> Ty {
		self.possibilities += 1;
		Ty::Possibility(self.possibilities - 1, variants, span)
	}

	/// Get access to an entry in the context.
	pub fn resolve_ref(&self, ty: &TyRef) -> &Ty {
		self.tys[ty.id()].as_ref().unwrap()
	}

	/// Remove possibilities with the provided id.
	pub fn remove_possibilities(&mut self, ids: &[usize]) -> Result<(), TypeError> {
		for entry in self.tys.iter_mut() {
			let t = entry.take().unwrap();
			entry.replace(t.remove_possibilities(ids)?);
		}
		Ok(())
	}

	/// Replace all unknowns in the context with a provided type.
	pub fn resolve_unknown(&mut self, id: usize, ty: Ty) {
		self.tys.iter_mut().for_each(|entry| {
			let t = entry.take().unwrap();
			entry.replace(t.resolve_unknown(id, ty.clone()));
		});
	}

	/// Are all types certain?
	pub fn solved(&self) -> Option<&Ty> {
		self.tys
			.iter()
			.find(|e| !e.as_ref().unwrap().is_certain())
			.map(|ty| ty.as_ref().unwrap())
	}

	/// Substitutes all type references with the type being referenced.
	/// This process gets rid of unnecessary complexity.
	pub fn substitute(&mut self) {
		self.tys
			.iter_mut()
			.map(|entry| entry.take().unwrap())
			.collect::<Vec<_>>()
			.into_iter()
			.enumerate()
			.for_each(|(idx, ty)| {
				let ty = self.substitute_ty(ty);
				let entry = &mut self.tys[idx];
				entry.replace(ty);
			});
	}

	/// Substitutes all type references in one type. This is done recursively
	/// in such a way where all the types being explored are also expanded.
	pub fn substitute_ty(&mut self, ty: Ty) -> Ty {
		match ty {
			Ty::Int(_) => ty,
			Ty::Float(_) => ty,
			Ty::String(_) => ty,
			Ty::Boolean(_) => ty,
			Ty::Void(_) => ty,
			Ty::Never(_) => ty,
			Ty::Unknown(_, _) => ty,
			Ty::TyRef(ref ty_ref) => {
				let entry = &mut self.tys[ty_ref.id()];
				let ty = entry.take().unwrap();
				let ty = self.substitute_ty(ty);
				let entry = &mut self.tys[ty_ref.id()];
				entry.replace(ty.clone());
				ty
			}
			Ty::Array(ty, s) => Ty::Array(Box::new(self.substitute_ty(*ty)), s),
			Ty::Possibility(pid, tys, s) => {
				// If the possibility contains 0 variants then it will be
				// impossible to solve for, this problem should've been caught
				// earlier.
				assert!(tys.len() != 0);
				let mut tys = tys
					.into_iter()
					.map(|(id, ty)| (id, self.substitute_ty(ty)))
					.collect::<Vec<_>>();
				// Remove all duplicates to remove unnecessary complexity.
				let mut unique_tys = vec![];
				while tys.len() != 0 {
					let mut found = false;
					let element = tys.last().unwrap();
					for ty in &unique_tys {
						if ty == element {
							found = true;
							break;
						}
					}
					if found {
						tys.pop();
					} else {
						unique_tys.push(tys.pop().unwrap());
					}
				}
				// If the possibility only contains one variant then that variant
				// must be the solution.
				if unique_tys.len() == 1 {
					let (_, ty) = unique_tys.pop().unwrap();
					return self.substitute_ty(ty);
				}
				Ty::Possibility(pid, unique_tys, s)
			}
			Ty::Union(tys, s) => {
				// If the union contains 0 variants then it will be
				// impossible to equate it to anything else. This
				// problem should never materialize as unions are
				// immutable and all unions created must contain
				// at least 1 variant.
				assert!(tys.len() != 0);
				let mut tys = tys
					.into_iter()
					.map(|ty| self.substitute_ty(ty))
					.collect::<Vec<_>>();
				// Remove all duplicates to remove unnecessary complexity.
				let mut unique_tys = vec![];
				while tys.len() != 0 {
					let mut found = false;
					let element = tys.last().unwrap();
					for t in &unique_tys {
						if t == element {
							found = true;
							break;
						}
					}
					if found {
						tys.pop();
					} else {
						unique_tys.push(tys.pop().unwrap());
					}
				}
				// If the union only contains one variant then that variant
				// will be the only possible variant.
				if unique_tys.len() == 1 {
					let ty = unique_tys.pop().unwrap();
					return self.substitute_ty(ty);
				}
				Ty::Union(unique_tys, s)
			}
			Ty::Fn(ps, r, s) => {
				let ps = ps.into_iter().map(|ty| self.substitute_ty(ty)).collect();
				let ret_ty = self.substitute_ty(*r);
				Ty::Fn(ps, Box::new(ret_ty), s)
			}
		}
	}

	/// Flatten all the types in the context.
	pub fn flatten(&mut self) {
		println!("Flattening context!");
		self.tys
			.iter_mut()
			.map(|entry| {
				let ty = entry.take().unwrap();
				// println!("Before: {}", ty.to_string());
				ty
			})
			.collect::<Vec<_>>()
			.into_iter()
			.enumerate()
			.for_each(|(idx, ty)| {
				let ty = self.flatten_ty(ty);
				// println!("After: {}", ty.to_string());
				self.tys[idx].replace(ty);
			});
	}

	/// All situations where unions or probabilities are within other
	/// unions or probabilities (respectively) are redundant and
	/// add unnecessary complexity thus those are ironed out.
	/// Probabilities and unions are also "bubbled" up to make
	/// solving easier.
	pub fn flatten_ty(&mut self, ty: Ty) -> Ty {
		match ty {
			Ty::Int(_) => ty,
			Ty::Float(_) => ty,
			Ty::String(_) => ty,
			Ty::Boolean(_) => ty,
			Ty::Void(_) => ty,
			Ty::Never(_) => ty,
			Ty::Unknown(_, _) => ty,
			Ty::TyRef(_) => panic!("cannot flatten type references"),
			Ty::Array(inner, s) => {
				let inner = self.flatten_ty(*inner);
				match inner {
					// Array<A || B || C> => Array<A> || Array<B> || Array<C>>>
					Ty::Possibility(pid, variants, ps) => Ty::Possibility(
						pid,
						variants
							.into_iter()
							.map(|(id, variant)| (id, Ty::Array(Box::new(variant), s.clone())))
							.collect(),
						ps.clone(),
					),
					// Array<A | B | C> != Array<A> | Array<B> | Array<C>
					// thus `Ty::Union` cannot be flattened.
					_ => Ty::Array(Box::new(inner), s),
				}
			}
			Ty::Possibility(pid, variants, s) => {
				let mut new_variants = Vec::with_capacity(variants.len());
				for (id, variant) in variants {
					let variant = self.flatten_ty(variant);
					match variant {
						// (A || B) || C => A || B || C
						Ty::Possibility(_, mut vs, _) => {
							new_variants.append(&mut vs);
						}
						_ => new_variants.push((id, variant)),
					}
				}
				Ty::Possibility(pid, new_variants, s)
			}
			Ty::Union(variants, s) => {
				let mut new_variants = Vec::with_capacity(variants.len());
				let mut possibilities = vec![];
				for variant in variants {
					let variant = self.flatten_ty(variant);
					match variant {
						// (A || B) | C => (A | C) || (B | C)
						Ty::Possibility(_, vs, _) => {
							possibilities.push(vs);
						}
						// (A | B) | C => A | B | C
						Ty::Union(mut vs, _) => {
							new_variants.append(&mut vs);
						}
						_ => new_variants.push(variant),
					}
				}
				// If the union variants don't contain possibilities
				// the final type will be a union
				if possibilities.len() == 0 {
					return Ty::Union(new_variants, s);
				}
				/// Get all possible outcomes given a list of possibilities
				/// eg: `A || B || C`, `X || Y` => `A and X`, `B and X`, `C and X`, `A and Y`, ...
				/// The basic idea is that you go through each possibility one at a time
				/// and pick one outcome from each form to combinations.
				fn get_outcomes(variants: &[Vec<(Vec<usize>, Ty)>]) -> Vec<(Vec<usize>, Vec<Ty>)> {
					// If the variants slice is empty return an empty variant
					// At least one element must be returned otherwise this combination
					// search algorithm breaks.
					if variants.len() == 0 {
						return vec![(vec![], vec![])];
					}
					let bases = get_outcomes(&variants[1..]);
					let mut outcomes = Vec::new();
					// For each base try each variant to get all possible outcomes
					for variant in &variants[0] {
						for base in &bases {
							let mut outcome = base.clone();
							outcome.0.append(&mut variant.0.clone());
							outcome.1.push(variant.1.clone());
							outcomes.push(outcome);
						}
					}
					outcomes
				}
				// Given a type, like A | (B || C) | (D || E) | F,
				// to properly flatten it all possibilities must be extracted
				// and all possible outcomes must be found. eg: for the above
				// type, the possibilities are B || C, D || E and the outcomes
				// would be B and D, C and D, B and E, C and E... Then we need to combine it
				// with the left over types in the union to get the final type,
				// (B | D | A | F) || (C | D | A | F) || (B | E | A | F) || (C | E | A | F)
				// In this code the variable `possibilities` contains all the extracted possibilities
				// and the variable `outcomes` contains all the possible outcomes.
				let outcomes = get_outcomes(&possibilities[..]);
				// And in this map the final step of combining with the left over types
				// and forming unions is done.
				let outcomes = outcomes
					.into_iter()
					.map(|(id, mut combination)| {
						combination.append(&mut new_variants.clone());
						(id, Ty::Union(combination, s.clone()))
					})
					.collect();
				// It is possible that a variant of a possibility could be a union thus it is flattened
				// again to iron those out. eg: if the type to flatten was A | ((B | C) || D)
				// then, at this moment, the final type would be ((B | C) | A) || (D | A)
				// but, as you can see, that type can be further simplified to (B | C | A) || (D | A)
				let p = self.new_possibility_ext(outcomes, s);
				self.flatten_ty(p)
			}
			Ty::Fn(mut args, ret, s) => {
				// This is done to simplify the process
				// and, later, the last element is popped
				// off to get the return type back.
				args.push(*ret);
				let mut unions = vec![];
				let mut new_args = Vec::with_capacity(args.len());
				// Unlike in other instance, here the order of the types matter as
				// the order arguments are not flexible in Ares. (Additionally the return
				// type must always be at the end)
				for arg in args {
					let arg = self.flatten_ty(arg);
					match arg {
						// Fn(A | B) -> C => (Fn(A) -> C) | (Fn(B) -> C)
						Ty::Union(vs, _) => {
							unions.push(vs);
							new_args.push(None);
						}
						_ => new_args.push(Some(arg)),
					}
				}

				let mut fns;
				if unions.len() != 0 {
					/// This works identically to the above `get_outcomes` function.
					fn get_combinations(variants: &[Vec<Ty>]) -> Vec<Vec<Ty>> {
						if variants.len() == 0 {
							return vec![vec![]];
						}
						let bases = get_combinations(&variants[1..]);
						let mut combinations = Vec::new();
						for variant in &variants[0] {
							for base in &bases {
								let mut combination = base.clone();
								combination.push(variant.clone());
								combinations.push(combination);
							}
						}
						combinations
					}
					// Find all possible combinations.
					// eg: Fn(A | B, C | D) -> E =>
					// (Fn(A, C) -> E) | (Fn(A, D) -> E) | (Fn(B, C) -> E) | (Fn(B, D) -> E)
					let combinations = get_combinations(&unions[..]);
					fns = Vec::with_capacity(combinations.len());
					for combination in combinations {
						let mut combination = combination.into_iter();
						fns.push(
							new_args
								.clone()
								.into_iter()
								.map(|arg| {
									if let Some(ty) = arg {
										Some(ty)
									} else {
										Some(combination.next().unwrap())
									}
								})
								.collect::<Vec<_>>(),
						);
					}
				} else {
					fns = vec![new_args];
				}

				// Get all the possibilities.
				let mut possibilities = vec![];
				let mut p_idxs = vec![];
				for (idx, arg) in (&mut fns[0]).iter_mut().enumerate() {
					if let Some(Ty::Possibility(_, _, _)) = arg {
						possibilities.push(match arg.take().unwrap() {
							Ty::Possibility(id, vs, _) => (id, vs),
							_ => unreachable!(),
						});
						p_idxs.push(idx);
					}
				}
				// Set the respective parameters in all the other functions
				// to None as well.
				for idx in p_idxs {
					for f in &mut fns[1..] {
						f[idx].take().unwrap();
					}
				}

				// If there are no possibilities then return a union.
				// (If there were no unions then just return the
				// one remaining variant.)
				if possibilities.len() == 0 {
					let mut fns: Vec<_> = fns
						.into_iter()
						.map(|f| {
							let mut f = f.into_iter().map(|a| a.unwrap()).collect::<Vec<_>>();
							let ret = f.pop().unwrap();
							Ty::Fn(f, Box::new(ret), s.clone())
						})
						.collect();
					if fns.len() == 1 {
						return fns.pop().unwrap();
					}
					return Ty::Union(fns, s);
				}

				/// This works identically to the above `get_outcomes` function.
				/// (Except if the outcomes of possibilities sharing the same
				/// ID will be the same.)
				fn get_outcomes(
					variants: &[(usize, Vec<(Vec<usize>, Ty)>)],
				) -> Vec<(Vec<usize>, Vec<Ty>, HashMap<usize, usize>)> {
					if variants.len() == 0 {
						return vec![(vec![], vec![], HashMap::new())];
					}
					let bases = get_outcomes(&variants[1..]);
					let mut outcomes = Vec::new();
					for base in &bases {
						if let Some(idx) = base.2.get(&variants[0].0) {
							let variant = &variants[0].1[*idx];
							let mut outcome = base.clone();
							outcome.0.append(&mut variant.0.clone());
							outcome.1.push(variant.1.clone());
							outcomes.push(outcome);
						} else {
							for (idx, variant) in variants[0].1.iter().enumerate() {
								let mut outcome = base.clone();
								outcome.2.insert(variants[0].0, idx);
								outcome.0.append(&mut variant.0.clone());
								outcome.1.push(variant.1.clone());
								outcomes.push(outcome);
							}
						}
					}
					outcomes
				}

				// Get all the possible outcomes.
				// eg: Fn(A || B, C | D) -> E =>
				// ((Fn(A, C) -> E) | (Fn(A, D) -> E)) || ((Fn(B, C) -> E) | (Fn(B, D) -> E))
				let outcomes = get_outcomes(&possibilities[..]);
				let outcomes = outcomes
					.into_iter()
					.map(|(id, outcome, _)| {
						(id, {
							let mut fns: Vec<_> = fns
								.clone()
								.into_iter()
								.map(|f| {
									let mut outcome = outcome.clone().into_iter();
									let mut f = f
										.into_iter()
										.map(|a| {
											if let Some(a) = a {
												a
											} else {
												outcome.next().unwrap()
											}
										})
										.collect::<Vec<_>>();
									let ret = f.pop().unwrap();
									Ty::Fn(f, Box::new(ret), s.clone())
								})
								.collect();
							if fns.len() == 1 {
								fns.pop().unwrap()
							} else {
								Ty::Union(fns, s.clone())
							}
						})
					})
					.collect();
				self.new_possibility_ext(outcomes, s)
			}
		}
	}
}
