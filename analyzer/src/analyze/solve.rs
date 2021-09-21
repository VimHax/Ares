use std::collections::{HashMap, HashSet};

use super::{
	ty::{Ctx, Ty},
	TypeError,
};

/// Keeps track of the current state of the current iteration
/// of the solver. (All invalidated possibilities and assertions)
struct SolverState<'a> {
	invalid_possibilities: HashSet<usize>,
	assertions: HashMap<usize, Vec<&'a Ty>>,
}

impl<'a> SolverState<'a> {
	pub fn new() -> Self {
		Self {
			invalid_possibilities: HashSet::new(),
			assertions: HashMap::new(),
		}
	}

	pub fn invalidate_possibilities(&mut self, ids: HashSet<usize>) {
		for id in ids {
			self.invalid_possibilities.insert(id);
		}
	}

	pub fn assert(&mut self, id: usize, ty: &'a Ty) {
		self.assertions.entry(id).or_insert(vec![]).push(ty)
	}

	pub fn export(self) -> (HashSet<usize>, HashMap<usize, Vec<&'a Ty>>) {
		(self.invalid_possibilities, self.assertions)
	}
}

/// Checks if 2 types are assignable.
/// Treats unknown types as any types.
/// Removes possibilities which are deemed impossible.
/// Adds assertions when unknown types are encountered.
/// Type references are expected to be absent.
/// eg: `let a: LHS = b :: RHS;`,
/// `(f :: Fn(LHS) -> Void)(b :: RHS)`
fn is_assignable<'a>(lhs: &'a Ty, rhs: &'a Ty, state: &mut SolverState<'a>, assert: bool) -> bool {
	let result = match (lhs, rhs) {
		// Type references should be completely eliminated at this point.
		(_, Ty::TyRef(_)) | (Ty::TyRef(_), _) => {
			panic!("cannot check assignability with types involving type references")
		}
		(_, Ty::Never(_)) | (Ty::Never(_), _) => {
			panic!("never type should never be compared")
		}
		(Ty::Unknown(_, _), Ty::Unknown(_, _)) => true,
		// The assignability of unknown types is currently unknown (how unexpected!)
		// and thus ignored. However all the types are being asserted to acknowledge
		// their requirement to be assignable to be valid.
		(t, Ty::Unknown(id, _)) | (Ty::Unknown(id, _), t) => {
			if assert {
				state.assert(id.clone(), t);
			}
			true
		}
		// Find the intersection of the 2 possibilities.
		(Ty::Possibility(_, ts1, _), Ty::Possibility(_, ts2, _)) => {
			let mut unassignable = HashSet::new();
			let mut assignable = HashSet::new();
			// Find all assignable types in ts1
			for (ids, t1) in ts1 {
				let found = ts2
					.iter()
					.any(|(_, t2)| is_assignable(t1, t2, state, false));
				if found {
					for &id in ids {
						assignable.insert(id);
					}
				} else {
					for &id in ids {
						unassignable.insert(id);
					}
				}
			}
			// Find all assignable types in ts2
			for (ids, t2) in ts2 {
				let found = ts1
					.iter()
					.any(|(_, t1)| is_assignable(t1, t2, state, false));
				if found {
					for &id in ids {
						assignable.insert(id);
					}
				} else {
					for &id in ids {
						unassignable.insert(id);
					}
				}
			}
			if assignable.len() == 0 {
				return false;
			}
			for id in assignable {
				unassignable.remove(&id);
			}
			state.invalidate_possibilities(unassignable);
			true
		}
		// Check whether the opposing side is assignable to at least one of the
		// variants of the chosen side. Remove all the variants which don't match.
		(tx, Ty::Possibility(_, ts, _)) | (Ty::Possibility(_, ts, _), tx) => {
			let mut unassignable = HashSet::new();
			let mut assignable = HashSet::new();
			// The order of the parameters for `is_assignable` needs to be the same
			// as the order matched.
			let is_rev = matches!((lhs, rhs), (Ty::Possibility(_, _, _), _));
			for (ids, ty) in ts {
				let is_assignable = if is_rev {
					is_assignable(ty, tx, state, false)
				} else {
					is_assignable(tx, ty, state, false)
				};
				if is_assignable {
					for &id in ids {
						assignable.insert(id);
					}
				} else {
					for &id in ids {
						unassignable.insert(id);
					}
				}
			}
			if assignable.len() == 0 {
				return false;
			}
			for id in assignable {
				unassignable.remove(&id);
			}
			state.invalidate_possibilities(unassignable);
			true
		}
		// Check whether all the variants of the RHS can be assigned to the LHS.
		(t1, Ty::Union(ts, _)) => ts.iter().all(|t2| is_assignable(t1, t2, state, false)),
		// Check whether the RHS matches at least one of the variants of the LHS.
		(Ty::Union(ts, _), t2) => ts.iter().any(|t1| is_assignable(t1, t2, state, false)),
		// Take care of all the primitive types (types which don't involve other types).
		// These types cannot contain uncertainties as they don't involve other types.
		(Ty::Int(_), Ty::Int(_)) => true,
		(Ty::Float(_), Ty::Float(_)) => true,
		(Ty::String(_), Ty::String(_)) => true,
		(Ty::Boolean(_), Ty::Boolean(_)) => true,
		(Ty::Void(_), Ty::Void(_)) => true,
		// Take care of the non-primitive types. Make sure to consider all the involved
		// types and their equalities.
		(Ty::Array(t1, _), Ty::Array(t2, _)) => is_assignable(t1, t2, state, assert),
		(Ty::Fn(ts1, t1, _), Ty::Fn(ts2, t2, _)) => {
			// Check if both have the same amount of parameters.
			if ts1.len() != ts2.len() {
				return false;
			}
			// Check whether both return types are assignable.
			if !is_assignable(t1, t2, state, assert) {
				return false;
			}
			// Check whether all the parameters are assignable.
			for (t1, t2) in ts1.iter().zip(ts2) {
				if !is_assignable(t1, t2, state, assert) {
					return false;
				}
			}
			true
		}
		(_, Ty::Int(_))
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
		| (Ty::Array(_, _), _) => false,
	};
	// println!("is_assignable ({:?}): {:?} <- {:?}", result, lhs, rhs);
	result
}

/// Solve all uncertain types solely based on the provided constraints and inference.
pub fn solve(ctx: &mut Ctx) -> Result<(), TypeError> {
	let eqs = ctx.take_constraints();

	loop {
		// Run these 2 processes to format types in an easy to validate way.
		ctx.substitute();
		ctx.flatten();

		let mut state = SolverState::new();
		// Each iteration substitution is required so that the types within
		// the equations are synchronized with the types within the context.
		let processed_eqs = eqs
			.iter()
			.cloned()
			.map(|((t1, t2), s)| ((ctx.substitute_ty(t1), ctx.substitute_ty(t2)), s))
			.collect::<Vec<_>>();

		// The heart of the solver, validates all the equations and any assertions
		// made or invalid possibilities are all recorded in the solver state.
		for ((lhs, rhs), _) in &processed_eqs {
			if !is_assignable(lhs, rhs, &mut state, true) {
				println!("LHS: {:?}, RHS: {:?}", lhs, rhs);
				return Err(TypeError::Contradiction(vec![lhs.clone(), rhs.clone()]));
			}
		}

		let (invalid_possibilities, assertions) = state.export();
		// If there are no new assertions or invalid possibilities then there is
		// nothing more any new iterations can do.
		if invalid_possibilities.len() == 0 && assertions.len() == 0 {
			ctx.substitute();
			ctx.flatten();
			break;
		}

		// Resolve all assertions.
		for (id, ts) in assertions {
			println!("Assertion: {}, {:?}", id, ts);
			let t1 = *ts.first().unwrap();
			// Check whether all the assertions made to the same
			// unknown are identical.
			/*for &t2 in ts.iter().skip(1) {
				if t1 != t2 {
					return Err(TypeError::Contradiction(ts.into_iter().cloned().collect()));
				}
			}*/
			// If there are no contradictions then the unknown in question can be resolved.
			ctx.resolve_unknown(id, t1.clone());
		}

		if invalid_possibilities.len() != 0 {
			println!("Invalid possibilities: {:?}", invalid_possibilities);
			// Process all the invalid possibilities.
			ctx.remove_possibilities(&invalid_possibilities.into_iter().collect::<Vec<_>>()[..])?;
		}
	}

	ctx.substitute();
	if let Some(ty) = ctx.solved() {
		return Err(TypeError::Ambiguous(ty.clone()));
	}

	Ok(())
}
