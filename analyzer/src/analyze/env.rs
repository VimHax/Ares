use std::{collections::HashMap, usize};

use crate::ty::Ty;

use super::ty::TyRef;

/// Keeps track of declared variables,
/// scopes, the return type and loop states
/// within a function body.
#[derive(Debug)]
pub struct Environment {
	scopes: Vec<HashMap<String, TyRef>>,
	ret_ty: Ty,
	loop_ty: Vec<Ty>,
}

impl Environment {
	pub fn new(ret_ty: Ty) -> Self {
		Self {
			scopes: vec![],
			ret_ty,
			loop_ty: vec![],
		}
	}

	/// The encompassing function return type.
	pub fn ret_ty(&self) -> &Ty {
		&self.ret_ty
	}

	/// The encompassing loop data type.
	pub fn loop_ty(&mut self) -> Option<&Ty> {
		self.loop_ty.last()
	}

	/// Add a new loop type.
	pub fn add_loop(&mut self, ty: Ty) {
		self.loop_ty.push(ty);
	}

	/// Remove the last loop type.
	/// **Panics if there is no loop type left.**
	pub fn remove_loop(&mut self) {
		self.loop_ty.pop().unwrap();
	}

	/// How many scopes are currently being kept
	/// track of?
	pub fn scopes(&self) -> usize {
		self.scopes.len()
	}

	/// Create a new lexical scope.
	pub fn add_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	/// Pops off the last created scope.
	/// **Panics if there is no scope left.**
	pub fn remove_scope(&mut self) {
		self.scopes.pop().unwrap();
	}

	/// Add a variable to the current scope.
	/// (Variable shadowing is allowed.)
	pub fn add_variable(&mut self, name: String, ty: TyRef) {
		let scope = self.scopes.last_mut().unwrap();
		scope.insert(name, ty);
	}

	/// Find a variable with the provided name.
	/// Explores the scopes from the innermost to
	/// the outermost.
	pub fn find_variable(&mut self, name: &String) -> Option<TyRef> {
		for scope in self.scopes.iter().rev() {
			let var = scope.get(name);
			if let Some(ty) = var {
				return Some(ty.clone());
			}
		}
		None
	}
}
