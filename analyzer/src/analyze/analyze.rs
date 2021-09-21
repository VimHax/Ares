use std::collections::HashMap;

use super::{
	ast::{Analyzed, NotAnalyzed, TopLevel, AST, DT},
	dt::analyze_dt,
	solve::solve,
	toplevel::analyze_toplevel,
	ty::{Ctx, Ty},
	TypeError,
};

/// Stores all the function signatures and types.
pub struct TopLevelInfo {
	pub fn_tys: HashMap<String, Ty>,
	pub fn_sigs: HashMap<String, (Vec<(String, DT<Analyzed>)>, Option<DT<Analyzed>>)>,
}

/// Find the data types of everything statically
/// while also verifying the integrity of the code
/// at the type level.
pub fn analyze(ast: AST<NotAnalyzed>) -> Result<(Ctx, AST<Analyzed>), TypeError> {
	// Create a context to store all the data types.
	let mut ctx = Ctx::new();

	// Store all the function signatures ahead of time
	// so that self references and forward references can
	// be resolved.
	let mut info = TopLevelInfo {
		fn_tys: HashMap::new(),
		fn_sigs: HashMap::new(),
	};

	// Create all the function signatures.
	for top_level in &ast {
		match top_level {
			TopLevel::Function(f) => {
				// Check if a function with the same name already exists.
				if let Some(ty) = info.fn_tys.get(f.name()) {
					return Err(TypeError::AlreadyExists(
						f.signature_span().clone(),
						ty.span().clone(),
					));
				}

				let mut param_tys = Vec::with_capacity(f.parameters().len());
				let mut analyzed_params = Vec::with_capacity(f.parameters().len());

				// Analyze every parameter.
				for (name, dt) in f.parameters().iter().cloned() {
					let dt = analyze_dt(dt, &mut ctx)?;
					param_tys.push(Ty::TyRef(dt.ty()));
					analyzed_params.push((name, dt))
				}
				// Analyze the return type.
				let (ret_ty, analyzed_ret) = if let Some(dt) = f.return_dt().cloned() {
					let dt = analyze_dt(dt, &mut ctx)?;
					(Ty::TyRef(dt.ty()), Some(dt))
				} else {
					(Ty::Void(f.signature_span().clone()), None)
				};

				// Build the function type.
				info.fn_tys.insert(
					f.name().clone(),
					Ty::Fn(param_tys, Box::new(ret_ty), f.signature_span().clone()),
				);
				// Build the function signature.
				info.fn_sigs
					.insert(f.name().clone(), (analyzed_params, analyzed_ret));
			}
		}
	}

	// Check if the main function exists and if it's the correct type.
	if let Some(ty) = info.fn_tys.get("main") {
		ctx.add_constraint(
			(
				Ty::Fn(
					vec![],
					Box::new(Ty::Void(ty.span().clone())),
					ty.span().clone(),
				),
				ty.clone(),
			),
			ty.span().clone(),
		);
	} else {
		return Err(TypeError::NoMain);
	}

	// Fully analyze all the top-level components.
	let mut analyzed = Vec::with_capacity(ast.len());
	for top_level in ast {
		analyzed.push(analyze_toplevel(top_level, &info, &mut ctx)?);
	}

	// Solve any existing uncertain types.
	solve(&mut ctx)?;

	Ok((ctx, analyzed))
}
