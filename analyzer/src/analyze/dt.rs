use super::{
	ast::{Analyzed, DTKind, NotAnalyzed, DT},
	ty::{Ctx, Ty, TyRef},
	TypeError,
};

/// Analyze a data type kind, `DTKind<NotAnalyzed>`.
fn analyze_dt_kind<'a>(
	dt: DTKind<NotAnalyzed>,
	ctx: &mut Ctx,
) -> Result<(TyRef, DTKind<Analyzed>), TypeError> {
	Ok(match dt {
		DTKind::Void(s) => (ctx.new_ty(Ty::Void(s.clone())), DTKind::Void(s)),
		DTKind::Int(s) => (ctx.new_ty(Ty::Int(s.clone())), DTKind::Int(s)),
		DTKind::Float(s) => (ctx.new_ty(Ty::Float(s.clone())), DTKind::Float(s)),
		DTKind::String(s) => (ctx.new_ty(Ty::String(s.clone())), DTKind::String(s)),
		DTKind::Boolean(s) => (ctx.new_ty(Ty::Boolean(s.clone())), DTKind::Boolean(s)),
		DTKind::Array(dt, s) => {
			// Analyze the inner type.
			let inner = analyze_dt(*dt, ctx)?;
			(
				ctx.new_ty(Ty::Array(Box::new(Ty::TyRef(inner.ty())), s.clone())),
				DTKind::Array(Box::new(inner), s),
			)
		}
	})
}

/// Analyze a data type node, `DT<NotAnalyzed>`.
pub fn analyze_dt(dt: DT<NotAnalyzed>, ctx: &mut Ctx) -> Result<DT<Analyzed>, TypeError> {
	dt.analyze(|kind| Ok(analyze_dt_kind(kind, ctx)?))
}
