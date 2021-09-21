use llvm_sys::prelude::LLVMValueRef;

use analyzer::{
	ast::{Analyzed, Block},
	ty::Ctx,
};

use super::{generate_ir::Environment, stmt::generate_stmt};

/// Generate LLVM IR for an AST `Block<Analyzed>` node.
pub unsafe fn generate_block<'a>(
	ctx: &Ctx,
	block: &'a Block<Analyzed>,
	env: &mut Environment<'a>,
) -> Option<LLVMValueRef> {
	env.add_scope();
	let mut last = None;
	// Generate IR for every statement.
	for (idx, stmt) in block.statements().iter().enumerate() {
		let res = generate_stmt(ctx, stmt, env);
		if idx + 1 == block.statements().len()
			&& block.does_eval()
			&& !block.exit_status().will_exit()
		{
			last = res;
		}
	}
	env.remove_scope();
	last
}
