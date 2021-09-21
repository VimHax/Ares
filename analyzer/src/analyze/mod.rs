mod analyze;
pub mod ast;
mod block;
mod dt;
mod env;
mod error;
mod expr;
mod solve;
mod stmt;
mod toplevel;
pub mod ty;

pub use analyze::analyze;
pub use error::TypeError;
