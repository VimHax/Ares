mod identifier;
mod number;
mod string;

pub use identifier::consume as consume_identifier;
pub use number::consume as consume_number;
pub use number::Number;
pub use string::consume as consume_string;
