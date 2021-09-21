use std::ops::Range;

use codespan_reporting::{
	diagnostic::{Diagnostic, Label},
	files::SimpleFile,
	term::{
		self,
		termcolor::{ColorChoice, StandardStream},
	},
};

/// Ares error code. (`ScanError`, `ParseError`
/// and `TypeError` combined)
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum ErrorCode {
	// Scan errors.
	NUMBER_OUT_OF_RANGE,
	INVALID_STRING,
	INVALID_SYNTAX,
	// Parse errors.
	INVALID_EXPRESSION,
	INVALID_DATATYPE,
	INVALID_STATEMENT,
	INVALID_OPERAND,
	INVALID_BLOCK,
	INVALID_TOPLEVEL,
	UNEXPECTED_TOKEN,
	// Type errors.
	CONTRADICTION,
	UNSOLVABLE,
	AMBIGUOUS,
	UNRESOLVED_VARIABLE,
	DEAD_CODE,
	BREAK_OUTSIDE_LOOP,
	RETURN_REQUIRED,
	INFINITE_LOOP,
	ALREADY_EXISTS,
	NO_MAIN,
}

/// A generalized Ares error.
pub trait AresError {
	fn message(&self) -> String;
	fn code(&self) -> ErrorCode;
	fn labels(&self, file_end: Range<usize>) -> Vec<Label<()>>;
	fn notes(&self) -> Option<Vec<String>>;
}

/// Print errors which implement the `AresError` trait.
pub fn print_error(error: impl AresError, file: &SimpleFile<&str, &String>) {
	// Configure a diagnostic.
	let mut diagnostic = Diagnostic::error()
		.with_message(error.message())
		.with_code(format!("{:?}", error.code()));
	if file.source().len() != 0 {
		diagnostic =
			diagnostic.with_labels(error.labels(file.source().len() - 1..file.source().len()));
	}
	if let Some(notes) = error.notes() {
		diagnostic = diagnostic.with_notes(notes);
	}
	// Output the error to stderr.
	let writer = StandardStream::stderr(ColorChoice::Always);
	let config = codespan_reporting::term::Config::default();
	term::emit(&mut writer.lock(), &config, file, &diagnostic).unwrap();
}
