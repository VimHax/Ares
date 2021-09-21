// use analyzer::{analyze, ast::Node};

use ares_core::Source;

use parser::{parse, tokenize};

fn main() {
	let source = Source::from_string(0, "main".to_string(), "".to_string());
	let tokens = match tokenize(&source) {
		Ok(value) => value,
		Err(_err) => {
			// print_error(err, &files);
			return;
		}
	};
	// println!("Tokens: {:?}", tokens);

	let ast = match parse(&tokens) {
		Ok(value) => value,
		Err(_err) => {
			// print_error(err, &files);
			return;
		}
	};
	println!("AST: {:?}", ast);
	/*let _analyzed = match analyze(ast) {
		Ok(value) => value,
		Err(_err) => {
			// print_error(err, &files);
			return;
		}
	};*/
}
