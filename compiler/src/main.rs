use clap::{App, Arg};
use codespan_reporting::files::SimpleFile;
use llvm_sys::{self, core::*, target::*, target_machine::*};
use std::{
	ffi::{CStr, CString},
	mem::MaybeUninit,
	path::Path,
	process::Command,
};

use analyzer::analyze;
use ares_core::Source;
use parser::{parse, tokenize};

mod errors;
mod llvm;

use errors::print_error;
use llvm::generate_ir;

fn main() {
	// Configure clap to parse CLI arguments.
	let matches = App::new("Ares Compiler")
		.version("0.1.0")
		.author("VimHax <vimhax@vimprojects.com>")
		.about("Compiles Ares source code into machine code.")
		.arg(
			Arg::with_name("INPUT")
				.help("Sets the input file to use.")
				.required(true)
				.index(1),
		)
		.arg(
			Arg::with_name("OBJ_OUTPUT")
				.help("Sets the output path to use for the object code.")
				.required(true)
				.index(2),
		)
		.arg(
			Arg::with_name("OUTPUT")
				.help("Sets the output path to use.")
				.required(true)
				.index(3),
		)
		.arg(
			Arg::with_name("asm")
				.long("asm")
				.value_name("FILE")
				.help("Outputs LLVM assembly at the specified location.")
				.takes_value(true),
		)
		.arg(
			Arg::with_name("run")
				.long("run")
				.help("Runs the compiled output."),
		)
		.get_matches();

	let input = matches.value_of("INPUT").unwrap();
	let obj_output = matches.value_of("OBJ_OUTPUT").unwrap();
	let output = matches.value_of("OUTPUT").unwrap();
	let asm = matches.value_of("asm");

	// Build a source struct.
	let source = match Source::from_file(0, Path::new(input)) {
		Ok(v) => v,
		Err(err) => clap::Error::from(err).exit(),
	};

	// Convert the source into a `Vec` of tokens.
	let file = SimpleFile::new(source.file_name().as_str(), source.contents());
	let tokens = match tokenize(&source) {
		Ok(value) => value,
		Err(err) => return print_error(err, &file),
	};

	// Convert the `Vec` of tokens into an AST.
	let ast = match parse(&tokens) {
		Ok(value) => value,
		Err(err) => return print_error(err, &file),
	};

	// Analyze the AST (annotate everything with types).
	let (ctx, analyzed) = match analyze(ast) {
		Ok(value) => value,
		Err(err) => return print_error(err, &file),
	};

	unsafe {
		// Initialize an LLVM context.
		let context = LLVMContextCreate();
		assert!(!context.is_null());
		let module_name = CString::new("program").unwrap();
		let module = LLVMModuleCreateWithNameInContext(module_name.as_ptr(), context);
		assert!(!module.is_null());

		// Generate LLVM IR.
		generate_ir(context, module, &ctx, &analyzed);
		LLVMDumpModule(module);

		// Initialize all the required components.
		LLVM_InitializeAllTargetInfos();
		LLVM_InitializeAllTargets();
		LLVM_InitializeAllTargetMCs();
		LLVM_InitializeAllAsmParsers();
		LLVM_InitializeAllAsmPrinters();

		// Configure the target machine.
		let target_name = CString::new("x86-64").unwrap();
		let target = LLVMGetTargetFromName(target_name.as_ptr());
		assert!(!target.is_null());

		let target_triple = CString::new("x86_64-pc-linux-gnu").unwrap();
		let target_cpu = CString::new("x86-64").unwrap();
		let target_features = CString::new("+avx2").unwrap();
		let target_machine = LLVMCreateTargetMachine(
			target,
			target_triple.as_ptr(),
			target_cpu.as_ptr(),
			target_features.as_ptr(),
			LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
			LLVMRelocMode::LLVMRelocDynamicNoPic,
			LLVMCodeModel::LLVMCodeModelDefault,
		);
		assert!(!target_machine.is_null());

		// Output LLVM assembly.
		if let Some(file) = asm {
			LLVMSetTargetMachineAsmVerbosity(target_machine, 1);
			let asm_file = CString::new(file).unwrap();
			let mut error_msg = MaybeUninit::uninit();
			let return_code = LLVMTargetMachineEmitToFile(
				target_machine,
				module,
				asm_file.as_ptr() as *mut _,
				LLVMCodeGenFileType::LLVMAssemblyFile,
				error_msg.as_mut_ptr(),
			);

			if return_code == 1 {
				let error_msg = CStr::from_ptr(error_msg.assume_init()).to_string_lossy();
				println!("{}", error_msg);
				return;
			}
		}

		// Output LLVM object code.
		let obj_file = CString::new(obj_output).unwrap();
		let mut error_msg = MaybeUninit::uninit();
		let return_code = LLVMTargetMachineEmitToFile(
			target_machine,
			module,
			obj_file.as_ptr() as *mut _,
			LLVMCodeGenFileType::LLVMObjectFile,
			error_msg.as_mut_ptr(),
		);

		if return_code == 1 {
			let error_msg = CStr::from_ptr(error_msg.assume_init()).to_string_lossy();
			println!("{}", error_msg);
			return;
		}
	}

	// Link the object code with Clang.
	println!(
		"{:?}",
		Command::new("sh")
			.arg("-c")
			.arg(format!("clang {} -o {}", obj_output, output))
			.output()
			.expect("failed to execute process")
	);

	// Run the linked executable.
	if matches.is_present("run") {
		print!(
			"--- Program Output ---\n{}",
			String::from_utf8(
				Command::new("sh")
					.arg("-c")
					.arg(format!("./{}", output))
					.output()
					.expect("failed to execute process")
					.stdout
			)
			.unwrap()
		);
	}
}
