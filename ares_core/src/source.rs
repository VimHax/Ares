use std::{
	fs,
	io::{self, Error, ErrorKind},
	path::Path,
};

use crate::Span;

/// Ares source code.
/// Contains the name of the source and it's contents.
#[derive(Debug)]
pub struct Source {
	id: usize,
	name: String,
	file_name: String,
	contents: String,
}

impl Source {
	/// Creates a new source from a string.
	pub fn from_string(id: usize, name: String, contents: String) -> Self {
		Self {
			id,
			name: name.clone(),
			file_name: format!("{}.ares", name),
			contents,
		}
	}

	/// Create a new source from a file.
	pub fn from_file(id: usize, file_path: &Path) -> io::Result<Self> {
		// Get the file name (without the extension).
		let name = file_path
			.file_stem()
			.ok_or(Error::new(ErrorKind::Other, "invalid file path"))?
			.to_str()
			.ok_or(Error::new(
				ErrorKind::Other,
				"cannot convert file stem to str",
			))?
			.to_string();
		// Get the file name.
		let file_name = file_path
			.file_name()
			.ok_or(Error::new(ErrorKind::Other, "invalid file path"))?
			.to_str()
			.ok_or(Error::new(
				ErrorKind::Other,
				"cannot convert file name to str",
			))?
			.to_string();

		// Get the file contents.
		let bytes = fs::read(file_path)?;
		let utf8 = String::from_utf8(bytes);
		let contents = utf8.map_err(|err| Error::new(ErrorKind::Other, err))?;

		Ok(Self {
			id,
			name,
			file_name,
			contents,
		})
	}

	pub fn id(&self) -> usize {
		self.id
	}

	pub fn name(&self) -> &String {
		&self.name
	}

	pub fn file_name(&self) -> &String {
		&self.file_name
	}

	pub fn contents(&self) -> &String {
		&self.contents
	}

	pub fn span_content(&self, span: &Span) -> String {
		self.contents
			.chars()
			.take(span.end())
			.skip(span.start())
			.collect()
	}
}
