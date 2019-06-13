// Copyright (c) 2018 E.S.R.Labs. All rights reserved.
//
// NOTICE:  All information contained herein is, and remains
// the property of E.S.R.Labs and its suppliers, if any.
// The intellectual and technical concepts contained herein are
// proprietary to E.S.R.Labs and its suppliers and may be covered
// by German and Foreign Patents, patents in process, and are protected
// by trade secret or copyright law.
// Dissemination of this information or reproduction of this material
// is strictly forbidden unless prior written permission is obtained
// from E.S.R.Labs.

use crate::model::Name;
use failure::{format_err, Error};
use std::{
    convert::Into,
    fmt::Display,
    fs,
    io::{BufWriter, Write},
    ops::{AddAssign, SubAssign},
    path::{self, Path, PathBuf},
};

/// Common Java types
pub mod java;

#[macro_export]
macro_rules! fmt {
    ($dst:expr,) => (
        fmt!($dst)
    );
    ($dst:expr, $arg:expr) => (
        $dst.line($arg)
    );
    ($dst:expr, $($arg:tt)*) => (
        $dst.line(format_args!($($arg)*))
    );
}

#[macro_export]
macro_rules! fmt____ {
    ($dst:expr, $arg:expr) => (
        $dst.line_with_indent($dst.indent + 1, $arg)
    );
    ($dst:expr, $($arg:tt)*) => (
        $dst.line_with_indent($dst.indent + 1, format_args!($($arg)*))
    );
}

/// Encapsulates a Write to write indented lines
pub struct Formatter {
    pub indent: usize,
    write: Box<dyn Write>,
}

impl Formatter {
    /// Indentation width in spaces
    pub const INDENTATION: usize = 4;

    /// Create a new Formatter with a `Write` instance
    pub fn new<T: Write + 'static>(write: T) -> Formatter {
        Formatter {
            indent: 0,
            write: Box::new(write),
        }
    }

    /// Create a new Formatter with preset indentation
    pub fn with_indent<T: Write + 'static>(write: T, indent: usize) -> Formatter {
        Formatter {
            indent,
            write: Box::new(write),
        }
    }

    /// Increment the indentation by one
    pub fn increment(&mut self) {
        self.indent += 1;
    }

    /// Decrement the indentation by one
    pub fn decrement(&mut self) {
        self.indent -= 1;
    }

    /// Write a line with current indentation
    pub fn line<A: Display + Sized>(&mut self, text: A) -> Result<(), Error> {
        self.line_with_indent(self.indent, text)
    }

    /// Write a line with given indentation
    pub fn line_with_indent<A: Display + Sized>(&mut self, indent: usize, text: A) -> Result<(), Error> {
        writeln!(self, "{}{}", " ".repeat(indent * Self::INDENTATION), text).map_err(Into::into)
    }

    /// Write lines with current indentation
    pub fn lines(&mut self, lines: &[&str]) -> Result<(), Error> {
        lines.iter().try_for_each(|l| self.line(l))
    }

    /// Write lines with given indentation
    pub fn lines_with_indent(&mut self, indent: usize, lines: &[&str]) -> Result<(), Error> {
        lines.iter().try_for_each(|l| self.line_with_indent(indent, l))
    }

    /// Write a newline
    pub fn newline(&mut self) -> Result<(), Error> {
        self.write_all(&[b'\n']).map_err(Into::into)
    }
}

impl AddAssign<usize> for Formatter {
    fn add_assign(&mut self, other: usize) {
        self.indent += other;
    }
}
impl SubAssign<usize> for Formatter {
    fn sub_assign(&mut self, other: usize) {
        self.indent -= other;
    }
}

impl Write for Formatter {
    fn write(&mut self, buf: &[u8]) -> Result<usize, ::std::io::Error> {
        self.write.write(buf)
    }

    fn flush(&mut self) -> Result<(), ::std::io::Error> {
        self.write.flush()
    }
}

/// Uppercase first char
pub trait Uppercase: ToOwned {
    fn uppercase(&self) -> Self::Owned;
}

impl Uppercase for str {
    fn uppercase(&self) -> String {
        let mut c = self.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }
}

/// Create a Buffered writer for name with extension
pub fn writer(name: &Name, out: &Path, extension: &str) -> Result<impl Write, Error> {
    let path = path_for_name(&out, name, extension);
    create_file(&path).map(BufWriter::new)
}

/// Generate a prefixed path for a Named element with extension e
pub fn path_for_name(prefix: &Path, name: &Name, extension: &str) -> PathBuf {
    prefix.join(name.as_fs_path(extension))
}

/// Open create a file with given extension for writing by
/// creating the subdirs from the identifer package.
pub fn create_file(file: &path::Path) -> Result<fs::File, Error> {
    file.parent()
        .ok_or_else(|| format_err!("{} has no parent", file.display()))
        .and_then(|dir| {
            fs::DirBuilder::new()
                .recursive(true)
                .create(&dir)
                .and_then(|_| fs::File::create(file))
                .map_err(Into::into)
        })
}

#[test]
fn test_named_path() {
    let name = crate::model::Name::from("a.b.c.d");
    assert_eq!(
        path_for_name(&PathBuf::from("/test"), &name, "foo"),
        PathBuf::from("/test/a/b/c/d.foo")
    )
}
