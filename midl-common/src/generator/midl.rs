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

use crate::{
    model::{
        AnnotationGroup, AnnotationItem, Annotations, Argument, Class, Collection, Constant, Enumeration,
        EnumerationItem, Field, Interface, Method, Model, Name, Object, PodType, Type, ONEWAY_ANNOTATION,
        OPTIONAL_ANNOTATION, PROMISE_ANNOTATION,
    },
    utils,
};
use failure::Error;
use itertools::Itertools;
use log::debug;
use std::{
    collections::HashSet,
    convert::Into,
    fs,
    io::{self, Write},
    path::PathBuf,
};

struct Options {
    mode: OutputMode,
    license: Option<String>,
}

struct Files<'a> {
    created: HashSet<PathBuf>,
    options: &'a Options,
}

trait ToMidl {
    fn format(&self, _: &Options) -> String;

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        w.write(self.format(opt).as_bytes())
            .and_then(|_| w.write(&[b'\n']))
            .map(|_| ())
    }
}

/// Output mode for producting midl
#[derive(Clone)]
pub enum OutputMode {
    /// Write each interface, class and enum in a dedicated file
    Split(PathBuf),
    /// Group interfaces, classes and enums by package. One file per package
    Package(PathBuf),
    /// Write to stdout
    Stdout,
}

/// Generate model in to midl format
pub fn generate(model: &Model, license: Option<&str>, mode: OutputMode) -> Result<i32, Error> {
    let license = license.map(|license| {
        format!(
            "//\n{}",
            license
                .lines()
                .map(|l| format!("//{}{}\n", if l.is_empty() { "" } else { " " }, l))
                .collect::<String>()
        )
    });

    let options = Options { mode, license };

    let mut files = Files::new(&options);

    for key in model.objects.keys().sorted() {
        match model.objects.get(key) {
            Some(Object::Class(c)) => files
                .write_for_name(&key)
                .and_then(|mut w| {
                    writeln!(w)
                        .map_err(Into::into)
                        .and_then(|_| c.borrow().write(&mut w, &options).map_err(Into::into))
                })
                .map(|_| ())?,
            Some(Object::Enumeration(e)) => files
                .write_for_name(&key)
                .and_then(|mut w| {
                    writeln!(w)
                        .map_err(Into::into)
                        .and_then(|_| e.borrow().write(&mut w, &options).map_err(Into::into))
                })
                .map(|_| ())?,
            Some(Object::Interface(i)) => files
                .write_for_name(&key)
                .and_then(|mut w| {
                    writeln!(w)
                        .map_err(Into::into)
                        .and_then(|_| i.borrow().write(&mut w, &options).map_err(Into::into))
                })
                .map(|_| ())?,
            Some(Object::Unresolved(_)) => panic!("Unresolved object"),
            _ => unreachable!(),
        }
    }

    Ok(0)
}

impl ToMidl for PodType {
    fn format(&self, _: &Options) -> String {
        match self {
            PodType::Bool => "bool",
            PodType::U8 => "u8",
            PodType::U16 => "u16",
            PodType::U32 => "u32",
            PodType::U64 => "u64",
            PodType::I8 => "i8",
            PodType::I16 => "i16",
            PodType::I32 => "i32",
            PodType::I64 => "i64",
            PodType::F32 => "float",
            PodType::F64 => "double",
            PodType::String => "String",
        }
        .into()
    }
}

impl ToMidl for Type {
    fn format(&self, opt: &Options) -> String {
        match self {
            Type::Collection(ref c) => c.format(opt),
            Type::Object(ref o) => o.format(opt),
            Type::Pod(ref p) => p.format(opt),
        }
    }
}

impl ToMidl for Option<Type> {
    fn format(&self, opt: &Options) -> String {
        if let Some(ref t) = self {
            t.format(opt)
        } else {
            "void".into()
        }
    }
}

impl ToMidl for Collection {
    fn format(&self, opt: &Options) -> String {
        match self {
            Collection::Array(ref c) => format!("{}[]", c.format(opt)),
            Collection::List(ref v) => format!("List<{}>", v.format(opt)),
            Collection::Map(ref k, ref v) => format!("Map<{}, {}>", k.format(opt), v.format(opt)),
            Collection::Set(ref p) => format!("Set<{}>", p.format(opt)),
        }
    }
}

impl ToMidl for Object {
    fn format(&self, opt: &Options) -> String {
        match self {
            Object::Class(c) => c.borrow().format(opt),
            Object::Enumeration(e) => e.borrow().format(opt),
            Object::Interface(i) => i.borrow().format(opt),
            Object::Unresolved(u) => panic!("Encountered unresolved type {} during generation", u),
        }
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        match self {
            Object::Class(c) => c.borrow().write(w, opt),
            Object::Enumeration(e) => e.borrow().write(w, opt),
            Object::Interface(i) => i.borrow().write(w, opt),
            Object::Unresolved(u) => panic!("Encountered unresolved type {} during generation", u),
        }
    }
}

impl ToMidl for Name {
    fn format(&self, _: &Options) -> String {
        self.ident.clone()
    }
}

impl ToMidl for AnnotationItem {
    fn format(&self, _: &Options) -> String {
        match self {
            AnnotationItem::Group(ref g) => g.to_string(),
            AnnotationItem::Numeric(ref n) => n.to_string(),
            AnnotationItem::String(s) => s.clone(),
        }
    }
}

impl<'a> ToMidl for (&'a str, Option<AnnotationItem>) {
    fn format(&self, opt: &Options) -> String {
        format!(
            "{}{}",
            self.0,
            self.1
                .clone()
                .map(|a| format!("={}", a.format(opt)))
                .unwrap_or_else(|| "".into())
        )
    }
}

impl ToMidl for AnnotationGroup {
    fn format(&self, _: &Options) -> String {
        if self.items.is_empty() {
            format!("@{}", self.name)
        } else {
            let values = self
                .items
                .keys()
                .sorted()
                .iter()
                .filter_map(|k| self.items.get(*k).map(|v| (k.as_str(), v.clone())))
                .map(|(k, v)| match v {
                    None => k.to_string(),
                    Some(AnnotationItem::Group(g)) => g.to_string(),
                    _ => {
                        let v = match v {
                            None => "".into(),
                            Some(v) => format!("={}", v),
                        };
                        format!("{}{}", k, v)
                    }
                })
                .join(", ");
            format!("@{}({})", self.name, values)
        }
    }
}

impl ToMidl for Annotations {
    fn format(&self, opt: &Options) -> String {
        self.groups
            .keys()
            .sorted()
            .iter()
            .filter_map(|k| self.groups.get(*k))
            .map(|g| g.format(opt))
            .join("\n")
    }
}

impl ToMidl for Constant {
    fn format(&self, opt: &Options) -> String {
        format!("{} {} = {};", self.type_.format(opt), self.ident, self.value)
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        writeln!(w, "    {}", self.format(opt))
    }
}

impl ToMidl for Field {
    fn format(&self, opt: &Options) -> String {
        let ty = self.type_.format(opt);
        let ty = if self.is_optional() {
            optionalize(&ty)
        } else {
            self.type_.format(opt)
        };
        format!("{} {};", ty, self.ident)
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        self.annotations
            .groups
            .keys()
            .sorted()
            .iter()
            .filter(|a| a.as_str() != OPTIONAL_ANNOTATION)
            .filter_map(|k| self.annotations.groups.get(*k))
            .map(|g| g.format(opt))
            .try_for_each(|a| writeln!(w, "    {}", a))?;
        writeln!(w, "    {}", self.format(opt))
    }
}

impl ToMidl for Class {
    fn format(&self, _: &Options) -> String {
        self.name.to_string()
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        if !self.annotations.groups.is_empty() {
            self.annotations.write(w, opt)?;
        }
        writeln!(
            w,
            "class {} {{",
            match opt.mode {
                OutputMode::Package(_) | OutputMode::Split(_) => self.name.ident.to_string(),
                OutputMode::Stdout => self.name.to_string(),
            }
        )?;
        for constant in &self.constants {
            constant.write(w, opt)?
        }
        if !self.constants.is_empty() {
            w.write_all(&[b'\n'])?;
        }
        for method in &self.fields {
            method.write(w, opt)?
        }
        writeln!(w, "}}")
    }
}

impl ToMidl for Argument {
    fn format(&self, options: &Options) -> String {
        let ty = match &self.type_ {
            Type::Collection(c) => c.format(options),
            Type::Object(o) => o.format(options),
            Type::Pod(p) => p.format(options),
        };

        let ty = if self.is_optional() { optionalize(&ty) } else { ty };

        let annotations = self
            .annotations
            .groups
            .keys()
            .sorted()
            .iter()
            .filter(|a| a.as_str() != OPTIONAL_ANNOTATION)
            .filter_map(|k| self.annotations.groups.get(*k))
            .map(|g| g.format(options))
            .join(" ");
        format!(
            "{}{}{} {}",
            annotations,
            if annotations.is_empty() { "" } else { " " },
            ty,
            self.ident
        )
    }
}

impl ToMidl for Method {
    fn format(&self, options: &Options) -> String {
        let ty = if self.is_promise() {
            format!("Promise<{}>", self.type_.format(options))
        } else {
            self.type_.format(options)
        };
        let ty = if self.is_optional() { optionalize(&ty) } else { ty };
        format!(
            "{} {}({}){};",
            ty,
            self.ident,
            self.arguments
                .iter()
                .map(|a| a.format(options))
                .collect::<Vec<String>>()
                .join(", "),
            if self.is_oneway() { " oneway" } else { "" },
        )
    }

    fn write(&self, w: &mut dyn Write, options: &Options) -> Result<(), io::Error> {
        self.annotations
            .groups
            .keys()
            .sorted()
            .iter()
            .filter(|a| a.as_str() != PROMISE_ANNOTATION)
            .filter(|a| a.as_str() != ONEWAY_ANNOTATION)
            .filter(|a| a.as_str() != OPTIONAL_ANNOTATION)
            .filter_map(|k| self.annotations.groups.get(*k))
            .map(|g| g.format(options))
            .try_for_each(|a| writeln!(w, "    {}", a))?;
        writeln!(w, "    {}", self.format(options))
    }
}

impl ToMidl for Interface {
    fn format(&self, _: &Options) -> String {
        self.name.to_string()
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        if !self.annotations.groups.is_empty() {
            self.annotations.write(w, opt)?;
        }
        writeln!(
            w,
            "interface {} {{",
            match opt.mode {
                OutputMode::Package(_) | OutputMode::Split(_) => self.name.ident.to_string(),
                OutputMode::Stdout => self.name.to_string(),
            }
        )?;
        for m in &self.methods {
            m.write(w, opt)?
        }
        writeln!(w, "}}")
    }
}

impl ToMidl for EnumerationItem {
    fn format(&self, _: &Options) -> String {
        self.ident().to_string()
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        self.annotations()
            .groups
            .keys()
            .sorted()
            .iter()
            .filter_map(|k| self.annotations().groups.get(*k))
            .map(|g| g.format(opt))
            .try_for_each(|a| writeln!(w, "    {}", a))?;
        writeln!(w, "    {},", self.format(opt))
    }
}

impl ToMidl for Enumeration {
    fn format(&self, _: &Options) -> String {
        self.name.to_string()
    }

    fn write(&self, w: &mut dyn Write, opt: &Options) -> Result<(), io::Error> {
        if !self.annotations.groups.is_empty() {
            self.annotations.write(w, opt)?;
        }
        writeln!(w, "enum {} {{", self.name)?;
        for item in &self.items {
            item.write(w, opt)?
        }
        writeln!(w, "}}")
    }
}

impl<'a> Files<'a> {
    pub fn new(options: &'a Options) -> Files<'_> {
        Files {
            options,
            created: HashSet::new(),
        }
    }

    fn license(mut w: impl Write, l: Option<String>) -> Result<impl Write, Error> {
        if let Some(license) = l {
            writeln!(w, "{}", license).map(|_| w).map_err(Into::into)
        } else {
            Ok(w)
        }
    }

    fn package(mut w: impl Write, n: &Name) -> Result<impl Write, Error> {
        writeln!(w, "package {};", n.package.as_path().join("."))
            .map(|_| w)
            .map_err(Into::into)
    }

    fn append_or_create(&mut self, file: PathBuf, name: &Name) -> Result<Box<dyn Write>, Error> {
        if self.created.contains(&file) {
            debug!("Appending to {}", file.display());
            fs::OpenOptions::new()
                .append(true)
                .open(file)
                .map_err(Into::into)
                .map(|f| Box::new(f) as Box<dyn Write>)
        } else {
            debug!("Creating {}", file.display());
            self.created.insert(file.clone());
            let license = self.options.license.clone();
            utils::create_file(&file)
                .and_then(|f| Self::license(f, license))
                .and_then(|f| Self::package(f, name))
                .map(|f| Box::new(f) as Box<dyn Write>)
        }
    }

    pub fn write_for_name(&mut self, name: &Name) -> Result<Box<dyn Write>, Error> {
        let license = self.options.license.clone();

        match self.options.mode {
            OutputMode::Stdout => {
                if self.created.contains(&PathBuf::from("stdout")) {
                    Ok(Box::new(::std::io::stdout()) as Box<dyn Write>)
                } else {
                    self.created.insert(PathBuf::from("stdout"));
                    Ok(Box::new(::std::io::stdout()))
                        .and_then(|w| Self::license(w, license))
                        .map(|w| Box::new(w) as Box<dyn Write>)
                }
            }
            OutputMode::Package(ref d) => {
                let file = name
                    .package
                    .as_path()
                    .iter()
                    .fold(d.to_owned(), |a, ref x| a.join(x))
                    .with_extension("midl");
                self.append_or_create(file, &name)
            }
            OutputMode::Split(ref d) => {
                let file = utils::path_for_name(&d, name, "midl");
                self.append_or_create(file, &name)
            }
        }
    }
}

fn optionalize(s: &str) -> String {
    format!("Optional<{}>", s)
}
