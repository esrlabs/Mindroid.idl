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

use crate::model::{
    AnnotationGroup, AnnotationItem, Annotations, Argument, Class, Collection, Constant, Enumeration, EnumerationItem,
    Field, Ident, Interface, MetaInfo, Method, Model, Name, Object, Package, PodType, Type, ONEWAY_ANNOTATION,
    OPTIONAL_ANNOTATION, PROMISE_ANNOTATION,
};
use failure::{err_msg, format_err, Error, Fail};
use itertools::Itertools;
use log::{debug, warn};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::{
    collections::HashMap,
    ffi::OsStr,
    fmt::Display,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};
const EXTENSTION: &str = "midl";

// Include the grammar here to make cargo recompile on grammar changes
#[cfg(debug_assertions)]
const _GRAMMAR: &str = include_str!("midl.pest");

#[derive(Debug, PartialEq)]
enum Nececitty {
    Optional,
    Mandatory,
}

impl Default for Nececitty {
    fn default() -> Nececitty {
        Nececitty::Mandatory
    }
}

#[derive(Parser)]
#[grammar = "parser/midl.pest"]
struct MidlParser;

#[derive(Debug, Fail)]
pub enum ParserError {
    #[fail(display = "Unexpected rule {} while parsing {}", rule, type_)]
    UnexpectedRule { type_: &'static str, rule: Rule },
    #[fail(display = "Failed to convert {} into {}", input, type_)]
    Conversion { input: String, type_: &'static str },
    #[fail(display = "Nececitty type in collection {}", input)]
    CollectionTypeNececitty { input: String },
}

impl ParserError {
    fn unexpected_rule(type_: &'static str, rule: Rule) -> Error {
        ParserError::UnexpectedRule { type_, rule }.into()
    }

    fn conversion(type_: &'static str, input: &str) -> Error {
        ParserError::Conversion {
            type_,
            input: input.to_owned(),
        }
        .into()
    }

    fn collection_type_optional(input: &str) -> Error {
        ParserError::CollectionTypeNececitty {
            input: input.to_owned(),
        }
        .into()
    }
}

trait TryFrom<T>: Sized {
    fn try_from(_: T) -> Result<Self, Error>;
}

trait TryInto<T>: Sized {
    fn try_into(self) -> Result<T, Error>;
}

impl<T, U> TryInto<U> for T
where
    U: TryFrom<T>,
{
    fn try_into(self) -> Result<U, Error> {
        U::try_from(self)
    }
}

impl<'a> TryFrom<&'a str> for PodType {
    fn try_from(i: &str) -> Result<PodType, Error> {
        i.parse().map_err(|_| ParserError::conversion("POD", i))
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for PodType {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::POD {
            i.as_str().try_into()
        } else {
            Err(ParserError::unexpected_rule("POD", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Ident {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::IDENT {
            Ok(i.as_str().into())
        } else {
            Err(ParserError::unexpected_rule("IDENT", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Name {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::NAME {
            Ok(i.as_str().into())
        } else {
            Err(ParserError::unexpected_rule("NAME", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Object {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if !i.as_str().is_empty() {
            // Object are always unresolved in this stage
            Ok(Object::Unresolved(i.as_str().into()))
        } else {
            Err(ParserError::conversion("Object", "empty string"))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Type {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        i.try_into().map(|(t, _): (Type, Nececitty)| t)
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for (Type, Nececitty) {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        match i.as_rule() {
            Rule::OPTIONAL => i
                .into_inner()
                .next()
                .unwrap()
                .try_into()
                .map(|(t, _): (Type, Nececitty)| (t, Nececitty::Optional)),
            Rule::TYPE => {
                let inner = i.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::ARRAY => {
                        let inner = inner.into_inner().next().unwrap();
                        let input = inner.as_str();
                        match inner.as_rule() {
                            rule @ Rule::LIST | rule @ Rule::SET => inner.try_into().and_then(|(ty, nececitty)| {
                                if nececitty == Nececitty::Optional {
                                    Err(ParserError::collection_type_optional(input))
                                } else {
                                    match rule {
                                        Rule::LIST => Ok(Collection::new_list(ty)),
                                        Rule::SET => Ok(Collection::new_list(ty)),
                                        _ => unreachable!(),
                                    }
                                }
                            }),
                            Rule::MAP => {
                                let mut inner = inner.into_inner();
                                let key: PodType = inner.next().unwrap().try_into()?;
                                inner.next().unwrap().try_into().and_then(|(ty, nececitty)| {
                                    if nececitty == Nececitty::Optional {
                                        Err(ParserError::collection_type_optional(input))
                                    } else {
                                        Ok(Collection::new_map(key, ty))
                                    }
                                })
                            }
                            Rule::NAME => inner.try_into().map(Type::Object),
                            Rule::POD => inner.as_str().try_into().map(Type::Pod),
                            _ => Err(ParserError::unexpected_rule("SET LIST MAP POD NAME", inner.as_rule()))?,
                        }
                        .map(|ty| (Collection::new_array(ty), Nececitty::Mandatory))
                    }
                    Rule::LIST => {
                        let inner = inner.into_inner().next().unwrap();
                        let input = inner.as_str();
                        inner.try_into().and_then(|(ty, nececitty)| {
                            if nececitty == Nececitty::Optional {
                                Err(ParserError::collection_type_optional(input))
                            } else {
                                Ok((Collection::new_list(ty), Nececitty::Mandatory))
                            }
                        })
                    }
                    Rule::MAP => {
                        let mut inner = inner.into_inner();
                        let input = inner.as_str();
                        let key: PodType = inner.next().unwrap().try_into()?;
                        inner.next().unwrap().try_into().and_then(|(ty, nececitty)| {
                            if nececitty == Nececitty::Optional {
                                Err(ParserError::collection_type_optional(input))
                            } else {
                                Ok((Collection::new_map(key, ty), Nececitty::Mandatory))
                            }
                        })
                    }
                    Rule::SET => {
                        let input = inner.as_str();
                        inner
                            .into_inner()
                            .next()
                            .unwrap()
                            .try_into()
                            .and_then(|(ty, nececitty)| {
                                if nececitty == Nececitty::Optional {
                                    Err(ParserError::collection_type_optional(input))
                                } else {
                                    Ok((Collection::new_set(ty), Nececitty::Mandatory))
                                }
                            })
                    }
                    Rule::POD => Ok((Type::Pod(inner.as_str().try_into()?), Nececitty::Mandatory)),
                    Rule::NAME => Ok((Type::Object(inner.try_into()?), Nececitty::Mandatory)),
                    Rule::OPTIONAL => inner.try_into().map(|(ty, _)| (ty, Nececitty::Optional)),
                    _ => unreachable!(),
                }
            }
            _ => Err(ParserError::unexpected_rule("TYPE", i.as_rule())),
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Package {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        let i = i.into_inner().next().unwrap();
        Ok(Package(
            i.as_str().split('.').map(ToString::to_string).collect::<Vec<String>>(),
        ))
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Field {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::FIELD {
            let mut i = i.into_inner();
            let mut annotations: Annotations = i.next().unwrap().try_into()?;
            let (type_, nececitty) = i.next().unwrap().try_into()?;
            if nececitty == Nececitty::Optional {
                annotations.push(AnnotationGroup::new(OPTIONAL_ANNOTATION.into()));
            }
            let ident = i.next().unwrap().try_into()?;
            Ok(Field::new(annotations, type_, ident))
        } else {
            Err(ParserError::unexpected_rule("FIELD", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Constant {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::CONSTANT {
            let mut i = i.into_inner();
            let type_ = Type::Pod(i.next().unwrap().try_into()?);
            let ident = i.next().unwrap().try_into()?;
            let a = i.next().unwrap();
            // TODO add a type check here
            let value = a.as_str().into();

            Ok(Constant::new(type_, ident, value))
        } else {
            Err(ParserError::unexpected_rule("CONSTANT", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Argument {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::ARGUMENT {
            let mut i = i.into_inner();
            let mut annotations: Annotations = i.next().unwrap().try_into()?;
            let (type_, nececitty) = i.next().unwrap().try_into()?;
            if nececitty == Nececitty::Optional {
                annotations.push(AnnotationGroup::new(OPTIONAL_ANNOTATION.into()));
            }
            let ident = i.next().unwrap().try_into()?;

            Ok(Argument::new(annotations, type_, ident))
        } else {
            Err(ParserError::unexpected_rule("ARG", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for (String, Option<AnnotationItem>) {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        let i = i.into_inner().next().unwrap();
        match i.as_rule() {
            Rule::ANNOTATION_GROUP => {
                let group: AnnotationGroup = i.try_into()?;
                Ok((group.name.clone(), Some(AnnotationItem::Group(group))))
            }
            Rule::ANNOTATION_KEY_VALUE_PAIR => {
                let mut i = i.into_inner();
                let ident: String = i.next().unwrap().try_into()?;
                let value = if let Some(n) = i.next() {
                    n.as_str()
                        .parse::<i64>()
                        .ok()
                        .map(AnnotationItem::Numeric)
                        .or_else(|| Some(AnnotationItem::String(n.as_str().to_owned())))
                } else {
                    None
                };
                Ok((ident, value))
            }
            _ => Err(ParserError::unexpected_rule(
                "ANNOTATION_GROUP or ANNOTATION_KEY_VALUE_PAIR",
                i.as_rule(),
            )),
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for AnnotationGroup {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        // key
        let mut i = i.into_inner();

        let group_name = i
            .next()
            .unwrap()
            .try_into()
            .map(|n: String| n.trim_start_matches('@').to_owned())?;

        let mut group = AnnotationGroup::new(group_name);

        // value
        for e in i {
            let (key, value): (String, Option<AnnotationItem>) = e.try_into()?;
            group.insert(&key, value);
        }

        Ok(group)
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Annotations {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        assert_eq!(i.as_rule(), Rule::ANNOTATIONS);
        let mut groups: HashMap<String, AnnotationGroup> = HashMap::new();
        for p in i.into_inner() {
            let group: AnnotationGroup = p.try_into()?;
            let key = group.name.to_string();

            if groups.contains_key(&key) {
                groups.get_mut(&key).and_then(|g| {
                    group.items.iter().for_each(|(k, v)| {
                        g.insert(k, v.clone());
                    });
                    Some(())
                });
            } else {
                groups.insert(group.name.to_string(), group);
            }
        }
        Ok(Annotations::new(groups))
    }
}

#[derive(Default, Debug, PartialEq)]
struct Return {
    type_: Option<Type>,
    nececitty: Nececitty,
    promise: bool,
}

impl<'i> TryFrom<Pair<'i, Rule>> for Return {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_str() == "void" {
            Ok(Return::default())
        } else {
            let nececitty = if i.as_str().starts_with("Optional") {
                Nececitty::Optional
            } else {
                Nececitty::Mandatory
            };
            let i = i.into_inner().next().unwrap();
            match i.as_rule() {
                Rule::PROMISE => {
                    let promise = true;
                    let i = i.into_inner().next().unwrap();
                    if i.as_str() == "void" {
                        // Promise<void>
                        Ok(Return {
                            type_: None,
                            nececitty,
                            promise,
                        })
                    } else {
                        // Promise<Type>
                        i.try_into().map(|(ty, _)| Return {
                            type_: Some(ty),
                            nececitty,
                            promise,
                        })
                    }
                }
                Rule::TYPE => {
                    if i.as_str() == "void" {
                        Ok(Return::default())
                    } else {
                        let (ty, nececitty) = i.try_into()?;
                        Ok(Return {
                            type_: Some(ty),
                            nececitty,
                            promise: false,
                        })
                    }
                }
                _ => unreachable!(),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct Oneway(bool);

impl<'i> TryFrom<Pair<'i, Rule>> for Oneway {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        Ok(Oneway(i.as_str() == "oneway"))
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for Method {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::METHOD {
            let mut i = i.into_inner();
            let mut annotations: Annotations = i.next().unwrap().try_into()?;

            let return_: Return = i.next().unwrap().try_into()?;
            let ident = i.next().unwrap().try_into()?;

            let mut args = vec![];
            let mut oneway = Oneway(false);
            for pair in i {
                match pair.as_rule() {
                    Rule::ARGUMENT => args.push(pair.try_into()?),
                    Rule::ONEWAY => oneway = pair.try_into()?,
                    _ => break,
                }
            }

            if return_.nececitty == Nececitty::Optional {
                annotations.push(AnnotationGroup::new(OPTIONAL_ANNOTATION.into()));
            }
            if return_.promise {
                annotations.push(AnnotationGroup::new(PROMISE_ANNOTATION.into()));
            } else if oneway.0 {
                annotations.push(AnnotationGroup::new(ONEWAY_ANNOTATION.into()));
            }

            Ok(Method::new(ident, args, return_.type_, annotations))
        } else {
            Err(ParserError::unexpected_rule("METHOD", i.as_rule()))
        }
    }
}

impl<'i> TryFrom<Pair<'i, Rule>> for EnumerationItem {
    fn try_from(i: Pair<'i, Rule>) -> Result<Self, Error> {
        if i.as_rule() == Rule::ENUM_ITEM {
            let mut i = i.into_inner();
            let annotations = i.next().unwrap().try_into()?;
            let ident = i.next().unwrap().try_into()?;
            Ok(EnumerationItem::new(ident, annotations))
        } else {
            Err(ParserError::unexpected_rule("ENUM_ITEM", i.as_rule()))
        }
    }
}

fn parse_enumeration(pair: Pair<'_, Rule>, package: &Package, source: &Path) -> Result<Enumeration, Error> {
    if pair.as_rule() == Rule::ENUM {
        let mut i = pair.into_inner();
        let annotations = i.next().unwrap().try_into()?;
        let next = i.next().ok_or_else(|| err_msg("Expected name"))?;
        let name: Name = match next.as_rule() {
            Rule::NAME => {
                let n: Name = next.try_into()?;
                if n.package.is_empty() {
                    Name::new(package.clone(), n.ident.clone())
                } else {
                    n
                }
            }
            _ => return Err(ParserError::unexpected_rule("NAME", next.as_rule())),
        };

        let mut items = Vec::new();
        for pair in i {
            match pair.as_rule() {
                Rule::ENUM_ITEM => items.push(pair.try_into()?),
                _ => return Err(ParserError::unexpected_rule("ENUM_ITEM", pair.as_rule())),
            }
        }
        let meta = MetaInfo::new(Some(source.to_path_buf()));
        Ok(Enumeration::new(name, items, annotations, meta))
    } else {
        Err(ParserError::unexpected_rule("ENUMERATION", pair.as_rule()))
    }
}

fn parse_interface(i: Pair<'_, Rule>, package: &Package, source: &Path) -> Result<Interface, Error> {
    if i.as_rule() == Rule::INTERFACE {
        let mut i = i.into_inner();

        let annotations = i.next().ok_or_else(|| err_msg("Expected annotations"))?.try_into()?;
        let next = i.next().ok_or_else(|| err_msg("Expected name"))?;
        let name: Name = match next.as_rule() {
            Rule::NAME => {
                let n: Name = next.try_into()?;
                if n.package.is_empty() {
                    Name::new(package.clone(), n.ident.clone())
                } else {
                    n
                }
            }
            _ => return Err(ParserError::unexpected_rule("NAME", next.as_rule())),
        };
        let methods = i.map(TryInto::try_into).fold_results(vec![], |mut v, m| {
            v.push(m);
            v
        })?;
        let meta = MetaInfo::new(Some(source.to_path_buf()));
        Ok(Interface::new(name, methods, annotations, meta))
    } else {
        Err(ParserError::unexpected_rule("INTERFACE", i.as_rule()))
    }
}

fn parse_class(i: Pair<'_, Rule>, package: &Package, source: &Path) -> Result<Class, Error> {
    if i.as_rule() == Rule::CLASS {
        let mut i = i.into_inner();

        let annotations = i.next().ok_or_else(|| err_msg("Expected annotations"))?.try_into()?;
        let next = i.next().ok_or_else(|| err_msg("Expected name"))?;
        let name: Name = match next.as_rule() {
            Rule::NAME => {
                let n: Name = next.try_into()?;
                if n.package.is_empty() {
                    Name::new(package.clone(), n.ident.clone())
                } else {
                    n
                }
            }
            _ => return Err(ParserError::unexpected_rule("NAME", next.as_rule())),
        };

        let mut fields = Vec::new();
        let mut constants = Vec::new();
        for pair in i {
            match pair.as_rule() {
                Rule::CONSTANT => constants.push(pair.try_into()?),
                Rule::FIELD => fields.push(pair.try_into()?),
                _ => return Err(ParserError::unexpected_rule("CLASS", pair.as_rule())),
            }
        }
        let meta = MetaInfo::new(Some(source.to_path_buf()));
        let class = Class::new(name, fields, constants, annotations, meta);

        Ok(class)
    } else {
        Err(format_err!("Cannot convert rule {} into Class", i.as_rule()))
    }
}

// Implement Display for Rule in order to get nice error messages. Extend this as needed...
impl Display for Rule {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Rule::ALPHA => "alphanumeric character 'a..zA..Z0..9'".into(),
            Rule::ANNOTATIONS => "annotations '@Foo(@Foo)'".into(),
            Rule::ANNOTATION_GROUP => "annotation group '@Foo'".into(),
            Rule::ANNOTATION_ITEM => "annotation item 'foo'".into(),
            Rule::ANNOTATION_KEY_VALUE_PAIR => "annotation key value pair 'foo=12'".into(),
            Rule::ANNOTATION_VALUE => "annotation value '12'".into(),
            Rule::ARGUMENT => "argument 'int foo'".into(),
            Rule::ARRAY => "array 'int[]'".into(),
            Rule::CLASS => "class 'class { ... }'".into(),
            Rule::COMMENT => "comment".into(),
            Rule::CONSTANT => "constant 'bool FALSE = false;'".into(),
            Rule::CONSTANT_VALUE => "constant value '\"foo\"'".into(),
            Rule::DIGIT => "'0'..'9'".into(),
            Rule::ENUM => "enumeration".into(),
            Rule::ENUM_ITEM => "enumeration item 'foo,'".into(),
            Rule::FIELD => "class field 'bool foo;'".into(),
            Rule::IDENT => "identifier".into(),
            Rule::INTERFACE => "interface declaration 'interface { ... }'".into(),
            Rule::LIST => "list 'List<i32>'".into(),
            Rule::MAP => "map 'Map<i32, Foo>'".into(),
            Rule::METHOD => "method 'void foo(bool bar);".into(),
            Rule::METHOD_TYPE => "type".into(),
            Rule::MODEL => "model".into(),
            Rule::NAME => "name".into(),
            Rule::NEWLINE => "newline".into(),
            Rule::ONEWAY => "oneway".into(),
            Rule::PACKAGE => "package".into(),
            Rule::PATH => "path 'test.foo.bar'".into(),
            Rule::POD => "pod type".into(),
            Rule::PROMISE => "promise".into(),
            Rule::SET => "set 'Set<int>'".into(),
            Rule::TYPE => "type".into(),
            Rule::WHITESPACE => "whitespace".into(),
            _ => format!("{:?}", self),
        };
        write!(f, "{}", s)
    }
}

/// Parse input string and extend the passed model
fn parse_extend(input: &str, model: &mut Model, source: &Path) -> Result<(), Error> {
    let format = |rule: &Rule| format!("{}", rule);
    let i = MidlParser::parse(Rule::MODEL, &input)
        .map_err(|e| format_err!("{}", e.renamed_rules(format)))?
        .next()
        .unwrap() // first model
        .into_inner(); // within model

    let mut package = Package::default();

    for pair in i {
        match pair.as_rule() {
            Rule::CLASS => {
                parse_class(pair, &package, &source).and_then(|c| model.push_class(c))?;
            }
            Rule::ENUM => parse_enumeration(pair, &package, &source).and_then(|e| model.push_enumeration(e))?,
            Rule::INTERFACE => parse_interface(pair, &package, &source).and_then(|i| model.push_interface(i))?,
            Rule::PACKAGE => package = pair.try_into()?,
            Rule::EOI => (),
            _ => return Err(ParserError::unexpected_rule("MODEL", pair.as_rule())),
        }
    }
    Ok(())
}

pub fn parse(files: &[PathBuf]) -> Result<Model, Error> {
    let mut model = Model::default();

    files
        .iter()
        .filter(|f| {
            let ext = f.extension().and_then(OsStr::to_str).unwrap_or_else(|| "");
            if ext != EXTENSTION {
                warn!("Skipping invalid file name {}", f.display());
                false
            } else {
                true
            }
        })
        .try_for_each(|f| {
            debug!("Parsing {}", f.display());
            let mut buffer = String::new();
            File::open(f.clone())
                .and_then(|mut f| f.read_to_string(&mut buffer))
                .map_err(Into::into)
                .and_then(|_| parse_extend(&buffer, &mut model, &f))
                .map_err(|e| format_err!("Failed to parse {}\n{}", f.display(), e.as_fail()))
        })
        .map(|_| model)
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! stringvec {
        ( $( $x:expr ),* ) => {
            vec!($( $x.to_owned(), )*)
        };
    }

    impl Return {
        fn new(type_: Option<Type>, nececitty: Nececitty, promise: bool) -> Return {
            Return {
                type_,
                nececitty,
                promise,
            }
        }
    }

    fn parse<T: TryFrom<Pair<'static, Rule>>>(r: Rule, s: &'static str) -> Result<T, Error> {
        let mut a = MidlParser::parse(r, s).map_err(|e| format_err!("{}", e))?;
        let v = TryFrom::try_from(a.next().unwrap())?;
        if a.next().is_some() {
            Err(format_err!("Not everything parsed with {:?}: {}", r, s))
        } else {
            Ok(v)
        }
    }

    #[test]
    fn pod() {
        assert_eq!(parse::<PodType>(Rule::POD, "i8").unwrap(), PodType::I8);
        assert_eq!(parse::<PodType>(Rule::POD, "i16").unwrap(), PodType::I16);
        assert_eq!(parse::<PodType>(Rule::POD, "i32").unwrap(), PodType::I32);
        assert_eq!(parse::<PodType>(Rule::POD, "u8").unwrap(), PodType::U8);
        assert_eq!(parse::<PodType>(Rule::POD, "u16").unwrap(), PodType::U16);
        assert_eq!(parse::<PodType>(Rule::POD, "u32").unwrap(), PodType::U32);
        assert_eq!(parse::<PodType>(Rule::POD, "u64").unwrap(), PodType::U64);
        assert_eq!(parse::<PodType>(Rule::POD, "f32").unwrap(), PodType::F32);
        assert_eq!(parse::<PodType>(Rule::POD, "f64").unwrap(), PodType::F64);
        assert_eq!(parse::<PodType>(Rule::POD, "string").unwrap(), PodType::String);
    }

    #[test]
    fn pod_java() {
        assert_eq!(parse::<PodType>(Rule::POD, "char").unwrap(), PodType::U8);
        assert_eq!(parse::<PodType>(Rule::POD, "byte").unwrap(), PodType::I8);
        assert_eq!(parse::<PodType>(Rule::POD, "short").unwrap(), PodType::I16);
        assert_eq!(parse::<PodType>(Rule::POD, "int").unwrap(), PodType::I32);
        assert_eq!(parse::<PodType>(Rule::POD, "long").unwrap(), PodType::I64);
        assert_eq!(parse::<PodType>(Rule::POD, "float").unwrap(), PodType::F32);
        assert_eq!(parse::<PodType>(Rule::POD, "double").unwrap(), PodType::F64);
        assert_eq!(parse::<PodType>(Rule::POD, "String").unwrap(), PodType::String);
    }

    #[test]
    fn type_pod() {
        assert_eq!(parse::<Type>(Rule::TYPE, "i8").unwrap(), Type::Pod(PodType::I8));
        assert_eq!(parse::<Type>(Rule::TYPE, "i16").unwrap(), Type::Pod(PodType::I16));
        assert_eq!(parse::<Type>(Rule::TYPE, "i32").unwrap(), Type::Pod(PodType::I32));
        assert_eq!(parse::<Type>(Rule::TYPE, "u8").unwrap(), Type::Pod(PodType::U8));
        assert_eq!(parse::<Type>(Rule::TYPE, "u16").unwrap(), Type::Pod(PodType::U16));
        assert_eq!(parse::<Type>(Rule::TYPE, "u32").unwrap(), Type::Pod(PodType::U32));
        assert_eq!(parse::<Type>(Rule::TYPE, "u64").unwrap(), Type::Pod(PodType::U64));
        assert_eq!(parse::<Type>(Rule::TYPE, "f32").unwrap(), Type::Pod(PodType::F32));
        assert_eq!(parse::<Type>(Rule::TYPE, "f64").unwrap(), Type::Pod(PodType::F64));
        assert_eq!(parse::<Type>(Rule::TYPE, "string").unwrap(), Type::Pod(PodType::String));
    }

    #[test]
    fn type_array() {
        let a = parse::<Type>(Rule::TYPE, "bool[]").unwrap();
        let b = Collection::new_array(Type::Pod(PodType::Bool));
        assert_eq!(a, b);
    }

    #[test]
    fn type_array_ident() {
        let a = parse::<Type>(Rule::TYPE, "Test[]").unwrap();
        let test = "Test".into();
        let b = Collection::new_array(Type::Object(Object::Unresolved(test)));
        assert_eq!(a, b);
    }

    #[test]
    fn type_array_fqn() {
        let a = parse::<Type>(Rule::TYPE, "foo.bar.Baz[]").unwrap();
        let b = Collection::new_array(Type::Object(Object::Unresolved("foo.bar.Baz".into())));
        assert_eq!(a, b);
    }

    #[test]
    fn type_list() {
        let a = parse::<Type>(Rule::TYPE, "List<bool>").unwrap();
        let b = Collection::new_list(Type::Pod(PodType::Bool));
        assert_eq!(a, b);
    }

    #[test]
    fn type_list_of_list() {
        let a = parse::<Type>(Rule::TYPE, "List<List<List<bool>>>").unwrap();
        let inner = Collection::new_list(Type::Pod(PodType::Bool));
        let inner = Collection::new_list(inner);
        let b = Collection::new_list(inner);
        assert_eq!(a, b);
    }

    #[test]
    fn type_list_of_set_of_list() {
        let a = parse::<Type>(Rule::TYPE, "List<Set<List<bool>>>").unwrap();
        let inner = Collection::new_list(Type::Pod(PodType::Bool));
        let inner = Collection::new_set(inner);
        let b = Collection::new_list(inner);
        assert_eq!(a, b);
    }

    #[test]
    fn type_list_ident() {
        let a = parse::<Type>(Rule::TYPE, "List<Foo>").unwrap();
        let f = "Foo".into();
        let b = Collection::new_list(Type::Object(Object::Unresolved(f)));
        assert_eq!(a, b);
    }

    #[test]
    fn type_list_object_fqn() {
        let a = parse::<Type>(Rule::TYPE, "List<bar.Foo>").unwrap();
        let b = Collection::new_list(Type::Object(Object::Unresolved("bar.Foo".into())));
        assert_eq!(a, b);

        let a = parse::<Type>(Rule::TYPE, "List<foo.bar.Foo>").unwrap();
        let b = Collection::new_list(Type::Object(Object::Unresolved("foo.bar.Foo".into())));
        assert_eq!(a, b);
    }

    #[test]
    fn type_set() {
        let a = parse::<Type>(Rule::TYPE, "Set<bool>").unwrap();
        let b = Collection::new_set(Type::Pod(PodType::Bool));
        assert_eq!(a, b);
    }

    #[test]
    fn type_map() {
        let a = parse::<Type>(Rule::TYPE, "Map<i32, string>").unwrap();
        let b = Collection::new_map(PodType::I32, Type::Pod(PodType::String));
        assert_eq!(a, b);
    }

    #[test]
    fn type_map_array() {
        let a = parse::<Type>(Rule::TYPE, "Map<i32, string[]>").unwrap();

        let string_array = Collection::new_array(Type::Pod(PodType::String));
        let b = Collection::new_map(PodType::I32, string_array);
        assert_eq!(a, b);
    }

    #[test]
    fn type_optional() {
        let a = parse::<Type>(Rule::TYPE, "Optional<List<bool>>").unwrap();
        let b = Collection::new_list(Type::Pod(PodType::Bool));
        assert_eq!(a, b);

        let a = parse::<Type>(Rule::TYPE, "Optional<bool>").unwrap();
        let b = Type::Pod(PodType::Bool);
        assert_eq!(a, b);

        assert!(parse::<Type>(Rule::TYPE, "List<Optional<Foo>>").is_err());
        assert!(parse::<Type>(Rule::TYPE, "Map<i32, Optional<Foo>>").is_err());
        assert!(parse::<Type>(Rule::TYPE, "Set<Optional<Foo>>").is_err());

        assert!(parse::<Type>(Rule::TYPE, "List<Optional<bool>>").is_err());
        assert!(parse::<Type>(Rule::TYPE, "Map<i32, Optional<bool>>").is_err());
        assert!(parse::<Type>(Rule::TYPE, "Set<Optional<bool>>").is_err());
    }

    #[test]
    fn oneway() {
        assert!(!parse::<Oneway>(Rule::ONEWAY, "").unwrap().0);
        assert!(!parse::<Oneway>(Rule::ONEWAY, "foo").unwrap().0);
        assert!(parse::<Oneway>(Rule::ONEWAY, "oneway").unwrap().0);
    }

    #[test]
    fn name() {
        let name = parse::<Name>(Rule::NAME, "A").unwrap();
        assert_eq!(name, "A".into());

        let name = parse::<Name>(Rule::NAME, "h5n1").unwrap();
        assert_eq!(name, "h5n1".into());

        let name = parse::<Name>(Rule::NAME, "h5n1_").unwrap();
        assert_eq!(name, "h5n1_".into());

        let name = parse::<Name>(Rule::NAME, "java.utils.HashMap").unwrap();
        assert_eq!(name, "java.utils.HashMap".into());

        let name = parse::<Name>(Rule::NAME, "HashMap").unwrap();
        assert_eq!(name, "HashMap".into());

        let name = parse::<Name>(Rule::NAME, "AbC.aBc.ABC").unwrap();
        assert_eq!(name, "AbC.aBc.ABC".into());
    }

    #[test]
    fn package() {
        let p = parse::<Package>(Rule::PACKAGE, "package a.b.c;").unwrap();
        assert_eq!(p, Package(stringvec!("a", "b", "c")));

        let p = parse::<Package>(Rule::PACKAGE, "package a.B.c;").unwrap();
        assert_eq!(p, Package(stringvec!("a", "B", "c")));

        let p = parse::<Package>(Rule::PACKAGE, "package java.utils;").unwrap();
        assert_eq!(p, Package(stringvec!("java", "utils")));
    }

    #[test]
    fn package_missing_semicolon() {
        assert!(parse::<Package>(Rule::PACKAGE, "package a.b.c").is_err());
    }

    #[test]
    fn method_attributes() {
        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "void").unwrap(),
            Return::new(None, Nececitty::Mandatory, false)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "i32").unwrap(),
            Return::new(Some(Type::Pod(PodType::I32)), Nececitty::Mandatory, false)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "string").unwrap(),
            Return::new(Some(Type::Pod(PodType::String)), Nececitty::Mandatory, false)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Optional<string>").unwrap(),
            Return::new(Some(Type::Pod(PodType::String)), Nececitty::Optional, false)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Optional<bool>").unwrap(),
            Return::new(Some(Type::Pod(PodType::Bool)), Nececitty::Optional, false)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Optional<Promise<bool>>").unwrap(),
            Return::new(Some(Type::Pod(PodType::Bool)), Nececitty::Optional, true)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Optional<Promise<List<bool>>>").unwrap(),
            Return::new(
                Some(Collection::new_list(Type::Pod(PodType::Bool))),
                Nececitty::Optional,
                true
            )
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Optional<Promise<Foo>>").unwrap(),
            Return::new(
                Some(Type::Object(Object::Unresolved("Foo".into()))),
                Nececitty::Optional,
                true
            )
        );
    }

    #[test]
    fn method_type_promise() {
        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Promise<void>").unwrap(),
            Return::new(None, Nececitty::Mandatory, true)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Promise<i32>").unwrap(),
            Return::new(Some(Type::Pod(PodType::I32)), Nececitty::Mandatory, true)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Promise<string>").unwrap(),
            Return::new(Some(Type::Pod(PodType::String)), Nececitty::Mandatory, true)
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Promise<Foo>").unwrap(),
            Return::new(
                Some(Type::Object(Object::Unresolved("Foo".into()))),
                Nececitty::Mandatory,
                true
            )
        );

        assert_eq!(
            parse::<Return>(Rule::METHOD_TYPE, "Promise<bar.Foo>").unwrap(),
            Return::new(
                Some(Type::Object(Object::Unresolved("bar.Foo".into()))),
                Nececitty::Mandatory,
                true
            )
        );
    }

    #[test]
    fn method() {
        let method = parse::<Method>(Rule::METHOD, "void foo();").unwrap();
        assert_eq!(method.ident, "foo");
        assert!(method.type_.is_none());
        assert!(!method.is_promise());
        assert!(!method.is_oneway());
        assert!(method.arguments.is_empty());

        let method = parse::<Method>(Rule::METHOD, "void foo() oneway;").unwrap();
        assert!(method.is_oneway());

        let method = parse::<Method>(Rule::METHOD, "Promise<void> foo();").unwrap();
        assert!(method.is_promise());
        let method = parse::<Method>(Rule::METHOD, "Promise<i32> foo();").unwrap();
        assert!(method.is_promise());
    }

    #[test]
    fn enumeration() {
        let enumeration = r#"
            package bar;
            enum Foo {
                @Blah(a=2)
                Foo,
                Bar,
                Baz,
            }
        "#;

        let mut model = Model::default();
        let path = PathBuf::from("");

        parse_extend(enumeration, &mut model, &path).unwrap();
        assert!(model.find_enumeration(&"bar.Foo".into()).is_some());
    }

    #[test]
    fn enumeration_with_fqn() {
        let enumeration = r#"
            enum foo.bar.Foo {
            }
        "#;

        let mut model = Model::default();
        let path = PathBuf::from("");

        parse_extend(enumeration, &mut model, &path).unwrap();
        assert!(model.find_enumeration(&"foo.bar.Foo".into()).is_some());
    }

    #[test]
    fn enumeration_item() {
        assert_eq!(
            parse::<EnumerationItem>(Rule::ENUM_ITEM, "Foo,").unwrap().ident(),
            &"Foo".to_string()
        );
        assert_eq!(
            parse::<EnumerationItem>(Rule::ENUM_ITEM, "@Foo Bar,").unwrap().ident(),
            &"Bar".to_string()
        );
    }

    #[test]
    fn interface() {
        let interface = r#"
            package test;
            interface TestInterface {
                void foo();
            }
        "#;

        let mut model = Model::default();
        let path = PathBuf::from("/tmp/test.midl");

        parse_extend(interface, &mut model, &path).unwrap();

        let interface = model.find_interface(&"test.TestInterface".into()).unwrap();
        assert!(model.find_interface(&"test.TestInterface".into()).is_some());
        assert!(interface.borrow().methods.iter().any(|m| m.ident == "foo"));
    }

    #[test]
    fn interface_with_fqn() {
        let interface = r#"
            package foo;
            interface bar.TestInterface {
                void foo();
            }
        "#;

        let mut model = Model::default();
        parse_extend(interface, &mut model, &PathBuf::from("/tmp/test.midl")).unwrap();
        assert!(model.find_interface(&"bar.TestInterface".into()).is_some());
    }

    #[test]
    fn class() {
        let class = r#"
            package foo;
            class Test {
                i32 bar;
            }
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("/tmp/test.midl")).unwrap();
        assert!(model.find_class(&"foo.Test".into()).is_some());
    }

    #[test]
    fn class_single_line() {
        let class = "package foo; class Test { i32 bar; }";
        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("/tmp/test.midl")).unwrap();
        assert!(model.find_class(&"foo.Test".into()).is_some());
    }

    #[test]
    fn class_with_fqn() {
        let class = r#"
            package foo;
            class bar.baz.Test {
                i32 bar;
            }
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("/tmp/test.midl")).unwrap();
        assert!(model.find_class(&"bar.baz.Test".into()).is_some());
    }

    #[test]
    fn enummeration() {
        let class = r#"
            package foo;
            enum Bar {}
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("/tmp/test.midl")).unwrap();
        assert!(model.find_enumeration(&"foo.Bar".into()).is_some());
    }

    #[test]
    fn annotation() {
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo()").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a)").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a=10)").unwrap();
        assert_eq!(
            parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a=-10)")
                .unwrap()
                .find_numeric("a")
                .unwrap(),
            -10
        );
        assert_eq!(
            parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a=2:2)")
                .unwrap()
                .find_string("a")
                .unwrap(),
            "2:2".to_string()
        );
        assert_eq!(
            parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a=2-2)")
                .unwrap()
                .find_string("a")
                .unwrap(),
            "2-2".to_string()
        );
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a, b)").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a=1, b)").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a, b=1)").unwrap();
        parse::<AnnotationGroup>(Rule::ANNOTATION_GROUP, "@Foo(a, b=1, @Bar(a=0))").unwrap();
    }

    #[test]
    fn annotations() {
        let class = r#"
            @Foo
            struct foo.Bar {}

            @Foo(a, b=12, c=baz)
            struct foo.Foo {}
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("")).unwrap();
        let class = model.find_class(&"foo.Bar".into()).unwrap();
        assert!(class.borrow().annotations.contains("Foo"));

        let class = model.find_class(&"foo.Foo".into()).unwrap();
        let group = class.borrow().annotations.group("Foo").unwrap().clone();
        assert!(group.contains("a"));
        assert_eq!(group.find_numeric("b").unwrap(), 12);
        assert_eq!(group.find_string("c").unwrap(), "baz");
    }

    #[test]
    fn annotations_nested() {
        let class = r#"
            @Foo(a, b=12, c=baz, @Bar(a, b=12, c=baz))
            struct baz.Baz {}
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("")).unwrap();
        let class = model.find_class(&"baz.Baz".into()).unwrap();
        assert!(class.borrow().annotations.contains("Foo"));

        let class = class.borrow();
        let class_bar = class.annotations.group("Foo").unwrap().find_group("Bar").unwrap();
        assert!(class_bar.contains("a"));
        assert!(class_bar.contains("b"));
        assert_eq!(class_bar.find_numeric("b").unwrap(), 12);
        assert_eq!(class_bar.find_string("c").unwrap(), "baz");
    }

    #[test]
    fn annotations_nested_deep() {
        let class = r#"
            @Foo(bar, @Foo(baz, @Foo(bar, @Foo(@Foo))))
            struct baz.Baz {}
        "#;

        let mut model = Model::default();
        parse_extend(class, &mut model, &PathBuf::from("")).unwrap();

        let class = model.find_class(&"baz.Baz".into()).unwrap();
        let class = class.borrow();
        let mut group_foo = class.annotations.group("Foo").unwrap();
        for _ in 0..4 {
            assert!(group_foo.contains("Foo"));
            group_foo = group_foo.find_group("Foo").unwrap();
        }
    }
}
