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

use crate::model::{Ident, Interface, Method, Model, Object};
use heck::ShoutySnakeCase;
use std::{collections::HashSet, iter::FromIterator};

pub mod json;
pub mod midl;
pub mod mindroid;
pub mod uml;

const JAVA_KEYWORDS: &[&str] = &[
    "abstract",
    "continue",
    "for",
    "new",
    "switch",
    "assert",
    "default",
    "goto",
    "package",
    "synchronized",
    "boolean",
    "do",
    "if",
    "private",
    "this",
    "break",
    "double",
    "implements",
    "protected",
    "throw",
    "byte",
    "else",
    "import",
    "public",
    "throws",
    "case",
    "enum",
    "instanceof",
    "return",
    "transient",
    "catch",
    "extends",
    "int",
    "short",
    "try",
    "char",
    "final",
    "interface",
    "static",
    "void",
    "class",
    "finally",
    "long",
    "strictfp",
    "volatile",
    "const",
    "float",
    "native",
    "super",
    "while",
    "null",
];

const CPP_KEYWORDS: &[&str] = &[
    "NULL",
    "alignas",
    "alignof",
    "and",
    "and_eq",
    "asm",
    "auto",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "char16_t",
    "char32_t",
    "class",
    "compl",
    "const",
    "const_cast",
    "constexpr",
    "continue",
    "decltype",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "final",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "noexcept",
    "not",
    "not_eq",
    "nullptr",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "thread_local",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq",
];

const RESERVED: &[&str] = &["Object", "String"];

pub fn sanitize(model: &Model, language: &Language) {
    let reserved: HashSet<&str> = HashSet::from_iter(RESERVED.iter().cloned().chain(match language {
        Language::Cpp => CPP_KEYWORDS.iter().cloned(),
        Language::Java => JAVA_KEYWORDS.iter().cloned(),
    }));

    let sanitize_ident = |i: &mut Ident| {
        match i.chars().next() {
            Some(c) => match c {
                '_' | 'a'..='z' | 'A'..='Z' => (),
                _ => i.insert(0, '_'),
            },
            _ => i.insert(0, '_'),
        }

        if reserved.contains(&i.as_ref()) {
            i.push('_');
        }
    };

    for object in model.objects.values() {
        match object {
            Object::Enumeration(enumeration) => {
                let mut enumeration = enumeration.borrow_mut();

                sanitize_ident(&mut enumeration.name.ident);
                for entry in &mut enumeration.items {
                    sanitize_ident(&mut entry.ident);
                }
            }
            Object::Class(class) => {
                let mut class = class.borrow_mut();

                sanitize_ident(&mut class.name.ident);
                for field in &mut class.fields {
                    sanitize_ident(&mut field.ident)
                }
                for class in &mut class.constants {
                    sanitize_ident(&mut class.ident)
                }
            }
            Object::Interface(interface) => {
                let mut interface = interface.borrow_mut();

                sanitize_ident(&mut interface.name.ident);
                for method in &mut interface.methods {
                    sanitize_ident(&mut method.ident);
                    for argument in &mut method.arguments {
                        sanitize_ident(&mut argument.ident)
                    }
                }
            }
            Object::Unresolved(_) => unreachable!(),
        }
    }
}

/// Prefixed ident names
trait Leveled {
    fn leveled(self, level: usize) -> String;
}

impl<'a> Leveled for &'a str {
    fn leveled(self, l: usize) -> String {
        format!("{}{}", "_".repeat(l), self)
    }
}

#[derive(Clone, Debug)]
pub enum Language {
    Cpp,
    Java,
}

impl<'a> From<&'a str> for Language {
    fn from(s: &str) -> Language {
        match s {
            "c++" => Language::Cpp,
            "java" => Language::Java,
            _ => panic!("Invalid language {}", s),
        }
    }
}

impl ToString for Language {
    fn to_string(&self) -> String {
        match self {
            Language::Cpp => "c++".to_owned(),
            Language::Java => "java".to_owned(),
        }
    }
}

/// Generate a unique identifier for the message ids e.g. MSG_ASK
pub fn method_message_id(interface: &Interface, method: &Method) -> String {
    // Must find this method, otherwise the model is broken
    let position = interface.methods.iter().position(|m| m == method).unwrap();
    // Counter number of methods with identical identifier *before* method
    let count = interface.methods[..position]
        .iter()
        .filter(|m| m.ident == method.ident)
        .count();
    if count == 0 {
        format!("MSG_{}", method.ident.to_shouty_snake_case())
    } else {
        format!("MSG_{}{}", method.ident.to_shouty_snake_case(), count)
    }
}
