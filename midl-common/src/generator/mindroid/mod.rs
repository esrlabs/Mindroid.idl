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
    generator::{sanitize, Language},
    model::Model,
};
use failure::Error;
use std::path::PathBuf;

mod cpp;
mod java;

/// Mindroid code generator options
#[derive(Debug)]
pub struct Options<'a> {
    /// Target language
    pub language: Language,
    /// Outupt directory
    pub out: PathBuf,
    /// License header
    pub license: Option<&'a str>,
}

/// Generate Mindroid RPC code from model with options
pub fn generate(model: &Model, options: &Options<'_>) -> Result<i32, Error> {
    let license = options.license.map(|license| {
        let license = license
            .lines()
            .map(|l| format!(" *{}{}\n", if l.is_empty() { "" } else { " " }, l))
            .collect::<String>();
        format!("/*\n{} */", license)
    });

    let license = license.as_ref().map(|x| &**x);

    sanitize(model, &options.language);

    match options.language {
        Language::Java => java::generate(&model, &options.out, license),
        Language::Cpp => cpp::generate(&model, &options.out, license),
    }
}
