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

use clap::{crate_name, crate_version, App, AppSettings, Arg, ArgMatches, SubCommand};
use failure::{err_msg, format_err, Error};
use log::{debug, error};
use midl_common::{generator, model};
use std::{convert::From, fs::File, io::Read, path::PathBuf};

const LICENSE_ESRLABS: &str = r#"Copyright (c) {year} E.S.R.Labs. All rights reserved.

NOTICE:  All information contained herein is, and remains
the property of E.S.R.Labs and its suppliers, if any.
The intellectual and technical concepts contained herein are
proprietary to E.S.R.Labs and its suppliers and may be covered
by German and Foreign Patents, patents in process, and are protected
by trade secret or copyright law.
Dissemination of this information or reproduction of this material
is strictly forbidden unless prior written permission is obtained
from E.S.R.Labs."#;

const LICENSE_APACHE: &str = r#"Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License."#;

fn run() -> Result<i32, Error> {
    let app = App::new(crate_name!())
        .version(crate_version!())
        .setting(AppSettings::ColoredHelp)
        .setting(AppSettings::SubcommandRequired)
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .help("Verbose output"),
        );

    fn add_generators<'a, 'b>(subcommand: App<'a, 'b>) -> App<'a, 'b> {
        #[cfg(feature = "someip")]
        let subcommand = add_someip_options(subcommand);
        let subcommand = add_uml_options(subcommand);
        let subcommand = add_json_options(subcommand);
        let subcommand = add_midl_options(subcommand);
        add_mindroid_options(subcommand)
    }

    let midl = midl_subcommand();
    let midl = add_generators(midl);
    let app = app.subcommand(midl);

    #[cfg(feature = "fibex")]
    let app = {
        let fibex = fibex_subcommand();
        let fibex = add_generators(fibex);
        app.subcommand(fibex)
    };

    let matches = app.get_matches();

    let level = if matches.is_present("verbose") {
        log::LevelFilter::Debug
    } else {
        log::LevelFilter::Warn
    };
    env_logger::Builder::from_default_env().filter(None, level).init();

    let (model, matches) = match matches.subcommand() {
        #[cfg(feature = "fibex")]
        ("fibex", Some(sub_matches)) => input_files(sub_matches, "xml")
            .and_then(|ref p| {
                let options = fibex::Options {
                    coding_bool: sub_matches.value_of("coding-bool"),
                    coding_annotations: sub_matches.is_present("coding-annotations"),
                };
                fibex::parse(p, &options)
            })
            .map(|model| (model, sub_matches))?,
        ("midl", Some(sub_matches)) => input_files(sub_matches, "midl")
            .and_then(|ref p| midl_common::parser::midl::parse(p))
            .map(|model| (model, sub_matches))?,
        (_, _) => unreachable!(), // clap takes care for unknown subcommands
    };

    let mut model = model::resolve(model)?;

    // Apply package prefix if set
    if let Some(package_prefix) = matches.value_of("package-prefix") {
        for o in model.objects.values_mut() {
            match o {
                model::Object::Class(ref c) => c.borrow_mut().name.package.push_front(package_prefix),
                model::Object::Enumeration(ref e) => e.borrow_mut().name.package.push_front(package_prefix),
                model::Object::Interface(ref i) => i.borrow_mut().name.package.push_front(package_prefix),
                model::Object::Unresolved(_) => unreachable!("Unresolved object"),
            }
        }
    }

    // Apply promise option if set: Make all methods async that are not oneway.
    if matches.is_present("promise") {
        for o in model.objects.values_mut() {
            match o {
                model::Object::Interface(ref i) => {
                    i.borrow_mut()
                        .methods
                        .iter_mut()
                        .filter(|m| !m.annotations.contains(model::ONEWAY_ANNOTATION))
                        .for_each(|m| {
                            let group = model::AnnotationGroup::new(model::PROMISE_ANNOTATION.into());
                            m.annotations.push(group);
                        });
                }
                model::Object::Unresolved(_) => unreachable!("Unresolved object"),
                _ => (),
            }
        }
    }

    match matches.subcommand() {
        ("uml", Some(matches)) => generator::uml::generate(&model, matches.value_of("output")),
        ("json", Some(matches)) => generator::json::generate(&model, matches.value_of("output")),
        ("midl", Some(matches)) => generate_midl(&model, matches),
        ("mindroid", Some(matches)) => generate_mindroid(&model, matches),
        #[cfg(feature = "someip")]
        ("someip", Some(matches)) => generate_someip(&model, matches),
        (_, _) => unreachable!(), // clap takes care for unknown subcommands
    }
}

fn generate_mindroid<'a>(model: &model::Model, matches: &ArgMatches<'a>) -> Result<i32, Error> {
    let language = matches.value_of("language").expect("Option error").into();
    let out = matches
        .value_of("output")
        .ok_or_else(|| err_msg("Missing output value"))
        .map(PathBuf::from)?;
    let license = license_header(matches)?;
    let license = license.as_ref().map(String::as_str);
    let options = generator::mindroid::Options { language, out, license };

    generator::mindroid::generate(model, &options)
}

fn generate_midl<'a>(model: &model::Model, matches: &ArgMatches<'a>) -> Result<i32, Error> {
    let mode = if let Some(o) = matches.value_of("output") {
        let o = PathBuf::from(o);
        if matches.is_present("package") {
            generator::midl::OutputMode::Package(o)
        } else if matches.is_present("split") {
            generator::midl::OutputMode::Split(o)
        } else {
            generator::midl::OutputMode::Package(o)
        }
    } else {
        generator::midl::OutputMode::Stdout
    };
    let license = license_header(matches)?;

    generator::midl::generate(&model, license.as_ref().map(String::as_str), mode)
}

#[cfg(feature = "someip")]
fn generate_someip<'a>(model: &model::Model, matches: &ArgMatches<'a>) -> Result<i32, Error> {
    let out = matches
        .value_of("output")
        .ok_or_else(|| err_msg("Missing output value"))
        .map(PathBuf::from)?;
    let flavor = matches
        .value_of("flavor")
        .ok_or_else(|| err_msg("Missing flavor"))
        .map(someip::Flavor::from)?;
    let license = license_header(matches)?;
    let license = license.as_ref().map(String::as_str);
    let options = someip::Options { flavor, out, license };

    someip::generate(model, &options)
}

/// Add midl parser options
fn midl_subcommand<'a, 'b>() -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("midl")
        .version(crate_version!())
        .setting(AppSettings::SubcommandRequired)
        .about("Parse midl");
    let subcommand = add_package_prefix_option(subcommand);
    let subcommand = add_promise_option(subcommand);

    subcommand.arg(
        Arg::with_name("input")
            .help("MIDL file, files or directory")
            .long("input")
            .short("i")
            .required(true)
            .takes_value(true),
    )
}

/// Add fibex parser options
#[cfg(feature = "fibex")]
fn fibex_subcommand<'a, 'b>() -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("fibex")
        .version(crate_version!())
        .setting(AppSettings::SubcommandRequired)
        .about("Parse fibex");
    let subcommand = add_package_prefix_option(subcommand);
    let subcommand = add_promise_option(subcommand);

    subcommand
        .arg(
            Arg::with_name("input")
                .help("Fibex file, files or directory")
                .long("input")
                .short("i")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("coding-bool")
                .help("Replace u8/i8 pods originated from a coding named <CODING> with 'bool'")
                .long("coding-bool")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("coding-annotations")
                .help("Add a annotation with original fibex coding to types")
                .long("coding-annotations"),
        )
}

/// Add a subcommand for the someip generator
#[cfg(feature = "someip")]
fn add_someip_options<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("someip")
        .version(crate_version!())
        .about("Generate someip");
    let subcommand = add_language_option(subcommand, &["java"]);
    let subcommand = add_output_option(subcommand, true, "Output directory");
    let subcommand = add_license_option(subcommand);
    let subcommand = subcommand.arg(
        clap::Arg::with_name("flavor")
            .short("f")
            .long("flavor")
            .required(true)
            .takes_value(true)
            .possible_values(&["rpc", "standalone"])
            .default_value("rpc")
            .help("Select flavor to generate"),
    );

    app.subcommand(subcommand)
}

/// Add a subcommand for the uml generator
fn add_uml_options<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("uml")
        .version(crate_version!())
        .about("Generate plantuml");
    let subcommand = add_output_option(subcommand, false, "Output file");
    app.subcommand(subcommand)
}

/// Add a subcommand for the json generator
fn add_json_options<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("json")
        .version(crate_version!())
        .about("Generate json");
    let subcommand = add_output_option(subcommand, false, "Output file");
    app.subcommand(subcommand)
}

/// Add a subcommand for the midl generator
fn add_midl_options<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("midl")
        .version(crate_version!())
        .about("Generate midl");
    let subcommand = subcommand
        .arg(
            clap::Arg::with_name("package")
                .short("p")
                .long("package")
                .requires("output")
                .conflicts_with("split")
                .help("Group output by package"),
        )
        .arg(
            clap::Arg::with_name("split")
                .short("s")
                .long("split")
                .requires("output")
                .conflicts_with("package")
                .help("Output each struct or interface into dedicated file"),
        );
    let subcommand = add_output_option(subcommand, false, "Output file or directory");
    let subcommand = add_license_option(subcommand);

    app.subcommand(subcommand)
}

/// Add a subcommand for the mindroid generator
fn add_mindroid_options<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    let subcommand = SubCommand::with_name("mindroid")
        .version(crate_version!())
        .about("Generate Mindroid protocol code");
    let subcommand = add_language_option(subcommand, &["c++", "java"]);
    let subcommand = add_output_option(subcommand, true, "Output directory");
    let subcommand = add_license_option(subcommand);

    app.subcommand(subcommand)
}

/// Attach language option to app
fn add_language_option<'a, 'b>(app: App<'a, 'b>, possible_values: &[&'a str]) -> App<'a, 'b> {
    app.arg(
        Arg::with_name("language")
            .short("l")
            .long("language")
            .required(true)
            .takes_value(true)
            .possible_values(possible_values)
            .help("Select language"),
    )
}

/// Attach output option to app
fn add_output_option<'a, 'b>(app: App<'a, 'b>, required: bool, help: &'static str) -> App<'a, 'b> {
    app.arg(
        Arg::with_name("output")
            .short("o")
            .long("output")
            .takes_value(true)
            .required(required)
            .help(help),
    )
}

/// Attach license and copyright options to app
fn add_license_option<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    app.arg(
        clap::Arg::with_name("license")
            .long("license")
            .short("L")
            .takes_value(true)
            .help("Select license for generated code: ESRLabs, Apache2.0 or <FILE> (optional)"),
    )
    .arg(
        clap::Arg::with_name("copyright")
            .long("copyright")
            .short("c")
            .takes_value(true)
            .help("Copyright text (optional)"),
    )
}

fn add_package_prefix_option<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    app.arg(
        clap::Arg::with_name("package-prefix")
            .long("package-prefix")
            .takes_value(true)
            .help("Prefix package"),
    )
}

fn add_promise_option<'a, 'b>(app: App<'a, 'b>) -> App<'a, 'b> {
    app.arg(
        clap::Arg::with_name("promise")
            .long("promise")
            .help("Turn all non oneway methods into async versions"),
    )
}

fn license_header(matches: &ArgMatches<'_>) -> Result<Option<String>, Error> {
    let license = if let Some(license) = matches.value_of("license") {
        Some(match license {
            "ESRLabs" => LICENSE_ESRLABS
                .to_string()
                .replace("{year}", &(time::now().tm_year + 1900).to_string()),
            "Apache2.0" => LICENSE_APACHE.to_string(),
            _ => {
                debug!("Trying to read {}", license);
                let mut buffer = String::new();
                File::open(&PathBuf::from(license))
                    .map_err(|e| format_err!("Failed to read license file: {}", e))?
                    .read_to_string(&mut buffer)?;
                buffer
            }
        })
    } else {
        None
    };

    Ok(matches
        .value_of("copyright")
        .map(|c| {
            format!(
                "Copyright (C) {}\n\n{}",
                c,
                license.clone().unwrap_or_else(|| "".to_string())
            )
        })
        .or(license))
}

fn input_files(matches: &ArgMatches<'_>, ext: &str) -> Result<Vec<PathBuf>, Error> {
    let inputs = matches
        .values_of("input")
        .ok_or_else(|| err_msg("Input argument is missing"))?;

    let mut files = vec![];

    for i in inputs {
        let path = PathBuf::from(i);

        if path.is_file() {
            files.push(path);
        } else if path.is_dir() {
            for f in glob::glob(&format!("{}/**/*.{}", path.display(), ext))? {
                files.push(f?);
            }
        } else {
            return Err(format_err!("Cannot find {}", path.display()));
        }
    }

    Ok(files)
}

fn main() {
    match run() {
        Ok(i) => std::process::exit(i),
        Err(e) => {
            error!("{}", e);
            std::process::exit(1);
        }
    }
}
