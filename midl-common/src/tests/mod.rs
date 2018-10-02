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

use crate::{model, parser};
use std::{
    env,
    path::{Path, PathBuf},
};
use subprocess::{Exec, Redirection};

// Set MIDL_TEST_DRY in env to skip builds during tests
const MIDL_TEST_DRY: &str = "MIDL_TEST_DRY";
// Overwrite GitHub Mindroid java repository if needed
const MIDL_TEST_MINDROID_JAVA: &str = "MIDL_TEST_MINDROID_JAVA";
// Overwrite GitHub Mindroid java repository if needed
const MIDL_TEST_MINDROID_CPP: &str = "MIDL_TEST_MINDROID_CPP";

const DEFAULT_MINDROID_CPP: &str = "https://github.com/esrlabs/Mindroid.cpp.git";
const DEFAULT_MINDROID_JAVA: &str = "https://github.com/esrlabs/Mindroid.java.git";

#[test]
fn test_compact_disc_player() {
    let model = parse_midl(&workspace_root().join("examples/compact_disc_player.midl"));
    test_cpp(&model);
    test_java(&model);
    test_roundtrip(&model);
}

#[test]
fn test_location() {
    let model = parse_midl(&workspace_root().join("examples/location"));
    test_cpp(&model);
    test_java(&model);
    test_roundtrip(&model);
}

#[test]
fn test_test() {
    let model = parse_midl(&workspace_root().join("midl-common/src/tests/test.midl"));
    test_cpp(&model);
    test_java(&model);
    test_roundtrip(&model);
}

#[cfg(test)]
fn test_cpp(model: &model::Model) {
    let dir = tempdir();
    generate_mindroid(&model, dir.as_path(), crate::generator::Language::Cpp);
    compile_cpp(dir.as_path());
}

#[cfg(test)]
fn test_java(model: &model::Model) {
    let dir = tempdir();
    generate_mindroid(&model, dir.as_path(), crate::generator::Language::Java);
    compile_java(dir.as_path());
}

pub fn test_roundtrip(model: &model::Model) {
    let dir = tempdir();

    crate::generator::midl::generate(&model, None, crate::generator::midl::OutputMode::Package(dir.clone()))
        .expect("Failed to generate");
    let pattern = format!("{}/**/*", dir.display());
    let files = glob::glob(&pattern).unwrap().map(Result::unwrap).collect::<Vec<_>>();
    let parsed = parser::midl::parse(&files)
        .and_then(model::resolve)
        .unwrap_or_else(|e| {
            eprintln!("{}", e);
            panic!("Failed to parse");
        });

    assert_eq!(model, &parsed);
}

pub fn parse_midl(input: &PathBuf) -> model::Model {
    println!("Parsing {}...", input.display());
    let model = if input.is_dir() {
        let files = glob::glob(&format!("{}/*.midl", input.display()))
            .unwrap()
            .map(Result::ok)
            .filter_map(|e| e)
            .collect::<Vec<PathBuf>>();
        println!("{:?}", files);
        parser::midl::parse(&files).unwrap_or_else(|e| {
            eprintln!("{}", e);
            panic!("Failed to parse");
        })
    } else {
        parser::midl::parse(&[input.to_owned()]).unwrap_or_else(|e| {
            eprintln!("{}", e);
            panic!("Failed to parse");
        })
    };

    model::resolve(model).unwrap_or_else(|e| {
        eprintln!("{}", e);
        panic!("Failed to parse");
    })
}

#[cfg(test)]
fn generate_mindroid(model: &model::Model, dir: &Path, language: crate::generator::Language) {
    let out = dir.join("gen");
    println!("Generating {} objects", model.objects.len());
    let options = crate::generator::mindroid::Options {
        language,
        license: None,
        out,
    };
    crate::generator::mindroid::generate(&model, &options).expect("Failed to generate");
}

pub fn compile_cpp(build_dir: &Path) {
    let mindroid = env::var(MIDL_TEST_MINDROID_CPP).unwrap_or_else(|_| DEFAULT_MINDROID_CPP.into());
    clone(&mindroid, &build_dir.join("Mindroid.cpp"));

    let src_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("src/tests");
    copy(&src_dir.join("CMakeLists.txt"), &build_dir);

    if env::var(MIDL_TEST_DRY).is_ok() {
        println!("Skipping build");
    } else {
        println!("Building...");
        shell(Some(&build_dir), "cmake .");
        shell(Some(&build_dir), &format!("make -j {}", num_cpus::get()));
    }
}

pub fn compile_java(build_dir: &Path) {
    let mindroid = env::var(MIDL_TEST_MINDROID_JAVA).unwrap_or_else(|_| DEFAULT_MINDROID_JAVA.into());
    clone(&mindroid, &build_dir.join("Mindroid.java"));

    let src_dir = workspace_root().join("midl-common/src/tests");
    copy(&src_dir.join("gradle"), &build_dir);
    copy(&src_dir.join("build.gradle"), &build_dir);
    copy(&src_dir.join("gradlew"), &build_dir);

    if env::var(MIDL_TEST_DRY).is_ok() {
        println!("Skipping build");
    } else {
        println!("Building...");
        shell(Some(&build_dir), "./gradlew build")
    }
}

fn copy(src: &Path, dest: &Path) {
    let options = fs_extra::dir::CopyOptions::new();
    fs_extra::copy_items(&vec![src], dest, &options).expect("Failed to copy");
}

fn shell(pwd: Option<&Path>, cmd: &str) {
    println!("Running {}", cmd);
    let cmd = if let Some(pwd) = pwd {
        format!("cd {}; {}", pwd.display(), cmd)
    } else {
        cmd.to_string()
    };

    let status = Exec::shell(cmd.clone())
        .stdout(Redirection::Pipe)
        .stderr(Redirection::Merge)
        .capture()
        .unwrap_or_else(|_| panic!("Failed to run shell command: {}", cmd));

    if !status.success() {
        println!("{}: {}", cmd, status.stdout_str());
    }
    assert!(status.success());
}

pub fn clone(from: &str, to: &Path) {
    let cmd = format!("git clone --depth 1 {} {}", from, to.display().to_string());
    shell(None, &cmd);
}

pub fn tempdir() -> PathBuf {
    tempdir::TempDir::new("midl")
        .expect("Failed to create tempdir")
        .into_path()
}

pub fn workspace_root() -> PathBuf {
    PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("..")
}
