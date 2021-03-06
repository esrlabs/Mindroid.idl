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

use crate::model::{Model, Object};
use failure::Error;
use std::{
    fs::File,
    io::{stdout, BufWriter, Write},
    path::PathBuf,
};

pub fn generate(model: &Model, out: Option<&str>) -> Result<i32, Error> {
    if let Some(f) = out {
        File::create(PathBuf::from(f))
            .map_err(Error::from)
            .and_then(|f| write(model, f))
    } else {
        write(model, stdout())
    }
}

fn write<T: Write>(model: &Model, w: T) -> Result<i32, Error> {
    let mut w = BufWriter::new(w);
    writeln!(w, "@startuml")?;

    for object in model.objects.values() {
        match object {
            Object::Class(ref class) => {
                let class = &*class.borrow();
                writeln!(w, "class {} {{", class.name)?;
                for field in &class.fields {
                    writeln!(w, "    {} {}", field.type_, field.ident)?;
                }
                writeln!(w, "}}")?;
                writeln!(w)?;

                for dependency in class.dependencies(true) {
                    writeln!(w, "{} *-- {}", class.name, dependency)?;
                }
                writeln!(w)?;
            }
            Object::Interface(ref interface) => {
                let interface = &*interface.borrow();
                writeln!(w, "interface {} {{", interface.name)?;
                for method in &interface.methods {
                    writeln!(w, "    {}", method)?;
                }
                writeln!(w, "}}")?;

                writeln!(w)?;
                for dependency in interface.dependencies(true) {
                    writeln!(w, "{} -> {}", interface.name, dependency)?;
                }
                writeln!(w)?;
            }
            Object::Enumeration(enumeration) => {
                let enumeration = &*enumeration.borrow();
                writeln!(w, "enum {} {{", enumeration.name)?;
                for item in &enumeration.items {
                    writeln!(w, "    {}", item.ident())?;
                }
                writeln!(w, "}}")?;
                writeln!(w)?;
            }
            Object::Unresolved(_) => panic!("Unresolved"),
        }
    }

    writeln!(w, "@enduml")?;
    Ok(0)
}
