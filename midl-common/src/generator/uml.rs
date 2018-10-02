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

    for o in model.objects.values() {
        match o {
            Object::Class(ref c) => {
                let c = &*c.borrow();
                writeln!(w, "class {} {{", c.name)?;
                for f in &c.fields {
                    writeln!(w, "    {} {}", f.type_, f.ident)?;
                }
                writeln!(w, "}}")?;
                writeln!(w)?;

                for d in c.dependencies(true) {
                    writeln!(w, "{} *-- {}", c.name, d)?;
                }
                writeln!(w)?;
            }
            Object::Interface(ref i) => {
                let i = &*i.borrow();
                writeln!(w, "interface {} {{", i.name)?;
                for m in &i.methods {
                    writeln!(w, "    {}", m)?;
                }
                writeln!(w, "}}")?;

                writeln!(w)?;
                for d in i.dependencies(true) {
                    writeln!(w, "{} -> {}", i.name, d)?;
                }
                writeln!(w)?;
            }
            Object::Enumeration(e) => {
                let e = &*e.borrow();
                writeln!(w, "enum {} {{", e.name)?;
                for i in &e.items {
                    writeln!(w, "    {}", i.ident())?;
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
