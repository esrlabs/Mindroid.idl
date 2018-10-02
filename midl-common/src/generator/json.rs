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

use crate::model::Model;
use failure::Error;
use std::{io, io::Write};

pub fn generate(model: &Model, out: Option<&str>) -> Result<i32, Error> {
    if let Some(f) = out {
        std::fs::File::create(f)
            .map_err(Error::from)
            .and_then(|f| write(f, model))
    } else {
        write(io::stdout(), &model)
    }
}

fn write<T: Write>(mut write: T, model: &Model) -> Result<i32, Error> {
    serde_json::to_string_pretty(&model)
        .map_err(Error::from)
        .and_then(|json| write.write_all(json.as_bytes()).map_err(Error::from))
        .map(|_| 0)
}
