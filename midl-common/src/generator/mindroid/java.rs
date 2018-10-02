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
    fmt, fmt____,
    generator::{method_message_id, Leveled},
    model::{
        Class, Collection, Dependency, Enumeration, Interface, Method, Model, Name, Object, PodType, Type,
        IMPORT_ANNOTATION,
    },
    utils::{
        java::{interface::return_type, Java},
        Formatter, Uppercase,
    },
};
use failure::Error;
use itertools::Itertools;
use log::info;
use std::{collections::HashSet, path::Path};

pub(super) fn generate(model: &Model, out: &Path, license: Option<&str>) -> Result<i32, Error> {
    info!("Generating {} objects", model.objects.len());
    for o in model.objects.values() {
        match o {
            Object::Class(ref c) if !c.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                crate::utils::java::class::generate(&*c.borrow(), out, license)?;
            }
            Object::Enumeration(ref e) if !e.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                crate::utils::java::enumeration::generate(&*e.borrow(), out, license)?;
            }
            Object::Interface(ref i) if !i.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                crate::utils::java::interface::generate::<Generator>(model, &*i.borrow(), out, license)?;
            }
            Object::Unresolved(_) => panic!("Unresolved object"),
            _ => (),
        }
    }
    Ok(0)
}

struct Generator;

impl crate::utils::java::interface::Generator for Generator {
    fn scheme() -> &'static str {
        "mindroid"
    }

    fn message_id(interface: &Interface, method: &Method) -> usize {
        interface.methods.iter().position(|m| m == method).unwrap() + 1
    }

    fn descriptor(_: &Model, interface: &Interface) -> String {
        let ident = &interface.name.ident;
        let package = &interface.name.package;
        format!("mindroid://interfaces/{}/{}", package.as_path().join("/"), ident)
    }

    fn imports(interface: &Interface) -> HashSet<Name> {
        let deps = interface.dependencies(true);

        let mut imports = HashSet::new();

        if deps.contains(&Dependency::Array) {
            imports.insert("java.util.Arrays".into());
            imports.insert("java.util.ArrayList".into());
        }

        if deps.contains(&Dependency::List) {
            imports.insert("java.util.List".into());
            imports.insert("java.util.ArrayList".into());
        }

        if deps.contains(&Dependency::Set) {
            imports.insert("java.util.Set".into());
            imports.insert("java.util.HashSet".into());
        }

        if deps.contains(&Dependency::Map) {
            imports.insert("java.util.Map".into());
            imports.insert("java.util.HashMap".into());
        }

        if deps.contains(&Dependency::Optional) {
            imports.insert("java.util.Optional".into());
        }

        imports
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    fn proxy_method(interface: &Interface, method: &Method, fmt: &mut Formatter) -> Result<(), Error> {
        let ident = &method.ident;
        let r#async = method.is_promise();
        let message_id = method_message_id(interface, method);
        let is_optional = method.is_optional();

        // Method signature
        let ty = return_type(method);
        fmt!(fmt, "@Override")?;
        fmt!(
            fmt,
            "public {} {}({}) throws RemoteException {{",
            ty,
            ident,
            method.args.java(),
        )?;

        fmt.increment();

        if method.type_.is_some() || method.is_promise() {
            if let Some(ty) = &method.type_ {
                fmt!(
                    fmt,
                    "Promise<{ty}> _promise = new Promise<>();",
                    ty = ty.optional_object(is_optional)
                )?;
            } else {
                fmt!(fmt, "Promise<Void> _promise = new Promise<>();")?;
            }
        }

        fmt!(fmt, "Parcel _data = Parcel.obtain();")?;

        let mut first = true;
        for a in &method.args {
            if !first {
                fmt.newline()?;
                first = false;
            }
            let is_optional = a.is_optional();
            let ident = if is_optional {
                fmt!(fmt, "if ({}.isPresent()) {{", a.ident)?;
                fmt.increment();
                let unwrapped = format!("{}Unwrapped", a.ident);
                fmt!(fmt, "{} {} = {}.get();", a.type_.java(), unwrapped, a.ident)?;
                fmt!(fmt, "_data.putBoolean(true);")?;
                unwrapped
            } else {
                a.ident.clone()
            };
            a.type_.put(fmt, &"_data", &ident, 0)?;
            if is_optional {
                fmt.decrement();
                fmt!(fmt, "} else {")?;
                fmt.increment();
                fmt!(fmt, "_data.putBoolean(false);")?;
                fmt.decrement();
                fmt!(fmt, "}")?;
            }
        }

        if method.type_.is_some() || method.is_promise() {
            fmt!(fmt, "mRemote.transact({}, _data, 0)", message_id)?;
            fmt.increment();
            fmt.increment();
            fmt!(fmt, ".then((parcel, exception) -> {")?;
            fmt.increment();
            fmt!(fmt, "if (exception == null) {")?;
            fmt.increment();
            if let Some(t) = &method.type_ {
                fmt!(fmt, "try {")?;
                fmt.increment();
                if is_optional {
                    fmt!(fmt, "if (parcel.getBoolean()) {")?;
                    fmt.increment();
                    t.get(fmt, "parcel", "_reply", 0)?;
                    fmt!(fmt, "_promise.complete(Optional.of(_reply));")?;
                    fmt.decrement();
                    fmt!(fmt, "} else {")?;
                    fmt____!(fmt, "_promise.complete(Optional.empty());")?;
                    fmt!(fmt, "}")?;
                } else {
                    t.get(fmt, "parcel", "_reply", 0)?;
                    fmt!(fmt, "_promise.complete(_reply);")?;
                }
                fmt.decrement();
                fmt!(fmt, "} catch (RemoteException e) {")?;
                fmt____!(fmt, "_promise.completeWith(e);")?;
                fmt!(fmt, "}")?;
            } else {
                // Promise.complete(null) never throws
                fmt!(fmt, "_promise.complete(null);")?;
            }
            fmt.decrement();
            fmt!(fmt, "} else {")?;
            fmt____!(fmt, "_promise.completeWith(exception);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "});")?;
            fmt.decrement();
            fmt.decrement();
            if r#async {
                fmt!(fmt, "return _promise;")?;
            } else {
                fmt!(fmt, "return Binder.get(_promise);")?;
            }
        } else if method.is_oneway() {
            fmt!(fmt, "mRemote.transact({}, _data, FLAG_ONEWAY);", message_id)?;
        } else {
            fmt!(fmt, "Binder.get(mRemote.transact({}, _data, 0));", message_id)?;
        }
        fmt.decrement();
        fmt!(fmt, "}")?;

        Ok(())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    fn on_transact(interface: &Interface, method: &Method, fmt: &mut Formatter) -> Result<(), Error> {
        let ident = &method.ident;
        let args = method.args.iter().map(|a| a.ident.leveled(1)).join(", ");
        let message_id = method_message_id(interface, method);

        let get_args = |fmt: &mut Formatter| -> Result<(), Error> {
            let mut first = true;
            for a in &method.args {
                if !first {
                    fmt.newline()?;
                    first = false;
                }
                let is_optional = a.is_optional();
                let ident = if is_optional {
                    fmt!(
                        fmt,
                        "{} {} = Optional.empty();",
                        a.type_.optional(true),
                        a.ident.leveled(1)
                    )?;
                    fmt!(fmt, "if (data.getBoolean()) {")?;
                    fmt.increment();
                    format!("{}Unwrapped", a.ident)
                } else {
                    a.ident.clone()
                };
                a.type_.get(fmt, &"data", &ident, 1)?;
                if is_optional {
                    fmt!(fmt, "{} = Optional.of({});", a.ident.leveled(1), ident.leveled(1))?;
                    fmt.decrement();
                    fmt!(fmt, "}")?;
                }
            }
            Ok(())
        };

        let is_optional = method.is_optional();

        fmt!(fmt, "case {}: {{", message_id)?;
        fmt.increment();
        if method.is_promise() {
            let ty = method.type_.optional_object(is_optional);
            get_args(fmt)?;
            fmt!(fmt, "Future<{}> _reply = {}({});", ty, ident, args)?;
            fmt!(fmt, "_reply.then((value, exception) -> {")?;
            fmt.increment();
            fmt!(fmt, "if (exception == null) {")?;
            fmt.increment();
            fmt!(fmt, "Parcel _parcel = Parcel.obtain();")?;
            fmt!(fmt, "try {")?;
            fmt.increment();
            if let Some(ty) = &method.type_ {
                let value = if is_optional {
                    fmt!(fmt, "if (value.isPresent()) {")?;
                    fmt.increment();
                    fmt!(fmt, "_parcel.putBoolean(true);")?;
                    fmt!(fmt, "{} valueUnwrapped = value.get();", ty.java())?;
                    "valueUnwrapped"
                } else {
                    "value"
                };
                ty.put(fmt, &"_parcel", value, 0)?;
                if is_optional {
                    fmt.decrement();
                    fmt!(fmt, "} else {")?;
                    fmt____!(fmt, "_parcel.putBoolean(false);")?;
                    fmt!(fmt, "}")?;
                }
            }
            fmt!(fmt, "result.complete(_parcel);")?;
            fmt.decrement();
            fmt!(fmt, "} catch (Exception e) {")?;
            fmt____!(fmt, "result.completeWith(e);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "} else {")?;
            fmt____!(fmt, "result.completeWith(exception);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "});")?;
        } else if let Some(ty) = &method.type_ {
            get_args(fmt)?;
            fmt!(fmt, "{} _reply = {}({});", ty.optional(is_optional), ident, args)?;
            fmt!(fmt, "Parcel _parcel = Parcel.obtain();")?;
            let reply = if is_optional {
                fmt!(fmt, "if (_reply.isPresent()) {")?;
                fmt.increment();
                fmt!(fmt, "_parcel.putBoolean(true);")?;
                fmt!(fmt, "{} _replyUnwrapped = _reply.get();", ty.java())?;
                "_replyUnwrapped"
            } else {
                "_reply"
            };
            ty.put(fmt, &"_parcel", reply, 0)?;
            if is_optional {
                fmt.decrement();
                fmt!(fmt, "} else {")?;
                fmt____!(fmt, "_parcel.putBoolean(false);")?;
                fmt!(fmt, "}")?;
            }
            fmt!(fmt, "result.complete(_parcel);")?;
        } else if method.is_oneway() {
            get_args(fmt)?;
            fmt!(fmt, "{}({});", ident, args)?;
        } else {
            get_args(fmt)?;
            fmt!(fmt, "{}({});", ident, args)?;
            fmt!(fmt, "result.complete(Parcel.obtain());")?;
        }
        fmt!(fmt, "break;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;

        Ok(())
    }
}

/// Implementations of something that can be tranfered via a Parcel
trait Parcelable {
    /// Generate code to encode "ident" into a parcel of name "parcel"
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error>;
    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error>;
}

impl Parcelable for PodType {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, _: usize) -> Result<(), Error> {
        let setter = match self {
            PodType::Bool => "putBoolean",
            PodType::U8 => "putShort",
            PodType::U16 => "putInt",
            PodType::U32 => "putLong",
            PodType::U64 => "putLong",
            PodType::I8 => "putByte",
            PodType::I16 => "putShort",
            PodType::I32 => "putInt",
            PodType::I64 => "putLong",
            PodType::F32 => "putFloat",
            PodType::F64 => "putDouble",
            PodType::String => "putString",
        };

        fmt!(fmt, "{}.{}({});", parcel, setter, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        let getter = match self {
            PodType::Bool => "getBoolean",
            PodType::U8 => "getShort",
            PodType::U16 => "getInt",
            PodType::U32 => "getLong",
            PodType::U64 => "getLong",
            PodType::I8 => "getByte",
            PodType::I16 => "getShort",
            PodType::I32 => "getInt",
            PodType::I64 => "getLong",
            PodType::F32 => "getFloat",
            PodType::F64 => "getDouble",
            PodType::String => "getString",
        };

        fmt!(
            fmt,
            "{} {} = {}.{}();",
            self.java(),
            ident.leveled(level),
            parcel,
            getter
        )
    }
}

impl Parcelable for Type {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Type::Collection(c) => c.put(fmt, parcel, ident, level),
            Type::Object(ref o) => o.put(fmt, parcel, ident, level),
            Type::Pod(ref p) => p.put(fmt, parcel, ident, level),
        }
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Type::Collection(c) => c.get(fmt, parcel, ident, level),
            Type::Object(ref o) => o.get(fmt, parcel, ident, level),
            Type::Pod(ref p) => p.get(fmt, parcel, ident, level),
        }
    }
}

impl Parcelable for Object {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Object::Class(c) => c.borrow().put(fmt, parcel, ident, level),
            Object::Enumeration(e) => e.borrow().put(fmt, parcel, ident, level),
            Object::Interface(i) => i.borrow().put(fmt, parcel, ident, level),
            Object::Unresolved(_) => panic!("Encountered unresolved type during generation"),
        }
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Object::Class(c) => c.borrow().get(fmt, parcel, ident, level),
            Object::Enumeration(e) => e.borrow().get(fmt, parcel, ident, level),
            Object::Interface(i) => i.borrow().get(fmt, parcel, ident, level),
            Object::Unresolved(_) => panic!("Encountered unresolved type during generation"),
        }
    }
}

impl Parcelable for Enumeration {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, _: usize) -> Result<(), Error> {
        fmt!(fmt, "{}.putInt({}.ordinal());", parcel, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        fmt!(
            fmt,
            "{} {} = {}.values()[{}.getInt()];",
            self.name.java(),
            ident.leveled(level),
            self.name.java(),
            parcel
        )
    }
}

impl Parcelable for Interface {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, _: usize) -> Result<(), Error> {
        fmt!(fmt, "{}.putBinder({}.asBinder());", parcel, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        fmt!(
            fmt,
            "{} {} = {}.Stub.asInterface({}.getBinder());",
            self.name.java(),
            ident.leveled(level),
            self.name.java(),
            parcel
        )
    }
}

impl Parcelable for Class {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        for f in &self.fields {
            let var = format!("{}{}", ident, f.ident.uppercase());
            let is_optional = f.is_optional();
            fmt!(
                fmt,
                "{ty} {inst}{ident} = {inst}.get{ident}();",
                ty = f.type_.optional(is_optional),
                inst = ident,
                ident = f.ident.uppercase()
            )?;
            if is_optional {
                fmt!(fmt, "if ({}.isPresent()) {{", var)?;
                fmt.increment();
                fmt!(fmt, "{}.putBoolean(true);", parcel)?;
            }
            let var = if is_optional {
                let new_var = format!("{}Unwrapped", var);
                fmt!(fmt, "{} {} = {}.get();", f.type_.java(), new_var, var)?;
                new_var
            } else {
                var
            };
            f.type_.put(fmt, parcel, &var, level)?;
            if is_optional {
                fmt.decrement();
                fmt!(fmt, "} else {")?;
                fmt.increment();
                fmt!(fmt, "{}.putBoolean(false);", parcel)?;
                fmt.decrement();
                fmt!(fmt, "}")?;
            }
        }
        Ok(())
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        // instance
        fmt!(
            fmt,
            "{} {} = new {}();",
            self.name.java(),
            ident.leveled(level),
            self.name.java()
        )?;

        if self.fields.is_empty() {
            // In case there are no fields, no parcel calls are done but
            // we're required to throw and cannot know this upstream.
            fmt!(fmt, "if ({} == null) throw new RemoteException();", parcel)?;
        } else {
            for f in &self.fields {
                let var = format!("{}{}", ident, f.ident.uppercase());
                let is_optional = f.is_optional();
                if is_optional {
                    fmt!(fmt, "if ({}.getBoolean()) {{", parcel)?;
                    fmt.increment();
                }
                f.type_.get(fmt, parcel, &var, level)?;
                if is_optional {
                    fmt!(
                        fmt,
                        "{}.set{}(Optional.of({}));",
                        ident.leveled(level),
                        f.ident.uppercase(),
                        format!("{}{}", ident, f.ident.uppercase()).leveled(level)
                    )?;
                    fmt.decrement();
                    fmt!(fmt, "}")?;
                } else {
                    fmt!(
                        fmt,
                        "{}.set{}({});",
                        ident.leveled(level),
                        f.ident.uppercase(),
                        format!("{}{}", ident, f.ident.uppercase()).leveled(level)
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl Parcelable for Collection {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Collection::Array(ref t) | Collection::List(ref t) | Collection::Set(ref t) => {
                // length
                match self {
                    Collection::Array(_) => fmt!(fmt, "{}.putInt({}.length);", parcel, ident)?,
                    Collection::List(_) | Collection::Set(_) => fmt!(fmt, "{}.putInt({}.size());", parcel, ident)?,
                    _ => unreachable!(),
                };

                let value = format!("i{}", ident);
                fmt!(
                    fmt,
                    "for ({ty} {value}: {ident}) {{",
                    ident = ident,
                    ty = t.java(),
                    value = value,
                )?;
                fmt.increment();
                t.put(fmt, parcel, &value, level + 1)?;
                fmt.decrement();
                fmt!(fmt, "}")
            }
            Collection::Map(key, value) => {
                // length
                fmt!(fmt, "{}.putInt({}.size());", parcel, ident)?;

                let entry = format!("{}Entry", ident).leveled(level);

                fmt!(
                    fmt,
                    "for (Map.Entry<{key_type}, {value_type}> {entry} : {ident}.entrySet()) {{",
                    key_type = key.object(),
                    value_type = value.object(),
                    entry = entry,
                    ident = ident
                )?;
                fmt.increment();

                // key
                fmt!(
                    fmt,
                    "{} {} = {}.getKey();",
                    key.object(),
                    "key".leveled(level + 1),
                    entry
                )?;
                key.put(fmt, parcel, &"key".leveled(level + 1), level + 1)?;

                // value
                fmt!(
                    fmt,
                    "{} {} = {}.getValue();",
                    value.object(),
                    "value".leveled(level + 1),
                    entry
                )?;
                value.put(fmt, parcel, &"value".leveled(level + 1), level + 1)?;
                fmt.decrement();
                fmt!(fmt, "}")
            }
        }
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Collection::Array(ref c) | Collection::List(ref c) | Collection::Set(ref c) => {
                // length
                let size_var = format!("{}Size", ident).leveled(level);
                fmt!(fmt, "int {} = {}.getInt();", size_var, parcel)?;
                let ident = ident.leveled(level);

                // instance
                let construct = match self {
                    Collection::Array(t) => format!("new {}[{}]", t.java(), size_var),
                    Collection::List(_) => "new ArrayList<>()".into(),
                    Collection::Set(_) => "new HashSet<>()".into(),
                    Collection::Map(_, _) => unreachable!(),
                };
                fmt!(
                    fmt,
                    "{ty} {ident} = {construct};",
                    ty = self.java(),
                    construct = construct,
                    ident = ident,
                )?;

                // elements
                let i = "i".leveled(level);
                fmt!(fmt, "for (int {i} = 0; {i} < {}; {i}++) {{", size_var, i = i)?;
                fmt.increment();

                c.get(fmt, parcel, "value", level + 1)?;
                let var = "value".leveled(level + 1);

                match self {
                    Collection::Array(_) => fmt!(fmt, "{}[{}] = {};", ident, i, var)?,
                    Collection::List(_) | Collection::Set(_) => fmt!(fmt, "{}.add({});", ident, var)?,
                    _ => unreachable!(),
                };

                fmt.decrement();
                fmt!(fmt, "}")?;
                fmt.newline()
            }
            Collection::Map(ref key, ref value) => {
                // length
                let size_var = format!("{}Size", ident);
                fmt!(fmt, "int {} = {}.getInt();", size_var, parcel)?;

                // instance
                let ident = ident.leveled(level);
                fmt!(fmt, "{} {} = new HashMap<>();", self.java(), ident)?;

                let i = "i".leveled(level);
                fmt!(fmt, "for (int {i} = 0; {i} < {}; {i}++) {{", size_var, i = i)?;
                fmt.increment();

                key.get(fmt, parcel, "key", level + 1)?;
                value.get(fmt, parcel, "value", level + 1)?;
                let key = "key".leveled(level + 1);
                let value = "value".leveled(level + 1);
                fmt!(fmt, "{}.put({}, {});", ident, key, value)?;

                fmt.decrement();
                fmt!(fmt, "}")?;
                fmt.newline()
            }
        }
    }
}
