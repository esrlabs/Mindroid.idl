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

use crate::model::{Argument, Collection, Ident, Name, Object, PodType, Type};
use std::convert::Into;

/// Java code formatting
pub trait Java {
    /// Java representation
    fn java(&self) -> String;

    /// Java object representation
    fn object(&self) -> String {
        self.java()
    }

    fn optional(&self, is_optional: bool) -> String {
        if is_optional {
            format!("Optional<{}>", self.object())
        } else {
            self.java()
        }
    }

    fn optional_object(&self, is_optional: bool) -> String {
        if is_optional {
            format!("Optional<{}>", self.object())
        } else {
            self.object()
        }
    }
}

impl Java for Ident {
    fn java(&self) -> String {
        self.to_string()
    }
}

impl Java for Name {
    fn java(&self) -> String {
        if self.package.is_empty() {
            self.ident.clone()
        } else {
            format!("{}.{}", self.package.as_path().join("."), self.ident)
        }
    }
}

impl<T: Java> Java for Option<T> {
    fn java(&self) -> String {
        if let Some(ref t) = self {
            t.java()
        } else {
            "void".into()
        }
    }

    fn object(&self) -> String {
        if let Some(t) = self {
            t.object()
        } else {
            "Void".into()
        }
    }
}

impl Java for PodType {
    fn java(&self) -> String {
        match self {
            PodType::Bool => "boolean",
            PodType::U8 => "short",
            PodType::U16 => "int",
            PodType::U32 => "long",
            PodType::U64 => "long",
            PodType::I8 => "byte",
            PodType::I16 => "short",
            PodType::I32 => "int",
            PodType::I64 => "long",
            PodType::F32 => "float",
            PodType::F64 => "double",
            PodType::String => "String",
        }
        .into()
    }

    fn object(&self) -> String {
        match self {
            PodType::Bool => "Boolean",
            PodType::U8 => "Short",
            PodType::U16 => "Integer",
            PodType::U32 => "Long",
            PodType::U64 => "Long",
            PodType::I8 => "Byte",
            PodType::I16 => "Short",
            PodType::I32 => "Integer",
            PodType::I64 => "Long",
            PodType::F32 => "Float",
            PodType::F64 => "Double",
            PodType::String => "String",
        }
        .into()
    }
}

impl Java for Type {
    fn java(&self) -> String {
        match self {
            Type::Pod(ref p) => p.java(),
            Type::Object(ref o) => o.java(),
            Type::Collection(ref c) => c.java(),
        }
    }

    fn object(&self) -> String {
        match self {
            Type::Pod(ref p) => p.object(),
            Type::Object(ref o) => o.object(),
            Type::Collection(ref c) => c.object(),
        }
    }
}

impl Java for Object {
    fn java(&self) -> String {
        match self {
            Object::Class(o) => o.borrow().name.java(),
            Object::Enumeration(e) => e.borrow().name.java(),
            Object::Interface(i) => i.borrow().name.java(),
            Object::Unresolved(name) => panic!("Encountered unresolved type {} during generation", name),
        }
    }
}

impl Java for Argument {
    fn java(&self) -> String {
        format!(
            "{} {}",
            if self.is_optional() {
                self.type_.optional(self.is_optional())
            } else {
                self.type_.java()
            },
            self.ident
        )
    }
}

/// Generate argument list string for signature use
impl Java for Vec<Argument> {
    fn java(&self) -> String {
        self.iter().map(Java::java).collect::<Vec<String>>().join(", ")
    }
}

impl Java for Collection {
    fn java(&self) -> String {
        match self {
            Collection::Array(ref c) => format!("{}[]", c.java()),
            Collection::List(ref c) => format!("List<{}>", c.object()),
            Collection::Map(ref k, ref v) => format!("Map<{}, {}>", k.object(), v.object()),
            Collection::Set(ref p) => format!("Set<{}>", p.object()),
        }
    }
}

pub mod enumeration {
    use crate::{
        fmt,
        model::Enumeration,
        utils::{writer, Formatter},
    };
    use failure::Error;
    use std::{io::Write, path::Path};

    pub fn generate(e: &Enumeration, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&e.name, out, "java")?);
        let name = &e.name;
        let ident = &name.ident;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }
        if !package.as_path().is_empty() {
            fmt!(fmt, "package {};", package.as_path().join("."))?;
            fmt.newline()?;
        }

        fmt!(fmt, "public enum {} {{", ident)?;
        fmt.increment();
        for i in &e.items {
            fmt!(fmt, "{},", i.ident())?;
        }
        fmt.decrement();

        fmt!(fmt, "}")?;

        fmt.flush().map_err(Into::into)
    }
}

pub mod class {
    use super::Java;
    use crate::model::{Class, Collection, Dependency, Field, Name, Object, PodType, Type};
    use crate::{
        fmt,
        utils::{writer, Formatter, Uppercase},
    };
    use failure::{format_err, Error};
    use itertools::Itertools;
    use std::{io::Write, path::Path};

    /// Extension for fields
    pub trait FieldExt {
        fn java_type(&self) -> String;
    }

    impl FieldExt for Field {
        /// Return the Java representation of the fields type including the optional case
        fn java_type(&self) -> String {
            if self.is_optional() {
                format!("Optional<{}>", self.type_.object())
            } else {
                self.type_.java()
            }
        }
    }

    #[cfg_attr(
        feature = "cargo-clippy",
        allow(clippy::cyclomatic_complexity, clippy::cognitive_complexity)
    )]
    pub fn generate(c: &Class, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let name = &c.name;
        let ident = &name.ident;
        let class_name = ident;
        let package = &name.package;
        let mut fmt = Formatter::new(writer(&c.name, out, "java")?);

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        if !package.as_path().is_empty() {
            fmt!(fmt, "package {};", package.as_path().join("."))?;
            fmt.newline()?;
        }

        let mut imports: Vec<Name> = Vec::new();
        let deps = c.dependencies(false);

        if deps.contains(&Dependency::Array) {
            imports.push("java.util.Arrays".into());
        }

        if deps.contains(&Dependency::List) {
            imports.push("java.util.List".into());
            imports.push("java.util.ArrayList".into());
        }

        if deps.contains(&Dependency::Map) {
            imports.push("java.util.Map".into());
            imports.push("java.util.HashMap".into());
        }

        if deps.contains(&Dependency::Set) {
            imports.push("java.util.Set".into());
            imports.push("java.util.HashSet".into());
        }

        if deps.contains(&Dependency::Optional) {
            imports.push("java.util.Optional".into());
            imports.push("java.util.Objects".into());
        }

        for i in &imports {
            fmt!(fmt, "import {};", i.as_path().join("."))?;
        }
        if !imports.is_empty() {
            fmt.newline()?;
        }

        fmt!(fmt, "public class {} {{", ident)?;
        // Declare constants
        fmt.increment();
        for c in &c.constants {
            let ty = c.type_.java();
            let ident = &c.ident;
            let value = &c.value;
            fmt!(fmt, "public static final {} {} = {};", ty, ident, value)?;
        }
        fmt.decrement();

        // Members
        fmt.increment();
        for field in &c.fields {
            fmt!(fmt, "private {} m{};", field.java_type(), field.ident.uppercase())?;
        }
        fmt.decrement();

        // Default constructor
        fmt.increment();
        fmt.newline()?;
        fmt!(fmt, "public {}() {{", class_name)?;
        fmt.increment();
        //Optional should never be null, so initialize with empty
        c.fields
            .iter()
            .filter(|field| field.is_optional())
            .try_for_each(|field| fmt!(fmt, "m{} = Optional.empty();", field.ident.uppercase()))?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.decrement();

        // Copy constructor
        fmt.increment();
        fmt.newline()?;
        fmt!(fmt, "public {name}({name} other) {{", name = class_name)?;
        fmt.increment();
        for field in &c.fields {
            let ident = &field.ident;
            let member = format!("m{}", ident.uppercase());
            let ty = &field.type_;
            generate_copy(
                &mut fmt,
                &format!("this.{}", member),
                &format!("other.{}", member),
                ty,
                field.is_optional(),
            )?;
        }
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.decrement();

        fmt.newline()?;

        // Value constructor
        if !c.fields.is_empty() {
            fmt.increment();
            let mut args = c.fields.iter().map(|f| format!("{} {}", f.java_type(), f.ident));
            fmt!(fmt, "public {}({}) {{", class_name, args.join(", "))?;
            fmt.increment();
            for field in &c.fields {
                if field.is_optional() {
                    fmt!(fmt, "Objects.requireNonNull({});", field.ident)?;
                }
                fmt!(fmt, "this.m{} = {};", field.ident.uppercase(), field.ident)?;
            }
            fmt.decrement();
            fmt!(fmt, "}")?;
            fmt.decrement();
        }

        // Implement getter and setter
        fmt.increment();
        for field in &c.fields {
            fmt.newline()?;
            let ident = &field.ident;
            let member = format!("m{}", ident.uppercase());
            let ty = field.java_type();

            // get
            fmt!(fmt, "public {} get{}() {{", ty, ident.uppercase())?;
            fmt.increment();
            fmt!(fmt, "return {};", member)?;
            fmt.decrement();
            fmt!(fmt, "}")?;

            // set
            fmt!(
                fmt,
                "public {} set{}({} {}) {{",
                class_name,
                ident.uppercase(),
                ty,
                ident
            )?;
            fmt.increment();
            if field.is_optional() {
                fmt!(fmt, "Objects.requireNonNull({});", ident)?;
            }
            fmt!(fmt, "this.{} = {};", member, ident)?;
            fmt!(fmt, "return this;")?;
            fmt.decrement();
            fmt!(fmt, "}")?;
        }
        if !c.fields.is_empty() {
            fmt.newline()?;
        }
        fmt.decrement();

        // Implement equals
        fmt.increment();
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public boolean equals(final Object object) {")?;
        fmt.increment();
        fmt!(fmt, "if (object == this) return true;")?;
        fmt!(
            fmt,
            "if (object == null || getClass() != object.getClass()) return false;"
        )?;
        fmt.newline()?;
        if !c.fields.is_empty() {
            fmt!(fmt, "final {} other = ({}) object;", ident, ident)?;
        }
        for field in &c.fields {
            let ident = field.ident.uppercase();
            if field.is_optional() {
                fmt!(fmt, "if (!m{i}.equals(other.m{i})) return false;", i = ident)?
            } else {
                match &field.type_ {
                    Type::Pod(PodType::F32) => {
                        fmt!(
                            fmt,
                            "if (Float.compare(other.m{i}, m{i}) != 0) return false;",
                            i = ident
                        )?;
                    }
                    Type::Pod(PodType::F64) => {
                        fmt!(
                            fmt,
                            "if (Double.compare(other.m{i}, m{i}) != 0) return false;",
                            i = ident
                        )?;
                    }
                    Type::Collection(c) => match **c {
                        Collection::Array(_) => {
                            fmt!(fmt, "if (!Arrays.equals(m{i}, other.m{i})) return false;", i = ident)?
                        }
                        _ => fmt!(
                            fmt,
                            "if (m{i} != null ? !m{i}.equals(other.m{i}) : other.m{i} != null) return false;",
                            i = ident
                        )?,
                    },
                    Type::Pod(PodType::String) | Type::Object(_) => fmt!(
                        fmt,
                        "if (m{i} != null ? !m{i}.equals(other.m{i}) : other.m{i} != null) return false;",
                        i = ident
                    )?,
                    _ => fmt!(fmt, "if (m{i} != other.m{i}) return false;", i = ident)?,
                }
            }
        }
        fmt!(fmt, "return true;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        // Implement hashCode
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public int hashCode() {")?;
        fmt.increment();
        fmt!(fmt, "int result = 0;")?;
        for field in &c.fields {
            let ident = field.ident.uppercase();
            if field.is_optional() {
                fmt!(fmt, "result = 31 * result + (m{i}.hashCode());", i = ident)?;
            } else {
                match &field.type_ {
                    Type::Pod(PodType::Bool) => {
                        fmt!(fmt, "result = 31 * result + (m{} ? 1 : 0);", ident)?;
                    }
                    Type::Pod(PodType::U64) | Type::Pod(PodType::I64) | Type::Pod(PodType::U32) => {
                        fmt!(fmt, "result = 31 * result + (int) (m{i} ^ (m{i} >>> 32));", i = ident)?;
                    }
                    Type::Pod(PodType::F32) => {
                        fmt!(
                            fmt,
                            "result = 31 * result + (m{i} != +0.0f ? Float.floatToIntBits(m{i}) : 0);",
                            i = ident
                        )?;
                    }
                    Type::Pod(PodType::F64) => {
                        fmt!(fmt, "long _{i} = Double.doubleToLongBits(m{i});", i = ident)?;
                        fmt!(fmt, "result = 31 * result + (int) (_{i} ^ (_{i} >>> 32));", i = ident)?;
                    }
                    Type::Collection(c) => match **c {
                        Collection::Array(_) => fmt!(fmt, "result = 31 * result + Arrays.hashCode(m{});", ident)?,
                        _ => {
                            fmt!(
                                fmt,
                                "result = 31 * result + (m{i} != null ? m{i}.hashCode() : 0);",
                                i = ident
                            )?;
                        }
                    },
                    Type::Pod(PodType::String) | Type::Object(_) => fmt!(
                        fmt,
                        "result = 31 * result + (m{i} != null ? m{i}.hashCode() : 0);",
                        i = ident
                    )?,
                    _ => fmt!(fmt, "result = 31 * result + m{};", ident)?,
                }
            }
        }
        fmt!(fmt, "return result;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        // Implement toString
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public String toString() {")?;
        fmt.increment();
        fmt!(fmt, "return \"{}{{\" +", ident)?;
        let mut first = true;
        fmt.increment();
        fmt.increment();
        for field in &c.fields {
            let ident = &field.ident;
            let member = format!("m{}", ident.uppercase());
            let ty = &field.type_;
            let is_string = is_string(ty) && !field.is_optional();
            fmt!(
                fmt,
                "\"{comma}{ident}={string_start}\" + {value} +{string_end}",
                comma = if first { "" } else { ", " },
                ident = ident,
                string_start = if is_string { "'" } else { "" },
                value = if is_array(ty) {
                    format!("Arrays.toString({})", member)
                } else {
                    member
                },
                string_end = if is_string { " '\\'' +" } else { "" }
            )?;
            first = false;
        }
        fmt!(fmt, "\"}\";")?;
        fmt.indent = 1;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;

        fmt.flush().map_err(Into::into)
    }

    fn is_string(ty: &Type) -> bool {
        match ty {
            Type::Pod(PodType::String) => true,
            _ => false,
        }
    }

    fn is_array(ty: &Type) -> bool {
        match ty {
            Type::Collection(c) => match **c {
                Collection::Array(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn generate_copy(
        fmt: &mut Formatter,
        target: &str,
        source: &str,
        ty: &Type,
        is_optional: bool,
    ) -> Result<(), Error> {
        match ty {
            Type::Pod(_) | Type::Object(Object::Enumeration(_)) => fmt!(fmt, "{} = {};", target, source),
            Type::Object(Object::Class(class)) => {
                if is_optional {
                    fmt!(
                        fmt,
                        "{} = {}.map(value -> new {}(value));",
                        target,
                        source,
                        class.as_ref().borrow().name
                    )
                } else {
                    fmt!(fmt, "{} = new {}({});", target, class.as_ref().borrow().name, source)
                }
            }
            Type::Collection(collection) => match collection.as_ref() {
                Collection::Array(sub_type) => {
                    let i = format!("{}i", "_".repeat(fmt.indent));
                    fmt!(fmt, "{} = new {}[{}.length];", target, sub_type.java(), source)?;
                    fmt!(
                        fmt,
                        "for (int {i} = 0; {i} < {target}.length; ++{i}) {{",
                        i = i,
                        target = target
                    )?;
                    fmt.increment();
                    generate_copy(
                        fmt,
                        &format!("{}[{}]", target, i),
                        &format!("{}[{}]", source, i),
                        sub_type,
                        false,
                    )?;
                    fmt.decrement();
                    fmt!(fmt, "}")
                }
                Collection::List(sub_type) | Collection::Set(sub_type) => {
                    let level = "_".repeat(fmt.indent);
                    if let Collection::List(_) = collection.as_ref() {
                        fmt!(fmt, "{} = new ArrayList<>({}.size());", target, source)?;
                    } else {
                        fmt!(fmt, "{} = new HashSet<>({}.size());", target, source)?;
                    }
                    fmt!(fmt, "for ({} {}element : {}) {{", sub_type.object(), level, source)?;
                    fmt.increment();
                    fmt!(fmt, "{} {}value;", sub_type.object(), level)?;
                    generate_copy(
                        fmt,
                        &format!("{}value", level),
                        &format!("{}element", level),
                        sub_type,
                        false,
                    )?;
                    fmt!(fmt, "{}.add({}value);", target, level)?;
                    fmt.decrement();
                    fmt!(fmt, "}")
                }
                Collection::Map(key_type, value_type) => {
                    let level = "_".repeat(fmt.indent);
                    fmt!(fmt, "{} = new HashMap<>({}.size());", target, source)?;
                    fmt!(
                        fmt,
                        "for (Map.Entry<{}, {}> {}entry : {}.entrySet()) {{",
                        key_type.object(),
                        value_type.object(),
                        level,
                        source
                    )?;
                    fmt.increment();
                    fmt!(fmt, "{} {}value;", value_type.object(), level)?;
                    generate_copy(
                        fmt,
                        &format!("{}value", level),
                        &format!("{}entry.getValue()", level),
                        value_type,
                        false,
                    )?;
                    //Since keys can only be pod no need to copy them
                    fmt!(fmt, "{}.put({}entry.getKey(), {}value);", target, level, level)?;
                    fmt.decrement();
                    fmt!(fmt, "}")
                }
            },
            _ => Err(format_err!("Cannot create copy of {}", ty)),
        }
    }
}

pub mod interface {
    use super::Java;
    use crate::{
        fmt, fmt____,
        generator::method_message_id,
        model::{Interface, Method, Model, Name, Object, Type},
        utils::{writer, Formatter},
    };
    use failure::Error;
    use itertools::Itertools;
    use std::{collections::HashSet, io::Write, path::Path};

    pub trait Generator {
        /// Deliver a set of imports this interface depends on
        fn imports(interface: &Interface) -> HashSet<Name>;

        /// Write the on_transact body for this interface
        fn on_transact(interface: &Interface, method: &Method, w: &mut Formatter) -> Result<(), Error>;

        /// Write the proxy method implementation for this interface
        fn proxy_method(interface: &Interface, method: &Method, out: &mut Formatter) -> Result<(), Error>;

        /// Scheme for this interface
        fn scheme() -> &'static str;

        /// Retrieve the message id for this method
        fn message_id(interface: &Interface, method: &Method) -> usize;

        /// Generate descriptor for interface
        fn descriptor(model: &Model, interface: &Interface) -> String;
    }

    /// Generate interface java file
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    pub fn generate<G: Generator>(
        model: &Model,
        i: &Interface,
        out: &Path,
        license: Option<&str>,
    ) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&i.name, out, "java")?);
        let name = &i.name;
        let ident = &name.ident;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        if !package.as_path().is_empty() {
            fmt!(fmt, "package {};", package.as_path().join("."))?;
            fmt.newline()?;
        }

        let mut imports = G::imports(i);

        imports.insert("mindroid.os.Binder".into());
        imports.insert("mindroid.os.IBinder".into());
        imports.insert("mindroid.os.IInterface".into());
        imports.insert("mindroid.os.Parcel".into());
        imports.insert("mindroid.os.RemoteException".into());
        imports.insert("mindroid.util.concurrent.Promise".into());
        // Future is only needed if any method is async
        if i.methods.iter().any(Method::is_promise) {
            imports.insert("mindroid.util.concurrent.Future".into());
        }

        for i in imports.iter().sorted() {
            fmt!(fmt, "import {};", i.as_path().join("."))?;
        }
        fmt.newline()?;

        fmt!(fmt, "public interface {} extends IInterface {{", ident)?;
        fmt.increment();
        fmt!(
            fmt,
            "public static abstract class Stub extends Binder implements {} {{",
            ident
        )?;
        fmt.increment();
        fmt!(
            fmt,
            "public static final String DESCRIPTOR = \"{}\";",
            G::descriptor(model, i)
        )?;
        fmt.newline()?;

        fmt!(fmt, "public Stub() {")?;
        fmt____!(fmt, "this.attachInterface(this, DESCRIPTOR);")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "public static {} asInterface(IBinder binder) {{", ident)?;
        fmt.increment();
        fmt!(fmt, "if (binder == null) {")?;
        fmt____!(fmt, "return null;")?;
        fmt!(fmt, "}")?;
        fmt!(fmt, "return new {}.Proxy(binder);", ident)?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public IBinder asBinder() {")?;
        fmt____!(fmt, "return this;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(
            fmt,
            "protected void onTransact(int what, Parcel data, Promise<Parcel> result) throws RemoteException {"
        )?;
        fmt.increment();
        fmt!(fmt, "switch (what) {")?;
        for m in &i.methods {
            G::on_transact(i, m, &mut fmt)?;
        }
        fmt!(fmt, "default:")?;
        fmt____!(fmt, "super.onTransact(what, data, result);")?;
        fmt____!(fmt, "break;")?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "private static class Proxy implements {} {{", ident)?;
        fmt.increment();

        fmt!(fmt, "private final IBinder mRemote;")?;
        fmt.newline()?;
        fmt!(fmt, "Proxy(IBinder remote) {")?;
        fmt____!(fmt, "mRemote = remote;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public IBinder asBinder() {")?;
        fmt____!(fmt, "return mRemote;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public boolean equals(final Object obj) {")?;
        fmt.increment();
        fmt!(fmt, "if (obj == null) return false;")?;
        fmt!(fmt, "if (obj == this) return true;")?;
        fmt!(fmt, "if (obj instanceof Stub.Proxy) {")?;
        fmt____!(fmt, "final Stub.Proxy that = (Stub.Proxy) obj;")?;
        fmt____!(fmt, "return this.mRemote.equals(that.mRemote);")?;
        fmt!(fmt, "}")?;
        fmt!(fmt, "return false;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public int hashCode() {")?;
        fmt____!(fmt, "return mRemote.hashCode();")?;
        fmt!(fmt, "}")?;

        for m in &i.methods {
            fmt.newline()?;
            G::proxy_method(i, m, &mut fmt)?;
        }

        fmt.decrement();
        fmt!(fmt, "}")?;

        fmt.newline()?;
        for method in &i.methods {
            let id = method_message_id(i, method);
            fmt!(fmt, "static final int {} = {};", id, G::message_id(i, method))?;
        }

        fmt.decrement();
        fmt!(fmt, "}")?;

        fmt.newline()?;

        fmt!(fmt, "static class Proxy implements {} {{", ident)?;

        fmt.increment();
        fmt!(fmt, "private final IBinder mBinder;")?;
        fmt!(fmt, "private final Stub mStub;")?;
        fmt!(fmt, "private final {} mProxy;", ident)?;
        fmt.newline()?;
        fmt!(fmt, "Proxy(IBinder binder) {")?;
        fmt.increment();
        fmt!(fmt, "mBinder = binder;")?;
        fmt!(fmt, "if (binder.getUri().getScheme().equals(\"{}\")) {{", G::scheme())?;
        fmt____!(fmt, "mStub = (Stub) binder.queryLocalInterface(Stub.DESCRIPTOR);")?;
        fmt____!(fmt, "mProxy = new Stub.Proxy(binder);")?;
        fmt!(fmt, "} else {")?;
        fmt____!(
            fmt,
            "mindroid.runtime.system.Runtime runtime = mindroid.runtime.system.Runtime.getRuntime();"
        )?;
        fmt____!(fmt, "mStub = (Stub) runtime.getBinder(binder.getId());")?;
        fmt____!(fmt, "mProxy = ({}) runtime.getProxy(binder);", ident)?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public IBinder asBinder() {")?;
        fmt____!(fmt, "return mBinder;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public boolean equals(final Object obj) {")?;
        fmt.increment();
        fmt!(fmt, "if (obj == null) return false;")?;
        fmt!(fmt, "if (obj == this) return true;")?;
        fmt!(fmt, "if (obj instanceof Proxy) {")?;
        fmt____!(fmt, "final Proxy that = (Proxy) obj;")?;
        fmt____!(fmt, "return this.mBinder.equals(that.mBinder);")?;
        fmt!(fmt, "}")?;
        fmt!(fmt, "return false;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;
        fmt!(fmt, "@Override")?;
        fmt!(fmt, "public int hashCode() {")?;
        fmt____!(fmt, "return mBinder.hashCode();")?;
        fmt!(fmt, "}")?;

        // Proxy methods
        for m in &i.methods {
            fmt.newline()?;
            let ident = &m.ident;
            let ty = return_type(m);
            let args = m.args.java();
            fmt!(fmt, "@Override")?;
            fmt!(fmt, "public {} {}({}) throws RemoteException {{", ty, ident, args)?;
            fmt.increment();
            fmt!(fmt, "if (mStub != null && mStub.isCurrentThread()) {")?;
            let return_ = if m.type_.is_some() || m.is_promise() {
                "return "
            } else {
                ""
            };
            let args = m
                .args
                .iter()
                .map(|a| match &a.type_ {
                    Type::Object(Object::Interface(i)) => {
                        format!("{}.Stub.asInterface({}.asBinder())", i.borrow().name.java(), a.ident)
                    }
                    _ => a.ident.to_owned(),
                })
                .collect::<Vec<String>>()
                .join(", ");
            fmt____!(fmt, "{}mStub.{}({});", return_, ident, args)?;
            fmt!(fmt, "} else {")?;
            let args = m
                .args
                .iter()
                .map(|a| a.ident.as_str())
                .collect::<Vec<&str>>()
                .join(", ");
            fmt____!(fmt, "{}mProxy.{}({});", return_, ident, args)?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "}")?;
        }
        fmt.decrement();
        fmt!(fmt, "}")?;

        fmt.newline()?;

        for m in &i.methods {
            let ident = &m.ident;
            let ty = return_type(m);
            let args = m.args.java();
            fmt!(fmt, "public {} {}({}) throws RemoteException;", ty, ident, args)?;
        }
        fmt.decrement();
        fmt!(fmt, "}")?;

        fmt.flush().map_err(Into::into)
    }

    pub fn return_type(m: &Method) -> String {
        if m.is_promise() {
            format!("Future<{}>", m.type_.optional_object(m.is_optional()))
        } else {
            m.type_.optional(m.is_optional())
        }
    }
}
