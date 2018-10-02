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
    fmt,
    generator::Leveled,
    model::{
        Argument, Class, Collection, Dependency, Enumeration, Interface, Model, Name, Object, Package, PodType, Type,
        IMPORT_ANNOTATION,
    },
    utils::{Formatter, Uppercase},
};
use failure::Error;
use log::info;
use std::{
    collections::{BTreeSet, HashSet},
    convert::Into,
    path::Path,
};

/// Generate mindroid c++ code into out
pub(super) fn generate(model: &Model, out: &Path, license: Option<&str>) -> Result<i32, Error> {
    info!("Generating {} objects", model.objects.len());
    for o in model.objects.values() {
        match o {
            Object::Class(ref c) if !c.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                class::generate(&*c.borrow(), out, license)?;
            }
            Object::Enumeration(ref e) if !e.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                enumeration::generate(&*e.borrow(), out, license)?;
            }
            Object::Interface(ref i) if !i.borrow().annotations.contains(IMPORT_ANNOTATION) => {
                interface::generate(&*i.borrow(), out, license)?;
            }
            Object::Unresolved(_) => panic!("Unresolved object"),
            _ => (),
        }
    }
    Ok(0)
}

mod enumeration {
    use super::{guard_close, guard_open, namespace_close, namespace_open};
    use crate::{
        fmt, fmt____,
        model::Enumeration,
        utils::{writer, Formatter},
    };
    use failure::Error;
    use std::{io::Write, path::Path};

    pub(super) fn generate(e: &Enumeration, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&e.name, out, "h")?);

        let name = &e.name;
        let ident = &name.ident;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        guard_open(&mut fmt, name)?;
        fmt.newline()?;

        namespace_open(&mut fmt, package)?;
        fmt.newline()?;

        fmt!(fmt, "enum class {} {{", ident)?;
        for i in &e.items {
            fmt____!(fmt, "{},", i.ident())?;
        }

        fmt!(fmt, "};")?;
        fmt.newline()?;

        namespace_close(&mut fmt, package)?;
        fmt.newline()?;

        guard_close(&mut fmt, name)?;

        fmt.flush().map_err(Into::into)
    }
}

mod class {
    use super::{guard_close, guard_open, map_dependencies, namespace_close, namespace_open, Cpp};
    use crate::{
        fmt, fmt____,
        model::{Class, Constant, Object, PodType, Type},
        utils::{writer, Formatter, Uppercase},
    };
    use failure::Error;
    use std::{io::Write, path::Path};

    pub(super) fn generate(c: &Class, out: &Path, license: Option<&str>) -> Result<(), Error> {
        // Generate cpp file for classes only if needed
        let is_pod_string = |c: &Constant| {
            if let Type::Pod(PodType::String) = c.type_ {
                true
            } else {
                false
            }
        };
        if c.constants.iter().any(is_pod_string) {
            cpp(c, out, license)?;
        }

        header(c, out, license)
    }

    fn header(c: &Class, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&c.name, out, "h")?);
        let name = &c.name;
        let ident = &name.ident;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        guard_open(&mut fmt, &name)?;
        fmt.newline()?;

        // Includes
        fmt!(fmt, "#include <mindroid/lang/Class.h>")?;
        fmt!(fmt, "#include <mindroid/lang/Object.h>")?;
        fmt!(fmt, "#include <mindroid/lang/String.h>")?;

        let dependencies = map_dependencies(&c.dependencies(false));
        for d in &dependencies {
            fmt!(fmt, "#include <{}>", d.as_fs_path("h").display())?;
        }
        fmt.newline()?;

        namespace_open(&mut fmt, package)?;
        fmt.newline()?;

        fmt!(fmt, "using mindroid::sp;")?;
        fmt!(fmt, "using mindroid::Class;")?;
        fmt!(fmt, "using mindroid::Object;")?;
        fmt!(fmt, "using mindroid::String;")?;
        fmt.newline()?;

        fmt!(fmt, "class {} : public Object {{", ident)?;
        fmt!(fmt, "public:")?;

        // Declare constants
        for cst in &c.constants {
            let ident = &cst.ident;
            match &cst.type_ {
                Type::Pod(PodType::String) => fmt____!(fmt, "static const sp<String> {};", ident)?,
                Type::Pod(p) => fmt____!(fmt, "static const {} {} = {};", p.cpp(), ident, cst.value)?,
                _ => {
                    let n = &c.name;
                    let t = &cst.type_;
                    unimplemented!("Constant {} of struct {} with type {}", ident, n, t)
                }
            }
        }
        if !c.constants.is_empty() && !c.fields.is_empty() {
            fmt.newline()?;
        }

        if !c.fields.is_empty() {
            fmt.increment();
            // Declare and implement getter and setter
            for f in &c.fields {
                let ident = &f.ident;
                let member = format!("m{}", ident.uppercase());
                let ty = f.type_.cpp();

                // get
                fmt!(fmt, "{} get{}() const {{", ty, ident.uppercase(),)?;
                fmt____!(fmt, "return {};", member)?;
                fmt!(fmt, "}")?;

                // set
                let arg_type = match f.type_ {
                    Type::Pod(_) | Type::Object(Object::Enumeration(_)) => ty,
                    _ => format!("const {}&", ty),
                };
                fmt!(fmt, "void set{}({} {}) {{", ident.uppercase(), arg_type, ident)?;
                fmt____!(fmt, "this->{} = {};", member, ident)?;
                fmt!(fmt, "}")?;

                // Additional setter with const char* interface
                if let Type::Pod(PodType::String) = f.type_ {
                    fmt!(fmt, "void set{}(const char* {}) {{", ident.uppercase(), ident)?;
                    fmt____!(fmt, "this->{} = String::valueOf({});", member, ident)?;
                    fmt!(fmt, "}")?;
                }
                fmt.newline()?;
            }
            fmt.decrement();

            fmt!(fmt, "private:")?;
            // Members
            for f in &c.fields {
                let ty = f.type_.cpp();
                fmt____!(fmt, "{} m{};", ty, f.ident.uppercase())?;
            }
        }
        fmt!(fmt, "};")?;
        fmt.newline()?;

        namespace_close(&mut fmt, package)?;
        fmt.newline()?;
        guard_close(&mut fmt, &name)?;

        fmt.flush().map_err(Into::into)
    }

    fn cpp(c: &Class, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&c.name, out, "cpp")?);
        let name = &c.name;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        fmt!(fmt, "#include <{}>", c.name.as_fs_path("h").display())?;
        fmt.newline()?;

        namespace_open(&mut fmt, package)?;
        fmt.newline()?;

        // Declare constants
        for cst in &c.constants {
            let ident = &cst.ident;
            if let Type::Pod(PodType::String) = cst.type_ {
                let value = &cst.value;
                fmt!(
                    fmt,
                    "const sp<String> {}::{} = String::valueOf({});",
                    name.ident,
                    ident,
                    value
                )?;
            }
        }
        fmt.newline()?;

        namespace_close(&mut fmt, package)?;

        fmt.flush().map_err(Into::into)
    }
}

mod interface {
    use super::{
        binder_name, guard_close, guard_open, map_dependencies, namespace_close, namespace_open, Cpp, Leveled,
        Parcelable,
    };
    use crate::{
        fmt, fmt____,
        generator::method_message_id,
        model::{Argument, Interface, Method, Object, Package, Type},
        utils::{writer, Formatter},
    };
    use failure::Error;
    use itertools::Itertools;
    use std::{io::Write, path::Path};

    pub(super) fn generate(i: &Interface, out: &Path, license: Option<&str>) -> Result<(), Error> {
        header(i, out, license)?;
        cpp(i, out, license)
    }

    /// Generate inderface header
    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    fn header(i: &Interface, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&i.name, out, "h")?);
        let name = &i.name;
        let binder = binder_name(&name).ident.clone();
        let binder_package = Package(vec!["binder".into()]);
        let ident = &name.ident;
        let package = &name.package;

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        guard_open(&mut fmt, &name)?;
        fmt.newline()?;

        let need_void = i.methods.iter().any(|m| m.is_promise() && m.type_.is_none());

        // Includes
        fmt!(fmt, "#include <mindroid/lang/Class.h>")?;
        fmt!(fmt, "#include <mindroid/lang/Object.h>")?;
        fmt!(fmt, "#include <mindroid/lang/String.h>")?;
        // Need Void.h for void async methods
        if need_void {
            fmt!(fmt, "#include <mindroid/lang/Void.h>")?;
        }
        fmt!(fmt, "#include <mindroid/os/Binder.h>")?;
        fmt!(fmt, "#include <mindroid/os/IBinder.h>")?;
        fmt!(fmt, "#include <mindroid/os/IInterface.h>")?;
        fmt!(fmt, "#include <mindroid/os/Parcel.h>")?;
        fmt!(fmt, "#include <mindroid/os/RemoteException.h>")?;
        fmt!(fmt, "#include <mindroid/util/concurrent/Promise.h>")?;

        let dependencies = map_dependencies(&i.dependencies(true));
        for i in &dependencies {
            fmt!(fmt, "#include <{}>", i.as_fs_path("h").display())?;
        }
        fmt.newline()?;

        namespace_open(&mut fmt, package)?;
        fmt.newline()?;

        fmt!(fmt, "using mindroid::sp;")?;
        fmt!(fmt, "using mindroid::Object;")?;
        fmt!(fmt, "using mindroid::String;")?;
        if need_void {
            fmt!(fmt, "using mindroid::Void;")?;
        }
        fmt.newline()?;

        // Interface
        fmt!(fmt, "class {} : public mindroid::IInterface {{", ident)?;
        fmt!(fmt, "public:")?;
        for m in &i.methods {
            let ident = &m.ident;
            let ty = if m.is_promise() {
                if m.type_.is_none() {
                    "sp<mindroid::Promise<sp<Void>>>".into()
                } else {
                    format!("sp<mindroid::Promise<{}>>", m.type_.cpp())
                }
            } else {
                m.type_.cpp()
            };
            let args = m.args.cpp();
            fmt____!(fmt, "virtual {} {}({}) = 0;", ty, ident, args,)?;
        }
        fmt!(fmt, "};")?;
        fmt.newline()?;

        // namespace binder
        namespace_open(&mut fmt, &binder_package)?;
        fmt.newline()?;
        fmt!(fmt, "class {} {{", binder)?;
        fmt!(fmt, "public:")?;
        fmt.increment();
        fmt!(fmt, "class Proxy;")?;
        fmt.newline()?;

        // Stub
        fmt!(fmt, "class Stub : public mindroid::Binder, public {} {{", ident)?;
        fmt!(fmt, "public:")?;
        fmt.increment();
        fmt!(fmt, "Stub() {")?;
        fmt____!(fmt, "attachInterface(this, String::valueOf(DESCRIPTOR));")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "static sp<{}> asInterface(const sp<IBinder>& binder) {{", ident)?;
        fmt.increment();
        fmt!(fmt, "if (binder == nullptr) {")?;
        fmt____!(fmt, "return nullptr;")?;
        fmt!(fmt, "}")?;
        fmt!(fmt, "return new {}::Proxy(binder);", binder)?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "sp<IBinder> asBinder() override {")?;
        fmt____!(fmt, "return this;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "void onTransact(int32_t what, const sp<mindroid::Parcel>& data, const sp<mindroid::Promise<sp<mindroid::Parcel>>>& result) override;")?;
        fmt.newline()?;

        fmt!(fmt, "class Proxy : public {} {{", ident)?;
        fmt!(fmt, "public:")?;
        fmt.increment();
        fmt!(fmt, "Proxy(const sp<IBinder>& remote) {")?;
        fmt____!(fmt, "mRemote = remote;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "sp<IBinder> asBinder() override {")?;
        fmt____!(fmt, "return mRemote;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "bool equals(const sp<Object>& obj) const override {")?;
        fmt.increment();
        fmt!(fmt, "if (obj == nullptr) return false;")?;
        fmt!(fmt, "if (obj == this) return true;")?;
        fmt!(fmt, "if (mindroid::Class<Stub::Proxy>::isInstance(obj)) {")?;
        fmt____!(fmt, "sp<Stub::Proxy> other = mindroid::Class<Stub::Proxy>::cast(obj);")?;
        fmt____!(fmt, "return mRemote->equals(other->mRemote);")?;
        fmt!(fmt, "} else {")?;
        fmt____!(fmt, "return false;")?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "size_t hashCode() const override {")?;
        fmt____!(fmt, "return mRemote->hashCode();")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        for m in &i.methods {
            let ident = &m.ident;
            let ty = if m.is_promise() {
                if m.type_.is_none() {
                    "sp<mindroid::Promise<sp<Void>>>".into()
                } else {
                    format!("sp<mindroid::Promise<{}>>", m.type_.cpp())
                }
            } else {
                m.type_.cpp()
            };
            let args = m.args.cpp();
            fmt!(fmt, "{} {}({}) override;", ty, ident, args,)?;
        }
        fmt.decrement();
        fmt.newline()?;

        fmt!(fmt, "private:")?;
        fmt____!(fmt, "sp<IBinder> mRemote;")?;
        fmt!(fmt, "};")?;
        fmt.newline()?;
        fmt.decrement();

        fmt!(fmt, "private:")?;
        fmt.increment();
        fmt!(fmt, "static const char* const DESCRIPTOR;")?;

        // Message id constants
        for (index, method) in i.methods.iter().enumerate() {
            let id = method_message_id(i, method);
            fmt!(fmt, "static const int32_t {} = {};", id, index + 1)?;
        }
        fmt.newline()?;

        fmt!(fmt, "friend class {}::Proxy;", binder)?;
        fmt.decrement();
        fmt!(fmt, "};")?;
        fmt.newline()?;

        // Proxy
        fmt!(fmt, "class Proxy : public {} {{", ident)?;
        fmt!(fmt, "public:")?;
        fmt.increment();
        fmt!(fmt, "Proxy(const sp<mindroid::IBinder>& binder);")?;
        fmt.newline()?;

        fmt!(fmt, "sp<mindroid::IBinder> asBinder() override {")?;
        fmt____!(fmt, "return mBinder;")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "bool equals(const sp<Object>& obj) const override {")?;
        fmt.increment();
        fmt!(fmt, "if (obj == nullptr) return false;")?;
        fmt!(fmt, "if (obj == this) return true;")?;
        fmt!(fmt, "if (mindroid::Class<Proxy>::isInstance(obj)) {")?;
        fmt____!(fmt, "sp<Proxy> other = mindroid::Class<Proxy>::cast(obj);")?;
        fmt____!(fmt, "return mBinder->equals(other->mBinder);")?;
        fmt!(fmt, "} else {")?;
        fmt____!(fmt, "return false;")?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        fmt!(fmt, "size_t hashCode() const override {")?;
        fmt____!(fmt, "return mBinder->hashCode();")?;
        fmt!(fmt, "}")?;
        fmt.newline()?;

        for m in &i.methods {
            let args = m.args.cpp();
            let ident = &m.ident;
            let ty = if m.is_promise() {
                if m.type_.is_none() {
                    "sp<mindroid::Promise<sp<Void>>>".into()
                } else {
                    format!("sp<mindroid::Promise<{}>>", m.type_.cpp())
                }
            } else {
                m.type_.cpp()
            };
            fmt!(fmt, "{} {}({}) override;", ty, ident, args)?;
        }
        fmt.decrement();
        fmt.newline()?;

        fmt!(fmt, "private:")?;
        fmt____!(fmt, "sp<mindroid::IBinder> mBinder;")?;
        fmt____!(fmt, "sp<{}::Stub> mStub;", binder)?;
        fmt____!(fmt, "sp<{}> mProxy;", ident)?;
        fmt!(fmt, "};")?;
        fmt.decrement();
        fmt!(fmt, "};")?;
        fmt.newline()?;

        namespace_close(&mut fmt, &binder_package)?;
        namespace_close(&mut fmt, package)?;
        fmt.newline()?;

        guard_close(&mut fmt, &name)?;

        fmt.flush().map_err(Into::into)
    }

    fn args_call(args: &[Argument]) -> String {
        args.iter().map(|a| a.ident.as_str()).collect::<Vec<&str>>().join(", ")
    }

    fn args_call_cast(args: &[Argument]) -> String {
        args.iter()
            .map(|a| match &a.type_ {
                Type::Object(Object::Interface(i)) => format!(
                    "{}::binder::{}::Stub::asInterface({}->asBinder())",
                    i.borrow().name.package.as_path().join("::"),
                    binder_name(&i.borrow().name).ident,
                    a.ident
                ),
                _ => a.ident.to_owned(),
            })
            .collect::<Vec<String>>()
            .join(", ")
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    fn proxy_method(interface: &Interface, method: &Method, fmt: &mut Formatter) -> Result<(), Error> {
        let ident = &method.ident;
        let r#async = method.is_promise();
        let name = &interface.name;
        let message_id = method_message_id(interface, method);

        // Method signature
        let ty = if let Some(ref t) = method.type_ {
            if r#async {
                format!("sp<Promise<{}>>", t.cpp())
            } else {
                t.cpp()
            }
        } else if r#async {
            "sp<Promise<sp<Void>>>".into()
        } else {
            "void".into()
        };
        fmt!(
            fmt,
            "{} {}::Stub::Proxy::{}({}) {{",
            ty,
            binder_name(&name).ident,
            ident,
            method.args.cpp(),
        )?;

        fmt.increment();

        if method.type_.is_some() || method.is_promise() {
            if let Some(ref ty) = method.type_ {
                fmt!(fmt, "sp<Promise<{ty}>> _promise = new Promise<{ty}>();", ty = ty.cpp())?;
            } else {
                fmt!(fmt, "sp<Promise<sp<Void>>> _promise = new Promise<sp<Void>>();")?;
            }
        }

        fmt!(fmt, "sp<Parcel> _data = Parcel::obtain();")?;

        let mut first = true;
        for a in &method.args {
            if !first {
                fmt.newline()?;
                first = false;
            }
            a.type_.put(fmt, &"_data", &a.ident, 0)?;
        }

        if method.type_.is_some() || method.is_promise() {
            fmt!(fmt, "mRemote->transact({}, _data, 0)", message_id)?;
            fmt.increment();
            fmt.increment();
            fmt!(
                fmt,
                "->then([=] (const sp<Parcel>& parcel, const sp<Exception>& exception) {"
            )?;
            fmt.increment();
            fmt!(fmt, "if (exception == nullptr) {")?;
            fmt.increment();
            fmt!(fmt, "try {")?;
            fmt.increment();
            if let Some(ref t) = method.type_ {
                t.get(fmt, "parcel", "_reply", 0)?;
                fmt!(fmt, "_promise->complete(_reply);")?;
            } else {
                fmt!(fmt, "_promise->complete(nullptr);")?;
            }
            fmt.decrement();
            fmt!(fmt, "} catch (const RemoteException& e) {")?;
            fmt____!(fmt, "_promise->completeWith(e);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "} else {")?;
            fmt____!(fmt, "_promise->completeWith(exception);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "});")?;
            fmt.decrement();
            fmt.decrement();
            if r#async {
                fmt!(fmt, "return _promise;")?;
            } else {
                fmt!(fmt, "return Binder::get(_promise);")?;
            }
        } else if method.is_oneway() {
            fmt!(fmt, "mRemote->transact({}, _data, FLAG_ONEWAY);", message_id)?;
        } else {
            fmt!(fmt, "Binder::get(mRemote->transact({}, _data, 0));", message_id)?;
        }
        fmt.decrement();
        fmt!(fmt, "}")?;

        Ok(())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    pub fn on_transact(interface: &Interface, method: &Method, fmt: &mut Formatter) -> Result<(), Error> {
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
                a.type_.get(fmt, &"data", &a.ident, 1)?;
            }
            Ok(())
        };

        fmt!(fmt, "case {}: {{", message_id)?;
        fmt.increment();
        if method.is_promise() {
            let ty = if let Some(ty) = &method.type_ {
                ty.cpp()
            } else {
                "sp<Void>".into()
            };
            get_args(fmt)?;
            fmt!(fmt, "sp<Promise<{}>> _reply = {}({});", ty, ident, args)?;
            fmt!(
                fmt,
                "_reply->then([=] (const {}& value, const sp<Exception>& exception) {{",
                ty
            )?;
            fmt.increment();
            fmt!(fmt, "if (exception == nullptr) {")?;
            fmt.increment();
            fmt!(fmt, "sp<Parcel> _parcel = Parcel::obtain();")?;
            fmt!(fmt, "try {")?;
            fmt.increment();
            if let Some(ty) = &method.type_ {
                ty.put(fmt, &"_parcel", "value", 0)?;
            }
            fmt!(fmt, "result->complete(_parcel);")?;
            fmt.decrement();
            fmt!(fmt, "} catch (const RemoteException& e) {")?;
            fmt____!(fmt, "result->completeWith(e);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "} else {")?;
            fmt____!(fmt, "result->completeWith(exception);")?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "});")?;
        } else if let Some(ty) = &method.type_ {
            get_args(fmt)?;
            fmt!(fmt, "{} _reply = {}({});", ty.cpp(), ident, args)?;
            fmt!(fmt, "sp<Parcel> _parcel = Parcel::obtain();")?;
            ty.put(fmt, &"_parcel", "_reply", 0)?;
            fmt!(fmt, "result->complete(_parcel);")?;
        } else if method.is_oneway() {
            get_args(fmt)?;
            fmt!(fmt, "{}({});", ident, args)?;
        } else {
            get_args(fmt)?;
            fmt!(fmt, "{}({});", ident, args)?;
            fmt!(fmt, "result->complete(Parcel::obtain());")?;
        }
        fmt!(fmt, "break;")?;
        fmt.decrement();
        fmt!(fmt, "}")?;

        Ok(())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
    fn cpp(i: &Interface, out: &Path, license: Option<&str>) -> Result<(), Error> {
        let mut fmt = Formatter::new(writer(&i.name, out, "cpp")?);
        let name = &i.name;
        let ident = &name.ident;
        let package = &name.package;
        let binder = binder_name(&name).ident.clone();
        let binder_package = Package(vec!["binder".into()]);

        if let Some(lic) = license {
            fmt!(fmt, lic)?;
            fmt.newline()?;
        }

        fmt!(fmt, "#include <mindroid/os/Parcel.h>")?;
        fmt!(fmt, "#include <mindroid/net/URI.h>")?;
        fmt!(fmt, "#include <mindroid/runtime/system/Runtime.h>")?;
        fmt!(fmt, "#include <mindroid/util/concurrent/Promise.h>")?;
        fmt!(fmt, "#include <{}>", name.as_fs_path("h").display())?;
        fmt.newline()?;

        fmt!(fmt, "using namespace mindroid;")?;
        fmt.newline()?;

        namespace_open(&mut fmt, package)?;
        namespace_open(&mut fmt, &binder_package)?;
        fmt.newline()?;

        let descriptor = format!("mindroid://interfaces/{}/{}", package.as_path().join("/"), ident);
        fmt!(
            fmt,
            "const char* const {}::Stub::DESCRIPTOR = \"{}\";",
            binder,
            descriptor
        )?;
        fmt.newline()?;

        fmt!(fmt,
            "void {binder}::Stub::onTransact(int32_t what, const sp<Parcel>& data, const sp<Promise<sp<Parcel>>>& result) {{",
            binder = binder
        )?;
        fmt.increment();
        fmt!(fmt, "switch (what) {")?;
        for m in &i.methods {
            on_transact(i, m, &mut fmt)?
        }
        fmt!(fmt, "default:")?;
        fmt____!(fmt, "Binder::onTransact(what, data, result);")?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        for m in &i.methods {
            proxy_method(i, m, &mut fmt)?;
            fmt.newline()?;
        }

        fmt!(fmt, "{}::Proxy::Proxy(const sp<IBinder>& binder) {{", binder)?;
        fmt.increment();
        fmt!(fmt, "mBinder = binder;")?;
        fmt!(fmt, "if (binder->getUri()->getScheme()->equals(\"mindroid\")) {")?;
        fmt____!(
            fmt,
            "mStub = object_cast<{b}::Stub>(binder->queryLocalInterface({b}::Stub::DESCRIPTOR));",
            b = binder
        )?;
        fmt____!(fmt, "mProxy = new {}::Stub::Proxy(binder);", binder)?;
        fmt!(fmt, "} else {")?;
        fmt____!(fmt, "sp<Runtime> runtime = Runtime::getRuntime();")?;
        fmt____!(
            fmt,
            "mStub = object_cast<{}::Stub>(runtime->getBinder(binder->getId()));",
            binder
        )?;
        fmt____!(fmt, "mProxy = object_cast<{}>(runtime->getProxy(binder));", ident)?;
        fmt!(fmt, "}")?;
        fmt.decrement();
        fmt!(fmt, "}")?;
        fmt.newline()?;

        // Proxy methods
        for m in &i.methods {
            let ident = &m.ident;
            let ty = m.type_.cpp();
            let args = m.args.cpp();
            if m.is_promise() {
                // Handle async void case
                let ty = if m.type_.is_some() { ty } else { "sp<Void>".into() };
                fmt!(fmt, "sp<Promise<{}>> {}::Proxy::{}({}) {{", ty, binder, ident, args)?;
            } else {
                fmt!(fmt, "{} {}::Proxy::{}({}) {{", ty, binder, ident, args)?;
            }
            fmt.increment();
            fmt!(fmt, "if (mStub != nullptr && mStub->isCurrentThread()) {")?;
            let return_ = if m.type_.is_some() || m.is_promise() {
                "return "
            } else {
                ""
            };
            let args = args_call_cast(&m.args);
            fmt____!(fmt, "{}mStub->{}({});", return_, ident, args)?;
            fmt!(fmt, "} else {")?;
            let args = args_call(&m.args);
            fmt____!(fmt, "{}mProxy->{}({});", return_, ident, args)?;
            fmt!(fmt, "}")?;
            fmt.decrement();
            fmt!(fmt, "}")?;
            fmt.newline()?;
        }

        namespace_close(&mut fmt, &binder_package)?;
        namespace_close(&mut fmt, package)?;

        fmt.flush().map_err(Into::into)
    }
}

trait Cpp {
    fn cpp(&self) -> String;
}

impl Cpp for Class {
    fn cpp(&self) -> String {
        format!("sp<{}::{}>", self.name.package.as_path().join("::"), self.name.ident)
    }
}

impl Cpp for Interface {
    fn cpp(&self) -> String {
        format!("sp<{}::{}>", self.name.package.as_path().join("::"), self.name.ident)
    }
}

impl Cpp for Enumeration {
    fn cpp(&self) -> String {
        format!("{}::{}", self.name.package.as_path().join("::"), self.name.ident)
    }
}

/// Generate argument list string for signature use
impl Cpp for Vec<Argument> {
    fn cpp(&self) -> String {
        self.iter().map(Cpp::cpp).collect::<Vec<String>>().join(", ")
    }
}

impl Cpp for Argument {
    fn cpp(&self) -> String {
        let t = match &self.type_ {
            Type::Collection(c) => format!("const {}&", c.cpp()),
            Type::Object(o) => match o {
                Object::Enumeration(e) => e.borrow().cpp(),
                _ => format!("const {}&", o.cpp()),
            },
            Type::Pod(pod) => match pod {
                PodType::String => "const sp<String>&".to_owned(),
                _ => pod.cpp(),
            },
        };

        format!("{} {}", t, self.ident)
    }
}

impl Cpp for PodType {
    fn cpp(&self) -> String {
        match self {
            PodType::Bool => "bool",
            PodType::U8 => "uint8_t",
            PodType::U16 => "uint16_t",
            PodType::U32 => "uint32_t",
            PodType::U64 => "uint64_t",
            PodType::I8 => "int8_t",
            PodType::I16 => "int16_t",
            PodType::I32 => "int32_t",
            PodType::I64 => "int64_t",
            PodType::F32 => "float",
            PodType::F64 => "double",
            PodType::String => "sp<String>",
        }
        .into()
    }
}

impl Cpp for Type {
    fn cpp(&self) -> String {
        match self {
            Type::Collection(ref c) => c.cpp(),
            Type::Object(ref o) => o.cpp(),
            Type::Pod(ref p) => p.cpp(),
        }
    }
}

impl Cpp for Collection {
    fn cpp(&self) -> String {
        match self {
            Collection::Array(ref c) | Collection::List(ref c) => format!("sp<mindroid::ArrayList<{}>>", c.cpp()),
            Collection::Set(ref p) => format!("sp<mindroid::HashSet<{}>>", p.cpp()),
            Collection::Map(ref k, ref v) => format!("sp<mindroid::HashMap<{}, {}>>", k.cpp(), v.cpp()),
        }
    }
}

impl<T> Cpp for Option<T>
where
    T: Cpp,
{
    fn cpp(&self) -> String {
        if let Some(ref t) = self {
            t.cpp()
        } else {
            "void".into()
        }
    }
}

impl Cpp for Object {
    fn cpp(&self) -> String {
        match self {
            Object::Class(c) => c.borrow().cpp(),
            Object::Enumeration(e) => e.borrow().cpp(),
            Object::Interface(i) => i.borrow().cpp(),
            Object::Unresolved(u) => panic!("Encountered unresolved type {} during generation", u),
        }
    }
}

/// Implementors are able to be parceled
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
            PodType::U64 => "putLong", // TODO: enabled for testing fibex
            PodType::I8 => "putByte",
            PodType::I16 => "putShort",
            PodType::I32 => "putInt",
            PodType::I64 => "putLong",
            PodType::F32 => "putFloat",
            PodType::F64 => "putDouble",
            PodType::String => "putString",
        };
        fmt!(fmt, "{}->{}({});", parcel, setter, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        let getter = match self {
            PodType::Bool => "getBoolean",
            PodType::U8 => "getShort",
            PodType::U16 => "getInt",
            PodType::U32 => "getLong",
            PodType::U64 => "getLong", // TODO: enabled for testing fibex
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
            "{} {} = {}->{}();",
            self.cpp(),
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
        fmt!(fmt, "{}->putInt((int32_t) {});", parcel, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        fmt!(
            fmt,
            "{} {} = ({}) {}->getInt();",
            self.cpp(),
            ident.leveled(level),
            self.cpp(),
            parcel
        )
    }
}

impl Parcelable for Interface {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, _: usize) -> Result<(), Error> {
        fmt!(fmt, "{}->putBinder({}->asBinder());", parcel, ident)
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        fmt!(
            fmt,
            "{} {} = {}::binder::{}::Stub::asInterface({}->getBinder());",
            self.cpp(),
            ident.leveled(level),
            self.name.package.as_path().join("::"),
            binder_name(&self.name).ident,
            parcel
        )
    }
}

impl Parcelable for Class {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        for f in &self.fields {
            let var = format!("{}{}", ident, f.ident.uppercase());
            fmt!(
                fmt,
                "auto {inst}{ident} = {inst}->get{ident}();",
                inst = ident,
                ident = f.ident.uppercase()
            )?;
            f.type_.put(fmt, parcel, &var, level)?;
        }
        Ok(())
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        fmt!(
            fmt,
            "{} {} = new {}();",
            self.cpp(),
            ident.leveled(level),
            format!("{}::{}", self.name.package.as_path().join("::"), self.name.ident)
        )?;

        for f in &self.fields {
            let var = format!("{}{}", ident, f.ident.uppercase());
            f.type_.get(fmt, parcel, &var, level)?;
            fmt!(
                fmt,
                "{}->set{}({});",
                ident.leveled(level),
                f.ident.uppercase(),
                format!("{}{}", ident, f.ident.uppercase()).leveled(level),
            )?;
        }
        Ok(())
    }
}

impl Parcelable for Collection {
    fn put(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Collection::Array(ref t) | Collection::List(ref t) | Collection::Set(ref t) => {
                fmt!(fmt, "{}->putInt({}->size());", parcel, ident)?;
                let iter = format!("{}Iterator", ident).leveled(level);
                fmt!(fmt, "auto {} = {}->iterator();", iter, ident)?;
                fmt!(fmt, "while ({}.hasNext()) {{", iter)?;
                fmt.increment();
                fmt!(fmt, "auto entry = {}.next();", iter)?;
                t.put(fmt, parcel, "entry", level + 1)?;
                fmt.decrement();
                fmt!(fmt, "}")
            }
            Collection::Map(key, value) => {
                let iter = format!("{}Iterator", ident).leveled(level);
                // length
                fmt!(fmt, "{}->putInt({}->size());", parcel, ident)?;

                fmt!(fmt, "auto {} = {}->iterator();", iter, ident)?;
                fmt!(fmt, "while ({}.hasNext()) {{", iter)?;
                fmt.increment();

                fmt!(fmt, "auto entry = {}.next();", iter)?;
                // key
                let key_ident = "key".leveled(level + 1);
                fmt!(fmt, "auto {} = entry.getKey();", key_ident)?;
                key.put(fmt, parcel, &key_ident, level + 1)?;

                // value
                let value_ident = "value".leveled(level + 1);
                fmt!(fmt, "auto {} = entry.getValue();", value_ident)?;
                value.put(fmt, parcel, &value_ident, level + 1)?;

                fmt.decrement();
                fmt!(fmt, "}")
            }
        }
    }

    fn get(&self, fmt: &mut Formatter, parcel: &str, ident: &str, level: usize) -> Result<(), Error> {
        match self {
            Collection::Array(ref c) | Collection::List(ref c) | Collection::Set(ref c) => {
                // length
                let size_var = format!("{}Size", ident);
                fmt!(fmt, "const size_t {} = {}->getInt();", size_var, parcel)?;
                let ident = ident.leveled(level);

                let i = "i".leveled(level);

                // instance

                match self {
                    Collection::Array(ref c) | Collection::List(ref c) => fmt!(
                        fmt,
                        "{} {} = new mindroid::ArrayList<{}>({});",
                        self.cpp(),
                        ident,
                        c.cpp(),
                        size_var
                    ),
                    Collection::Set(ref p) => {
                        fmt!(fmt, "{} {} = new mindroid::HashSet<{}>();", self.cpp(), ident, p.cpp())
                    }
                    _ => unreachable!(),
                }?;

                fmt!(
                    fmt,
                    "for (size_t {i} = 0; {i} < {size_var}; {i}++) {{",
                    i = i,
                    size_var = size_var
                )?;
                fmt.increment();
                c.get(fmt, parcel, "value", level + 1)?;
                fmt!(fmt, "{}->add({});", ident, "value".leveled(level + 1))?;
                fmt.decrement();
                fmt!(fmt, "}")
            }
            Collection::Map(ref key, ref value) => {
                // length
                let size_var = format!("{}Size", ident);
                fmt!(fmt, "const size_t {} = {}->getInt();", size_var, parcel)?;
                let ident = ident.leveled(level);

                // instance
                match self {
                    Collection::Map(ref k, ref v) => fmt!(
                        fmt,
                        "{} {} = new mindroid::HashMap<{}, {}>();",
                        self.cpp(),
                        ident,
                        k.cpp(),
                        v.cpp()
                    )?,
                    _ => unimplemented!(),
                }

                let i = "i".leveled(level);
                fmt!(
                    fmt,
                    "for (size_t {i} = 0; {i} < {size_var}; {i}++) {{",
                    i = i,
                    size_var = size_var
                )?;

                fmt.increment();
                key.get(fmt, parcel, "key", level + 1)?;
                value.get(fmt, parcel, "value", level + 1)?;

                let key_ident = "key".leveled(level + 1);
                let value_ident = "value".leveled(level + 1);
                fmt!(fmt, "{}->put({}, {});", ident, key_ident, value_ident)?;

                fmt.decrement();
                fmt!(fmt, "}")
            }
        }
    }
}

fn namespace_open(fmt: &mut Formatter, p: &Package) -> Result<(), Error> {
    for p in p.as_path() {
        fmt!(fmt, "namespace {} {{", p)?;
    }
    Ok(())
}

fn namespace_close(fmt: &mut Formatter, p: &Package) -> Result<(), Error> {
    for p in p.as_path().iter().rev() {
        fmt!(fmt, "}} /* namespace {} */", p)?;
    }
    Ok(())
}

fn guard(n: &Name) -> String {
    let mut guard = n.package.as_path().clone();
    guard.push(n.ident.clone());
    format!("{}_H", guard.join("_").to_uppercase(),)
}

fn guard_open(fmt: &mut Formatter, n: &Name) -> Result<(), Error> {
    fmt!(fmt, "#ifndef {}", guard(n))?;
    fmt!(fmt, "#define {}", guard(n))?;
    Ok(())
}

fn guard_close(fmt: &mut Formatter, n: &Name) -> Result<(), Error> {
    fmt!(fmt, "#endif /* {} */", guard(n))?;
    Ok(())
}

fn binder_name(n: &Name) -> Name {
    let ident = if n.ident.starts_with('I') {
        n.ident[1..].to_owned()
    } else {
        format!("{}Impl", n.ident)
    };
    Name::new(n.package.clone(), ident)
}

// Map a vector of dependencies into Names
fn map_dependencies(dependencies: &HashSet<Dependency>) -> BTreeSet<Name> {
    dependencies
        .iter()
        .filter(|d| d != &&Dependency::Optional) //TODO remove once optional available!
        .map(|d| match d {
            Dependency::Array | Dependency::List => "mindroid.util.ArrayList".into(),
            Dependency::Map => "mindroid.util.HashMap".into(),
            Dependency::Set => "mindroid.util.HashSet".into(),
            Dependency::Optional => unimplemented!(), //TODO "mindroid.util.Optional".into(),
            Dependency::Name(n) => n.clone(),
        })
        .collect()
}
