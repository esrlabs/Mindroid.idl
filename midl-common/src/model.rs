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

use failure::{format_err, Error, Fail};
use itertools::Itertools;
use serde_derive::{Deserialize, Serialize};
use std::{
    borrow::ToOwned,
    cell::RefCell,
    cmp::{Ord, Ordering},
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter},
    path::PathBuf,
    rc::Rc,
    str::FromStr,
    string::ToString,
};

pub const PROMISE_ANNOTATION: &str = "Promise";
pub const OPTIONAL_ANNOTATION: &str = "Optional";
pub const IMPORT_ANNOTATION: &str = "Import";
pub const ONEWAY_ANNOTATION: &str = "Oneway";

#[derive(Debug, Fail)]
pub enum ModelError {
    #[fail(display = "Name conflict of {}", name)]
    NameConflict { name: String },

    #[fail(display = "Redefined class field name in {}", name)]
    DuplicateClassField { name: String },

    #[fail(display = "Duplicate method {} in {}", method_name, name)]
    DuplicateMethod { name: String, method_name: String },

    #[fail(display = "Cannot resolve {} used in {}", name, outer)]
    Unresolved { outer: String, name: String },

    #[fail(
        display = "Oneway methods cannot have a return type other than void: {}: {}",
        outer, name
    )]
    OnewayMethodReturnType { outer: String, name: String },
}

/// Identifier
pub type Ident = String;

/// Name
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct Name {
    pub package: Package,
    pub ident: Ident,
}

impl Name {
    pub fn new(package: Package, ident: Ident) -> Name {
        Name { package, ident }
    }

    pub fn as_fs_path(&self, ext: &str) -> PathBuf {
        let path: PathBuf = self.package.clone().into();
        path.join(self.ident.clone()).with_extension(ext)
    }

    pub fn as_path(&self) -> Vec<String> {
        let mut p = self.package.as_path().clone();
        p.push(self.ident.clone());
        p
    }
}

impl<'a, T: AsRef<str>> From<T> for Name {
    fn from(s: T) -> Name {
        let s = s.as_ref();
        if let Some(n) = s.rfind('.') {
            let p = Package(s[..n].split('.').map(ToOwned::to_owned).collect());
            Name {
                package: p,
                ident: s[n + 1..].into(),
            }
        } else {
            Name {
                package: Package::default(),
                ident: s.into(),
            }
        }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        if self.package.is_empty() {
            f.write_str(&self.ident)
        } else {
            write!(f, "{}.{}", self.package, self.ident)
        }
    }
}

/// A dependency of a class or interface
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dependency {
    Array,
    List,
    Map,
    Name(Name),
    Set,
    Optional,
}

impl From<&Rc<Collection>> for Dependency {
    fn from(c: &Rc<Collection>) -> Self {
        match **c {
            Collection::Array(_) => Dependency::Array,
            Collection::List(_) => Dependency::List,
            Collection::Map(_, _) => Dependency::Map,
            Collection::Set(_) => Dependency::Set,
        }
    }
}

impl Display for Dependency {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        let s = match self {
            Dependency::Array => "Array".to_string(),
            Dependency::List => "List".to_string(),
            Dependency::Map => "Map".to_string(),
            Dependency::Set => "Set".to_string(),
            Dependency::Optional => "Optional".to_string(),
            Dependency::Name(n) => n.to_string(),
        };
        write!(f, "{}", s)
    }
}

/// MetaInfo
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct MetaInfo {
    pub source: Option<PathBuf>,
}

impl MetaInfo {
    pub fn new(source: Option<PathBuf>) -> MetaInfo {
        MetaInfo { source }
    }
}

impl Display for MetaInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        if let Some(ref path) = self.source {
            write!(f, "{}", path.display())
        } else {
            write!(f, "na")
        }
    }
}

impl PartialEq for MetaInfo {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

/// Package
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct Package(pub Vec<String>);

impl Package {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn as_path(&self) -> &Vec<String> {
        &self.0
    }

    pub fn push_front(&mut self, p: &str) {
        self.0.insert(0, p.into())
    }
}

impl Into<PathBuf> for Package {
    fn into(self) -> PathBuf {
        PathBuf::from(self.as_path().join("/"))
    }
}

impl Display for Package {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        write!(f, "{}", self.0.join("."))
    }
}

pub type AnnotationKey = String;

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum AnnotationItem {
    Group(AnnotationGroup),
    Numeric(i64),
    String(String),
}

impl Display for AnnotationItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&match self {
            AnnotationItem::Group(ref g) => g.to_string(),
            AnnotationItem::Numeric(ref n) => n.to_string(),
            AnnotationItem::String(s) => s.clone(),
        })
    }
}

impl From<String> for AnnotationItem {
    fn from(s: String) -> Self {
        AnnotationItem::String(s)
    }
}

impl From<i64> for AnnotationItem {
    fn from(i: i64) -> Self {
        AnnotationItem::Numeric(i)
    }
}

impl From<AnnotationGroup> for AnnotationItem {
    fn from(g: AnnotationGroup) -> Self {
        AnnotationItem::Group(g)
    }
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Annotations {
    pub groups: HashMap<AnnotationKey, AnnotationGroup>,
}

impl Annotations {
    pub fn new(groups: HashMap<AnnotationKey, AnnotationGroup>) -> Annotations {
        Annotations { groups }
    }

    pub fn with_empty_group(group_key: AnnotationKey) -> Annotations {
        let mut groups = HashMap::new();
        groups.insert(group_key.clone(), AnnotationGroup::new(group_key));
        Annotations { groups }
    }

    pub fn group(&self, group: &str) -> Result<&AnnotationGroup, Error> {
        self.groups
            .get(group)
            .ok_or_else(|| format_err!("Cannot find group {:?}", group))
    }

    pub fn push(&mut self, group: AnnotationGroup) {
        self.groups.insert(group.name.clone(), group);
    }

    pub fn contains(&self, group: &str) -> bool {
        self.groups.contains_key(group)
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct AnnotationGroup {
    pub name: AnnotationKey,
    pub items: HashMap<AnnotationKey, Option<AnnotationItem>>,
}

impl AnnotationGroup {
    pub fn new(name: AnnotationKey) -> AnnotationGroup {
        AnnotationGroup {
            name,
            items: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn contains(&self, k: &str) -> bool {
        self.items.contains_key(k)
    }

    pub fn find_numeric(&self, k: &str) -> Result<i64, Error> {
        self.items
            .get(k)
            .ok_or_else(|| format_err!("Cannot find key {:?}", k))
            .and_then(|v| match v {
                Some(AnnotationItem::Numeric(n)) => Ok(*n),
                _ => Err(format_err!(
                    "Key {:?} in group {:?} is not a numeric annotation",
                    self.name,
                    k
                )),
            })
    }

    pub fn find_string(&self, k: &str) -> Result<String, Error> {
        self.items
            .get(k)
            .ok_or_else(|| format_err!("Cannot find key {:?}", k))
            .and_then(|v| match v {
                Some(AnnotationItem::String(s)) => Ok(s.to_string()),
                _ => Err(format_err!(
                    "Key {:?} in group {:?} is not a string annotation",
                    self.name,
                    k
                )),
            })
    }

    pub fn find_group(&self, k: &str) -> Result<&AnnotationGroup, Error> {
        self.items
            .get(k)
            .ok_or_else(|| format_err!("Cannot find key {:?}", k))
            .and_then(|v| match v {
                Some(AnnotationItem::Group(g)) => Ok(g),
                _ => Err(format_err!(
                    "Key {:?} in group {:?} is not a annotation group",
                    self.name,
                    k
                )),
            })
    }

    pub fn insert(&mut self, k: &str, v: Option<AnnotationItem>) {
        self.items.insert(k.into(), v);
    }
}

impl Display for AnnotationGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        let s = if self.items.is_empty() {
            format!("@{}", self.name)
        } else {
            let values = self
                .items
                .keys()
                .sorted()
                .iter()
                .filter_map(|k| self.items.get(*k).map(|v| (k.as_str(), v.clone())))
                .map(|(k, v)| match v {
                    None => k.to_string(),
                    Some(AnnotationItem::Group(g)) => g.to_string(),
                    _ => {
                        let v = match v {
                            None => "".into(),
                            Some(v) => format!("={}", v),
                        };
                        format!("{}{}", k, v)
                    }
                })
                .join(", ");
            format!("@{}({})", self.name, values)
        };
        f.write_str(&s)
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Enumeration {
    pub name: Name,
    pub items: Vec<EnumerationItem>,
    pub annotations: Annotations,
    pub meta: MetaInfo,
}

impl Enumeration {
    pub fn new(name: Name, items: Vec<EnumerationItem>, annotations: Annotations, meta: MetaInfo) -> Enumeration {
        Enumeration {
            name,
            items,
            annotations,
            meta,
        }
    }
}

impl PartialOrd for Enumeration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.name.to_string().cmp(&other.name.to_string()))
    }
}

impl Display for Enumeration {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&self.name.to_string())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct EnumerationItem {
    pub ident: Ident,
    pub annotations: Annotations,
}

impl EnumerationItem {
    pub fn new(ident: Ident, annotations: Annotations) -> EnumerationItem {
        EnumerationItem { ident, annotations }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn annotations(&self) -> &Annotations {
        &self.annotations
    }
}

/// POD types
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum PodType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    String,
}

impl FromStr for PodType {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let r = match s {
            "bool" | "boolean" => PodType::Bool,
            "u8" | "char" => PodType::U8,
            "u16" => PodType::U16,
            "u32" => PodType::U32,
            "u64" => PodType::U64,
            "i8" | "byte" => PodType::I8,
            "i16" | "short" => PodType::I16,
            "i32" | "int" => PodType::I32,
            "i64" | "long" => PodType::I64,
            "f32" | "float" => PodType::F32,
            "f64" | "double" => PodType::F64,
            "string" | "String" => PodType::String,
            _ => return Err(format_err!("Failed to parse pod from {}", s)),
        };
        Ok(r)
    }
}

impl Display for PodType {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(match self {
            PodType::Bool => "bool",
            PodType::U8 => "u8",
            PodType::U16 => "u16",
            PodType::U32 => "u32",
            PodType::U64 => "u64",
            PodType::I8 => "i8",
            PodType::I16 => "i16",
            PodType::I32 => "i32",
            PodType::I64 => "i64",
            PodType::F32 => "f32",
            PodType::F64 => "f64",
            PodType::String => "string",
        })
    }
}

/// Type for objects
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum Object {
    Class(Rc<RefCell<Class>>),
    Enumeration(Rc<RefCell<Enumeration>>),
    Interface(Rc<RefCell<Interface>>),
    Unresolved(Name),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&match self {
            Object::Class(ref i) => i.borrow().name.to_string(),
            Object::Enumeration(i) => i.borrow().name.to_string(),
            Object::Interface(ref i) => i.borrow().name.to_string(),
            Object::Unresolved(ref i) => i.to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum Collection {
    Array(Type),
    List(Type),
    Map(PodType, Type),
    Set(Type),
}

impl Collection {
    pub fn new_array(t: Type) -> Type {
        Type::Collection(Rc::new(Collection::Array(t)))
    }

    pub fn new_list(t: Type) -> Type {
        Type::Collection(Rc::new(Collection::List(t)))
    }

    pub fn new_map(k: PodType, t: Type) -> Type {
        Type::Collection(Rc::new(Collection::Map(k, t)))
    }

    pub fn new_set(t: Type) -> Type {
        Type::Collection(Rc::new(Collection::Set(t)))
    }
}

impl Display for Collection {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&match self {
            Collection::Array(ref i) => format!("{}[]", i),
            Collection::List(ref v) => format!("List<{}>", v),
            Collection::Map(ref k, ref v) => format!("Map<{},{}>", k, v),
            Collection::Set(ref i) => format!("Set<{}>", i),
        })
    }
}

/// Type
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum Type {
    Collection(Rc<Collection>),
    Object(Object),
    Pod(PodType),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&match self {
            Type::Collection(ref c) => c.to_string(),
            Type::Object(Object::Class(ref c)) => c.borrow().name.to_string(),
            Type::Object(Object::Enumeration(e)) => e.borrow().name.to_string(),
            Type::Object(Object::Interface(i)) => i.borrow().name.to_string(),
            Type::Object(Object::Unresolved(ref u)) => u.to_string(),
            Type::Pod(ref p) => p.to_string(),
        })
    }
}

/// Field within a structure
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Field {
    pub annotations: Annotations,
    pub ident: Ident,
    pub type_: Type,
}

impl Field {
    pub fn new(annotations: Annotations, type_: Type, ident: Ident) -> Field {
        Field {
            annotations,
            ident,
            type_,
        }
    }

    pub fn is_optional(&self) -> bool {
        self.annotations.contains(OPTIONAL_ANNOTATION)
    }
}

impl PartialEq for Field {
    fn eq(&self, other: &Field) -> bool {
        self.ident == other.ident && self.type_ == other.type_
    }
}

/// Constant defined in a field
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Constant {
    pub ident: Ident,
    pub type_: Type,
    pub value: String,
}

impl Constant {
    pub fn new(type_: Type, ident: Ident, value: String) -> Constant {
        Constant { ident, type_, value }
    }
}

/// Class
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Class {
    pub name: Name,
    pub fields: Vec<Field>,
    pub constants: Vec<Constant>,
    pub annotations: Annotations,
    pub meta: MetaInfo,
}

impl<'a> Class {
    /// Construct a new class
    pub fn new(
        name: Name,
        fields: Vec<Field>,
        constants: Vec<Constant>,
        annotations: Annotations,
        meta: MetaInfo,
    ) -> Class {
        Class {
            name,
            fields,
            constants,
            annotations,
            meta,
        }
    }

    /// List of all named objects that this class depends on
    pub fn dependencies(&self, recursive: bool) -> HashSet<Dependency> {
        let types = self.fields.iter().map(|f| &f.type_).collect::<Vec<&Type>>();
        let mut deps = dependencies(&types, recursive);
        if self.fields.iter().any(Field::is_optional) {
            deps.insert(Dependency::Optional);
        }
        deps
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Class) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

/// Method argument
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Argument {
    pub annotations: Annotations,
    pub ident: Ident,
    pub type_: Type,
}

impl Argument {
    pub fn new(annotations: Annotations, type_: Type, ident: Ident) -> Argument {
        Argument {
            annotations,
            ident,
            type_,
        }
    }

    pub fn is_optional(&self) -> bool {
        self.annotations.contains(OPTIONAL_ANNOTATION)
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&format!("{}: {}", self.ident, self.type_))
    }
}

/// Method
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Method {
    pub ident: Ident,
    pub arguments: Vec<Argument>,
    pub type_: Option<Type>,
    pub annotations: Annotations,
}

impl Method {
    pub fn new(ident: Ident, arguments: Vec<Argument>, type_: Option<Type>, annotations: Annotations) -> Method {
        Method {
            ident,
            arguments,
            type_,
            annotations,
        }
    }

    pub fn is_oneway(&self) -> bool {
        self.annotations.contains(ONEWAY_ANNOTATION)
    }

    pub fn is_promise(&self) -> bool {
        self.annotations.contains(PROMISE_ANNOTATION)
    }

    pub fn is_optional(&self) -> bool {
        self.annotations.contains(OPTIONAL_ANNOTATION)
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        write!(
            f,
            "{} {}({})",
            if let Some(ref t) = self.type_ {
                t.to_string()
            } else {
                "void".to_string()
            },
            self.ident,
            self.arguments
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(",")
        )
    }
}

/// Interface
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Interface {
    pub name: Name,
    pub methods: Vec<Method>,
    pub annotations: Annotations,
    pub meta: MetaInfo,
}

/// Construct a new interface
impl<'a> Interface {
    pub fn new(name: Name, methods: Vec<Method>, annotations: Annotations, meta: MetaInfo) -> Interface {
        Interface {
            name,
            methods,
            annotations,
            meta,
        }
    }

    /// List of named objects this interface depends on
    pub fn dependencies(&self, recursive: bool) -> HashSet<Dependency> {
        let return_types = self.methods.iter().filter_map(|m| m.type_.as_ref());
        let arguments = self.methods.iter().flat_map(|m| &m.arguments).map(|a| &a.type_);
        let types = return_types.chain(arguments).collect::<Vec<&Type>>();
        let mut deps = dependencies(&types, recursive);
        if self.methods.iter().any(Method::is_optional) {
            deps.insert(Dependency::Optional);
        } else {
            let optional_arg = self
                .methods
                .iter()
                .flat_map(|m| m.arguments.iter())
                .any(Argument::is_optional);
            if optional_arg {
                deps.insert(Dependency::Optional);
            }
        }
        deps
    }
}

impl PartialOrd for Interface {
    fn partial_cmp(&self, other: &Interface) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Model {
    pub objects: HashMap<Name, Object>,
}

impl<'a> Model {
    fn duplicate_name_check(&self, n: &Name) -> Result<(), Error> {
        if self.objects.contains_key(n) {
            Err(ModelError::NameConflict { name: n.to_string() }.into())
        } else {
            Ok(())
        }
    }

    pub fn push_enumeration(&mut self, e: Enumeration) -> Result<(), Error> {
        self.duplicate_name_check(&e.name)?;
        let name = e.name.clone();
        self.objects.insert(name, Object::Enumeration(Rc::new(RefCell::new(e))));
        Ok(())
    }

    pub fn push_class(&mut self, s: Class) -> Result<Rc<RefCell<Class>>, Error> {
        self.duplicate_name_check(&s.name)?;

        // Check for unique field names
        if s.fields.iter().map(|f| &f.ident).collect::<HashSet<&String>>().len() != s.fields.len() {
            return Err(ModelError::DuplicateClassField {
                name: s.name.to_string(),
            }
            .into());
        }

        let name = s.name.clone();
        let class_ref = Rc::new(RefCell::new(s));
        self.objects.insert(name, Object::Class(class_ref.clone()));
        Ok(class_ref)
    }

    pub fn push_interface(&mut self, i: Interface) -> Result<(), Error> {
        self.duplicate_name_check(&i.name)?;

        // Check for unique method signatures despite return type
        {
            let method_to_string = |f: &Method| {
                format!(
                    "{}({})",
                    f.ident,
                    f.arguments.iter().map(ToString::to_string).join(", ")
                )
            };
            let unique_methods = i.methods.iter().map(method_to_string).collect::<HashSet<String>>();
            if unique_methods.len() != i.methods.len() {
                return Err(ModelError::DuplicateMethod {
                    method_name: unique_methods
                        .iter()
                        .find(|&method| i.methods.iter().filter(|m| &method_to_string(m) == method).count() > 1)
                        .map(ToString::to_string)
                        .unwrap(),
                    name: i.name.to_string(),
                }
                .into());
            }
        }

        // Check for oneway flagged methods with return type
        for m in &i.methods {
            if m.type_.is_some() && m.is_oneway() {
                return Err(ModelError::OnewayMethodReturnType {
                    outer: i.name.to_string(),
                    name: m.ident.to_string(),
                }
                .into());
            }
        }

        let name = i.name.clone();
        self.objects.insert(name, Object::Interface(Rc::new(RefCell::new(i))));
        Ok(())
    }

    /// Find a class by indentifier
    pub fn find_class(&self, name: &Name) -> Option<Rc<RefCell<Class>>> {
        self.objects.get(name).and_then(|o| match o {
            Object::Class(o) => Some(o.clone()),
            _ => None,
        })
    }

    /// Find a interface by name
    pub fn find_interface(&self, name: &Name) -> Option<Rc<RefCell<Interface>>> {
        self.objects.get(name).and_then(|o| match o {
            Object::Interface(o) => Some(o.clone()),
            _ => None,
        })
    }

    /// Find a enumeration by name
    pub fn find_enumeration(&self, name: &Name) -> Option<Rc<RefCell<Enumeration>>> {
        self.objects.get(name).and_then(|o| match o {
            Object::Enumeration(o) => Some(o.clone()),
            _ => None,
        })
    }
}

trait Resolve: Sized {
    fn resolve(&self, model: &Model, outer: &Name) -> Result<Self, Error>;
}

impl Resolve for Type {
    fn resolve(&self, model: &Model, outer: &Name) -> Result<Self, Error> {
        match self {
            Type::Collection(c) => {
                let t = c.resolve(model, outer)?;
                Ok(Type::Collection(Rc::new(t)))
            }
            Type::Object(o) => Ok(Type::Object(o.resolve(model, outer)?)),
            Type::Pod(_) => Ok(self.clone()),
        }
    }
}

impl Resolve for Collection {
    fn resolve(&self, model: &Model, outer: &Name) -> Result<Self, Error> {
        match self {
            Collection::Array(ref i) => {
                let a = i.resolve(model, outer)?;
                Ok(Collection::Array(a))
            }
            Collection::List(i) => {
                let a = i.resolve(model, outer)?;
                Ok(Collection::List(a))
            }
            Collection::Map(k, i) => {
                let a = i.resolve(model, outer)?;
                // Safe to clone POD
                Ok(Collection::Map(k.clone(), a))
            }
            Collection::Set(i) => {
                let a = i.resolve(model, outer)?;
                Ok(Collection::Set(a))
            }
        }
    }
}

impl Resolve for Object {
    fn resolve(&self, model: &Model, outer: &Name) -> Result<Self, Error> {
        match self {
            Object::Class(_) | Object::Enumeration(_) | Object::Interface(_) => Ok(self.clone()), // clones Rc
            Object::Unresolved(unresolved) => {
                // If a used type doen't contain a package path the default of the package
                // of the outer struct/interface is taken.
                let unresolved = if unresolved.package.is_empty() {
                    Name::new(outer.package.clone(), unresolved.ident.clone())
                } else {
                    unresolved.clone()
                };

                match model.objects.get(&unresolved).cloned() {
                    Some(o) => Ok(o),
                    None => Err(ModelError::Unresolved {
                        outer: outer.to_string(),
                        name: unresolved.to_string(),
                    }
                    .into()),
                }
            }
        }
    }
}

pub fn resolve(model: Model) -> Result<Model, Error> {
    for o in model.objects.values() {
        match o {
            Object::Class(c) => {
                let mut class = c.borrow_mut();
                let name = class.name.clone();

                for field in &mut class.fields {
                    field.type_ = field.type_.clone().resolve(&model, &name)?;
                }
            }
            Object::Interface(i) => {
                let mut interface = i.borrow_mut();
                let name = interface.name.clone();
                for method in &mut interface.methods {
                    if let Some(ref t) = method.type_ {
                        method.type_ = Some(t.clone().resolve(&model, &name)?);
                    }

                    for arg in &mut method.arguments {
                        arg.type_ = arg.type_.clone().resolve(&model, &name)?;
                    }
                }
            }
            _ => (),
        }
    }

    Ok(model)
}

fn dependencies(types: &[&Type], recursive: bool) -> HashSet<Dependency> {
    types
        .iter()
        .filter_map(|t| match t {
            Type::Collection(c) => match **c {
                Collection::Array(ref t)
                | Collection::List(ref t)
                | Collection::Map(_, ref t)
                | Collection::Set(ref t) => {
                    let mut deps = vec![c.into()];
                    deps.extend(dependencies(&[t], recursive));
                    Some(deps)
                }
            },
            Type::Object(Object::Class(c)) => {
                let mut deps = vec![Dependency::Name(c.borrow().name.clone())];
                if recursive {
                    deps.extend(c.borrow().dependencies(true))
                }
                Some(deps)
            }
            Type::Object(Object::Enumeration(e)) => Some(vec![Dependency::Name(e.borrow().name.clone())]),
            Type::Object(Object::Interface(i)) => Some(vec![Dependency::Name(i.borrow().name.clone())]),
            Type::Object(Object::Unresolved(_)) => panic!("Unresolved type"),
            Type::Pod(_) => None,
        })
        .flat_map(|x| x)
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn name_from_str() {
        assert_eq!(
            Name::new(Package(["a".to_owned(), "b".to_owned()].to_vec()), "c".into()),
            "a.b.c".into()
        );
        assert_eq!(Name::new(Package(vec![]), "a".into()), "a".into());
        assert_eq!(Name::new(Package(vec![]), "abc".into()), "abc".into());
    }

    #[test]
    fn duplicate_enum_name() {
        let mut model = Model::default();
        let enumeration = Enumeration::new("a.b.C".into(), vec![], Annotations::default(), MetaInfo::default());
        assert!(model.push_enumeration(enumeration.clone()).is_ok());
        assert!(model.push_enumeration(enumeration).is_err());
    }

    #[test]
    fn duplicate_class_name() {
        let mut model = Model::default();
        let class = Class::new(
            Name::from("a.b.Test"),
            vec![],
            vec![],
            Annotations::default(),
            MetaInfo::default(),
        );
        model.push_class(class.clone()).unwrap();
        assert!(model.push_class(class).is_err());
    }

    #[test]
    fn duplicate_class_field_name() {
        let mut model = Model::default();
        let field = Field::new(Annotations::new(HashMap::new()), Type::Pod(PodType::Bool), "foo".into());
        let class = Class::new(
            Name::from("a.b.C"),
            vec![field.clone(), field],
            vec![],
            Annotations::default(),
            MetaInfo::default(),
        );
        assert!(model.push_class(class).is_err());
    }

    #[test]
    fn duplicate_interface_name() {
        let mut model = Model::default();
        let interface = Interface::new("a.b.C".into(), vec![], Annotations::default(), MetaInfo::default());
        assert!(model.push_interface(interface.clone()).is_ok());
        assert!(model.push_interface(interface).is_err());
    }

    #[test]
    fn duplicate_name() {
        let mut model = Model::default();
        let interface = Interface::new("a.b.C".into(), vec![], Annotations::default(), MetaInfo::default());
        let class = Class::new(
            Name::from("a.b.C"),
            vec![],
            vec![],
            Annotations::default(),
            MetaInfo::default(),
        );
        assert!(model.push_interface(interface).is_ok());
        assert!(model.push_class(class).is_err());
    }

    #[test]
    fn invalid_method() {
        let mut model = Model::default();
        // Method with return type but oneway
        let method = Method::new(
            "foo".into(),
            vec![],
            Some(Type::Pod(PodType::Bool)),
            Annotations::with_empty_group(ONEWAY_ANNOTATION.to_string()),
        );
        let interface = Interface::new(
            "a.b.C".into(),
            vec![method],
            Annotations::default(),
            MetaInfo::default(),
        );
        assert!(model.push_interface(interface).is_err());
    }

    #[test]
    fn duplicate_method() {
        let mut model = Model::default();
        let a = Method::new("foo".into(), vec![], None, Annotations::default());
        let b = Method::new(
            "foo".into(),
            vec![],
            Some(Type::Pod(PodType::Bool)),
            Annotations::default(),
        );

        // a and b differ only in return type
        let interface = Interface::new(
            "a.b.C".into(),
            // Identical methods
            vec![a, b],
            Annotations::default(),
            MetaInfo::default(),
        );
        assert!(model.push_interface(interface).is_err());
    }
}
