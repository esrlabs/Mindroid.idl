[![Travis Build Status](https://travis-ci.org/esrlabs/Mindroid.idl.svg?branch=master)](https://travis-ci.org/esrlabs/Mindroid.idl)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/esrlabs/Mindroid.idl?branch=master&svg=true)](https://ci.appveyor.com/project/flxo/mindroid-idl)

# Mindroid.idl

Mindroid is an application framework (with focus on messaging and
concurrency) that lets you create applications using a set of reusable
components - just like Android. The name Mindroid has two different meanings.
On one hand Mindroid is a minimal set of core Android classes and on the
other hand these classes also form Android's mind. Refer to
[Mindroid.java](https://github.com/esrlabs/Mindroid.java) and
[Mindroid.cpp](https://github.com/esrlabs/Mindroid.cpp) for details.

Mindroid.idl is a interface definition language and a code generator for the Mindroid framework.
The IDL (midl) is a simple protocol agnostic interface definition that is designed for readability.

## MIDL: Mindroid Interface Definition Language

Midl defnitions are stored in ASCII text files. Comments in `midl` files are
indentified by a double forward slash (`//`) and can appear at almost any location
in the file. Everything between the douple slash and the next newline
character(s) is irgnored.

Each `midl` definition must be placed in a dedicated package with the
`package` keyword that may appear once is a `midl` file (same concept as
Java). Package definitions are mandatory.

```java
package examples.time;
```

A structure is declared with the `class` or `struct` keyword. A type in `midl` is always
placed *before* the identifier. A simple class definition that contains two POD types looks like this:

```java
class Time {
    long subsecNanos;
    int seconds;
}
```

Structures can contain contants that are defined in the class definition:

```java
class Time {
    const long ZERO = 0;
    const String UTC = "UTC";
    ...
}
```

To declare a structure directly in a particular package, use the full qualified name in the definition:

```java
class examples.time.Time {
    ...
```

Classes can be nested unless they have recursive dependencies.

Midl supports a enumeration type that is declared with the `enum` keyword.

```java
enum TimeZone {
    CEST
    CET,
    GMT,
    UTC,
    ...
}
```

Interfaces start with the `interface` keyword. Method declarations follow the common form of Java or C/C++:

```java
interface ITimer {
    // Starts the timer with the duration `duration`. Upon expiration callback will be called.
    // Returns an id to match expiration events.
    int start(int duration, ICallback callback);
    // Stop the time and return the time since it has been started.
    int stop();
}

interface ICallback {
    void onExpiration(int id);
}
```

Method argument types can be `PODs`, `classes` or `interface` definitions.

The full midl grammar is declared [here](./midl-common/src/parser/midl.pest).

## Getting Started

The `midl` generator is a command line application that is controlled via switches. Each input and output format is
handeled in a dedicated subcommand. 

```
# midl --help

idl 0.1.0

USAGE:
    midl [FLAGS] <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information
    -v, --verbose    Verbose output

SUBCOMMANDS:
    help    Prints this message or the help of the given subcommand(s)
    midl    Parse midl
```

Each subcommand has specific options that can be listed with `--help`. For exampel the `mindroid`
generator supports the following options:

```
# midl midl mindroid --help

midl-midl-mindroid 0.1.0
Generate Mindroid protocol code

USAGE:
    midl midl --input <input> mindroid [OPTIONS] --language <language> --output <output>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -c, --copyright <copyright>    Copyright text (optional)
    -l, --language <language>      Select language [possible values: c++, java]
    -L, --license <license>        Select license for generated code: ESRLabs, Apache2.0 or <FILE> (optional)
    -o, --output <output>          Output directory
```

### Installation

Obtain a `midl` binary from the projects [release page](https://github.com/esrlabs/Mindroid.idl/releases).
Compiling `midl` from source requires a recent `Rust` setup that is easiest obtaind via [Rustup](https://rustup.rs).

```
git clone https://github.com/esrlabs/Mindroid.idl.git
cd Mindroid.idl/midl
cargo install --path .
```

### Examples

[Mindroid.java](https://github.com/esrlabs/Mindroid.java) and
[Mindroid.cpp](https://github.com/esrlabs/Mindroid.cpp) contain a example service
definition (Eliza) that can be generated with `midl`. The definition is located in
[Eliza.midl](https://github.com/esrlabs/Mindroid.java/blob/master/examples/Eliza/res/Eliza.midl)
and does *not* contain any language specific elements.

### Java

```
git clone https://github.com/esrlabs/Mindroid.java
cd Mindroid.java
midl midl -i examples/Eliza/res/Eliza.midl mindroid -l java -o examples/Eliza/gen -L Apache2.0 -c "2019 ESR Labs"
```

### C++ 

```
git clone https://github.com/esrlabs/Mindroid.cpp
cd Mindroid.cpp
midl midl -i examples/Eliza/res/Eliza.midl mindroid -l c++ -o examples/Eliza/gen -L Apache2.0 -c "2019 ESR Labs"
```

## Extended input and output formats: Fibex and SomeIP

The Mindroid IDL generator can generate services that implement the SomeIP
protocol declared by [AUTOSAR](https://www.autosar.org). SomeIP services are
often specified in the [ASAM Fibex](https://www.asam.net) format that can be
used by `midl` to generate SomeIP compliant code. Contact [ESRLabs
AG](https://www.esrlabs.com/company/#contact) for details.
