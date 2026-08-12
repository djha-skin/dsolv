![dsolv logo](docs/assets/dsolv.png)

# dsolv

A generic dependency resolver written in Common Lisp.

dsolv is a port of the [degasolv](https://github.com/djhaskin987/degasolv)
dependency resolver, originally written in Clojure. It exists independent of
programming languages or systems: you can use it to declare the existence of
files that your build depends on, version them, and retrieve the URLs of files
which are of the correct versions. You can easily use these URLs in your builds
to download everything the build needs.

Since dsolv is a dependency resolver that is relatively technology-agnostic,
you can declare dependencies between components that are not of the same
technology. You can declare that a DLL depends on a pip package, or that in
order to use an NPM package, a certain ruby gem file must be present as well.

## Documentation

The full manual is published at
[**djha-skin.github.io/dsolv**](https://djha-skin.github.io/dsolv/), covering
the [quickstart](https://djha-skin.github.io/dsolv/quickstart.html), the
[command reference](https://djha-skin.github.io/dsolv/command-reference.html),
and the [API reference](https://djha-skin.github.io/dsolv/api-reference.html).

## Building

Build the `dsolv` executable with:

```bash
./scripts/build
```

Then get an overview of the command line interface:

```bash
dsolv help
```

## Running the Tests

The test suite runs under [Parachute](https://github.com/Shinmera/parachute)
via ASDF:

```lisp
(asdf:test-system :com.djhaskin.dsolv/tests)
```

## Status

dsolv is an active port of the degasolv dependency resolver. It is developed in
[Common Lisp](https://common-lisp.net/), on top of the
[CLIFF](https://github.com/djha-skin/cliff) command line framework and the
[NRDL](https://github.com/djha-skin/nrdl) data language.
