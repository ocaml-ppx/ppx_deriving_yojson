# Contributing to `ppx_deriving_yojson`

## Setting up

This document assumes you have [OPAM](https://opam.ocaml.org/) installed.

### Installing

To start building this project you will need to install the packages it depends
on. To do so, run the following command:

```shell
$ opam install . --deps-only -t
```

## Developing

### Building & Testing

This project uses [dune](http://dune.build/) as its build system. The
[Makefile](./Makefile) in this repo provides shorter commands over the `dune`
commands.

#### Building

To build the project, run `make` or `make build`.

### Running Tests

`make test` will build and run the tests in the current OPAM switch.

### Cleaning up

`make clean` can be used to clean up the build artifacts.

## Cutting a release

### Testing for a release

Before cutting a release, it is useful to test this project against all the
supported OCaml versions. `make all-supported-ocaml-versions` will do just that,
but requires some setting up beforehand. The instructions are as follows:

1. The [`dune-workspace.dev`](./dune-workspace.dev) defines all the OPAM
   switches that will be tested when running `make
   all-supported-ocaml-versions`. Make sure you have switches for all those
   OCaml version, with the appropriate names (e.g., for the build context that
   `(context (opam (switch 4.07.1)))` defines, make sure you have a switch named
   `4.07.1`. To find out which OPAM switches you have, run `opam switch list`).
2. For every OPAM switch listed in the Dune workspace file, switch into it and
   run the installation command at the top of this document.
3. Finally, you can now run `make all-supported-ocaml-versions`, which will
   build and test this project against all those OCaml versions.

### Making a release

- WIP