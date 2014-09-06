[@@deriving Yojson]
===================

_deriving Yojson_ is a [ppx_deriving][pd] plugin that generates
[JSON][] serializers and deserializes that use the [Yojson][] library
from an OCaml type definition.

Sponsored by [Evil Martians](http://evilmartians.com).

[pd]: https://github.com/whitequark/ppx_deriving
[json]: http://tools.ietf.org/html/rfc4627
[yojson]: http://mjambon.com/yojson.html

Installation
------------

_deriving Yojson_ can be installed via [OPAM](https://opam.ocaml.org):

    $ opam install ppx_deriving_yojson

Usage
-----

In order to use _deriving Yojson_, require the syntax extension package
`ppx_deriving` and the runtime library package `ppx_deriving_yojson.runtime`.

Syntax
------

_deriving Yojson_ generates two functions per type:

    type ty = ... [@@deriving Yojson]
    val ty_of_yojson : Yojson.Safe.json -> [ `Ok of t | `Error of string ]
    val ty_to_yojson : t -> Yojson.Safe.json

When the deserializing function returns <code>\`Error loc</code>, `loc` points to the point in the JSON hierarchy where the error has occurred.

Semantics
---------

_deriving Yojson_ handles tuples, records, normal and polymorphic variants; builtin types: `int`, `int32`, `int64`, `nativeint`, `float`, `bool`, `char`, `string`, `bytes`, `ref`, `list`, `array`, `option` and their `Mod.t` aliases.

The following table summarizes the correspondence between OCaml types and JSON values:

| OCaml type             | JSON value | Remarks                                        |
| ---------------------- | ---------- | ---------------------------------------------- |
| `int`, `int32`, `float`| Number     |                                                |
| `int64`, `nativeint`   | Number     | Can exceed range of `float` e.g. in JavaScript |
| `bool`                 | Boolean    |                                                |
| `string`, `bytes`      | String     |                                                |
| `char`                 | String     | Strictly one character in length               |
| `list`, `array`        | Array      |                                                |
| `ref`                  | 'a         |                                                |
| `option`               | Null or 'a |                                                |
| A record               | Object     |                                                |

Variants (regular and polymorphic) are represented using arrays; the first element is a string with the name of the constructor, the rest are the arguments. Note that the implicit tuple in a polymorphic variant is flattened. For example:

``` ocaml
# type pvs = [ `A | `B of int | `C of int * string ] list [@@deriving Yojson];;
# type v = A | B of int | C of int * string [@@deriving Yojson];;
# type vs = v list [@@deriving Yojson];;
# print_endline (Yojson.Safe.to_string (vs_to_yojson [A; B 42; C (42, "foo")]));;
[["A"],["B",42],["C",42,"foo"]]
# print_endline (Yojson.Safe.to_string (pvs_to_yojson [`A; `B 42; `C (42, "foo")]));;
[["A"],["B",42],["C",42,"foo"]]
```

License
-------

_deriving Yojson_ is distributed under the terms of [MIT license](LICENSE.txt).
