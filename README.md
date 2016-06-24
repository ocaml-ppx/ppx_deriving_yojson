[@@deriving yojson]
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

In order to use _deriving yojson_, require the package `ppx_deriving_yojson`.

Syntax
------

_deriving yojson_ generates two functions per type:

``` ocaml
# #require "ppx_deriving";;
# type ty = .. [@@deriving yojson];;
val ty_of_yojson : Yojson.Safe.json -> (ty, string) Result.result
val ty_to_yojson : ty -> Yojson.Safe.json
```

When the deserializing function returns <code>\`Error loc</code>, `loc` points to the point in the JSON hierarchy where the error has occurred.

It is possible to generate only serializing or deserializing functions by using `[@@deriving to_yojson]` or `[@@deriving of_yojson]`. It is also possible to generate an expression for serializing or deserializing a type by using `[%to_yojson:]` or `[%of_yojson:]`; non-conflicting versions `[%derive.to_yojson:]` or `[%derive.of_yojson:]` are available as well.

If the type is called `t`, the functions generated are `{of,to}_yojson` instead of `t_{of,to}_yojson`.

Semantics
---------

_deriving yojson_ handles tuples, records, normal and polymorphic variants; builtin types: `int`, `int32`, `int64`, `nativeint`, `float`, `bool`, `char`, `string`, `bytes`, `ref`, `list`, `array`, `option` and their `Mod.t` aliases.

The following table summarizes the correspondence between OCaml types and JSON values:

| OCaml type             | JSON value | Remarks                          |
| ---------------------- | ---------- | -------------------------------- |
| `int`, `int32`, `float`| Number     |                                  |
| `int64`, `nativeint`   | Number     | Can exceed range of `double`     |
| `bool`                 | Boolean    |                                  |
| `string`, `bytes`      | String     |                                  |
| `char`                 | String     | Strictly one character in length |
| `list`, `array`        | Array      |                                  |
| `ref`                  | 'a         |                                  |
| `option`               | Null or 'a |                                  |
| A record               | Object     |                                  |
| `Yojson.Safe.json`     | any        | Identity transformation          |

Variants (regular and polymorphic) are represented using arrays; the first element is a string with the name of the constructor, the rest are the arguments. Note that the implicit tuple in a polymorphic variant is flattened. For example:

``` ocaml
# type pvs = [ `A | `B of int | `C of int * string ] list [@@deriving yojson];;
# type v = A | B of int | C of int * string [@@deriving yojson];;
# type vs = v list [@@deriving yojson];;
# print_endline (Yojson.Safe.to_string (vs_to_yojson [A; B 42; C (42, "foo")]));;
[["A"],["B",42],["C",42,"foo"]]
# print_endline (Yojson.Safe.to_string (pvs_to_yojson [`A; `B 42; `C (42, "foo")]));;
[["A"],["B",42],["C",42,"foo"]]
```

By default, objects are deserialized strictly; that is, all keys in the object have to correspond to fields of the record. Passing `strict = false` as an option to the deriver  (i.e. `[@@deriving yojson { strict = false }]`) changes the behavior to ignore any unknown fields.

### Options

Option attribute names may be prefixed with `yojson.` to avoid conflicts with other derivers.

#### [@key]

If the JSON object keys differ from OCaml conventions, lexical or otherwise, it is possible to specify the corresponding JSON key implicitly using <code>[@key "field"]</code>, e.g.:

``` ocaml
type geo = {
  lat : float [@key "Latitude"];
  lon : float [@key "Longitude"];
}
[@@deriving yojson]
```

#### [@name]

If the JSON variant names differ from OCaml conventions, it is possible to specify the corresponding JSON string explicitly using <code>[@name "constr"]</code>, e.g.:

``` ocaml
type units =
| Metric   [@name "metric"]
| Imperial [@name "imperial"]
[@@deriving yojson]
```

#### [@encoding]

Very large `int64` and `nativeint` numbers can wrap when decoded in a runtime which represents all numbers using double-precision floating point, e.g. JavaScript and Lua. It is possible to specify the <code>[@encoding \`string]</code> attribute to encode them as strings.

#### [@default]

It is possible to specify a default value for fields that can be missing from the JSON object, e.g.:

``` ocaml
type pagination = {
  pages   : int;
  current : (int [@default 0]);
} [@@deriving yojson]
```

Fields with default values are not required to be present in inputs and will not be emitted in outputs.

License
-------

_deriving yojson_ is distributed under the terms of [MIT license](LICENSE.txt).
