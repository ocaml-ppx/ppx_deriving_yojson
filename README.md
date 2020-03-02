[@@deriving yojson]
===================

_deriving Yojson_ is a [ppx_deriving][pd] plugin that generates
[JSON][] serializers and deserializes that use the [Yojson][] library
from an OCaml type definition.

Sponsored by [Evil Martians](http://evilmartians.com).

[pd]: https://github.com/ocaml-ppx/ppx_deriving
[json]: http://tools.ietf.org/html/rfc4627
[yojson]: https://github.com/ocaml-community/yojson

Installation
------------

_deriving Yojson_ can be installed via [OPAM](https://opam.ocaml.org):

    $ opam install ppx_deriving_yojson

Usage
-----

In order to use _deriving yojson_, require the package `ppx_deriving_yojson`.

If you are using dune, add `ppx_deriving_json` to the `preprocess` entry, and `ppx_deriving_json.runtime` to your requirements, like so:

```
...
(libraries  yojson core ppx_deriving_yojson.runtime) ;
(preprocess (pps ppx_deriving_yojson)) ;
...
```

Syntax
------

_deriving yojson_ generates two functions per type:

``` ocaml
# #require "ppx_deriving_yojson";;
# type ty = .. [@@deriving yojson];;
val ty_of_yojson : Yojson.Safe.t -> (ty, string) Result.result
val ty_to_yojson : ty -> Yojson.Safe.t
```

When the deserializing function returns <code>Error loc</code>, `loc` points to the point in the JSON hierarchy where the error has occurred.

It is possible to generate only serializing or deserializing functions by using `[@@deriving to_yojson]` or `[@@deriving of_yojson]`. It is also possible to generate an expression for serializing or deserializing a type by using `[%to_yojson:]` or `[%of_yojson:]`; non-conflicting versions `[%derive.to_yojson:]` or `[%derive.of_yojson:]` are available as well. Custom or overriding serializing or deserializing functions can be provided on a per-field basis via `[@to_yojson]` and `[@of_yojson]` attributes.

If the type is called `t`, the functions generated are `{of,to}_yojson` instead of `t_{of,to}_yojson`.

Using the option `[@@deriving yojson { exn = true }]` will also generate a function `ty_of_yojson_exn : Yojson.Safe.t -> ty` which raises `Failure err` on error instead of returning an `Error err` result.

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
| A tuple                | Array      |                                  |
| `ref`                  | 'a         |                                  |
| `option`               | Null or 'a |                                  |
| A record               | Object     |                                  |
| `Yojson.Safe.t`        | any        | Identity transformation          |
| `unit`                 | Null       |                                  |

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

Record variants are represented in the same way as if the nested structure was defined separately. For example:

```ocaml
# type v = X of { v: int } [@@deriving yojson];;
# print_endline (Yojson.Safe.to_string (v_to_yojson (X { v = 0 })));;
["X",{"v":0}]
```

Record variants are currently not supported for extensible variant types.

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

#### [@to_yojson] / [@of_yojson]

One can provide custom serialization or deserialization functions, either
overriding the default derivation or to provide support for abstract, functor,
or other types that aren't otherwise amenable to derivation (similar to the
`@printer` option provided by [ppx_deriving's `show` plugin](https://github.com/ocaml-ppx/ppx_deriving#plugin-show)):

```ocaml
# module StringMap = Map.Make(struct type t = string let compare = compare end);;
# let yojson_of_stringmap m = StringMap.bindings m
                           |> [%to_yojson: (string * string) list];;
# type page = { number : int [@to_yojson fun i -> `Int (i + 1)]
              ; bounds : (int * int * int * int)
              ; attrs  : string StringMap.t [@to_yojson yojson_of_stringmap]}
              [@@deriving to_yojson];;
# { number = 0
  ; bounds = (0, 0, 792, 612)
  ; attrs  = StringMap.add "foo" "bar" StringMap.empty }
  |> page_to_yojson
  |> Yojson.Safe.to_string
  |> print_endline

{"number":1,"bounds":[0,0,792,612],"attrs":[["foo","bar"]]}
```

#### `Yojson_meta` module

The `meta` deriver option can be used to generate a module containing all JSON key names, e.g.

```ocaml
type foo = {
 fvalue : float;
 svalue : string [@key "@svalue_json"];
 ivalue : int;
} [@@deriving to_yojson { strict = false, meta = true } ]
end
```

defines the following module:

```ocaml
module Yojson_meta_foo = struct
  let keys = ["fvalue"; "@svalue_json"; "ivalue"]
  let _ = keys
end
```

When the type is named `t`, the module is named just `Yojson_meta`.

License
-------

_deriving yojson_ is distributed under the terms of [MIT license](LICENSE.txt).
