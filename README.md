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

TBD

License
-------

_deriving Yojson_ is distributed under the terms of [MIT license](LICENSE.txt).
