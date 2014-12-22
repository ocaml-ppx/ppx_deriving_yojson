Changelog
=========

2.2
---

2.1
---

  * Handle inheriting from a parametric polymorphic variant type.
  * Don't leak type variables.

2.0
---

  * Update to accomodate syntactic changes in _deriving_ 1.0.
  * Common helper functions have been extracted into
    ppx_deriving_yojson.runtime, reducing code size.
  * Add support for `[@@deriving to_yojson, of_yojson]`
    and `[%to_yojson:]`, `[%of_yojson:]` shortcuts.
  * Add support for `[@@deriving yojson { strict = false }]`.

1.1
---

  * Add `[@key]`, `[@name]` and `[@default]` attributes.
  * Add support for `Yojson.Safe.json` values.

1.0
---

  * Initial release.
