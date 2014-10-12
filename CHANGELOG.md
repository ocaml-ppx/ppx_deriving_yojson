Changelog
=========

0.3
---

  * Common helper functions have been extracted into
    ppx_deriving_yojson.runtime, reducing code size.
  * Add support for `[@@deriving to_yojson, of_yojson]`
    and `[%to_yojson:]`, `[%of_yojson:]` shortcuts.

0.2
---

  * Add `[@key]`, `[@name]` and `[@default]` attributes.
  * Add support for `Yojson.Safe.json` values.

0.1
---

  * Initial release.
