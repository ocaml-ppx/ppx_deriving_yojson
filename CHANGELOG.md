3.6.1
-----

  * Update to ppxlib >= 0.14.0
    (#127)
    Kate Deplaix

3.6.0
-----

  * Update to ppx_deriving 5.0 and ppxlib
    (#121)
    Rudi Grinberg, Thierry Martinez, Kate Deplaix and Gabriel Scherer

  * Fix issues when the equality operator `(=)` is shadowed
    (#126, #128, #131, fixes #79)
    Martin Slota, Kate Deplaix

3.5.3
-----

  * Support for OCaml 4.11 (requires feature from `ppx_deriving.4.5`)
    (#122)
    Thierry Martinez
  * Documentation improvements
    (#115)
    Olivier Andrieu

3.5.2
-----

  * [@to_yojson], [@from_yojson] to override serialization functions
    for certain record fields
    (#107, #108)
    Chas Emerick
  * Support for OCaml 4.10
    (#112)
    Kate Deplaix

3.5.1
-----

  * Two bugfixes when using [%to_json ], [%of_json ] extensions
    (error with polymorphic variables, unbound value 'safe_map')
    (#100, #101)
    Gabriel Scherer, report by Matt Windsor

3.5
---

  * use tail-recursive functions to (de)serialize long lists
    (#97)
    Alex Knauth
  * Support for OCaml 4.08
    (#99)
    Antonio Nuno Monteiro

3.4
---

  * compatibility with yojson 1.6.0
    (#90, #92)
    Vadim Radovel and Nathan Rebours

3.3
---

  * Make `_exn` functions opt-in (`[@@deriving yojson { exn = true }]`)
    to preserve backward-compatibility for fully-manual implementations
    of the [@@deriving yojson] interface.
    (#86)
    Gabriel Scherer

3.2
---

  * Add `let _ = to_yojson / of_yojson` to generated code to avoid warnings when
    they aren't used
    (#68)
    Steve Bleazard
  * Fix bug where doing [@@deriving of_yojson] causes an unused rec warning
    (#68)
    Steve Bleazard
  * Add generated `ty_of_yojson_exn` to raise an exception rather than return an
    error
    (#57, #68)
    Steve Bleazard
  * Port `ppx_deriving_yojson` to `dune`
    (#69, #85)
    Rudi Grinberg, Antonio Nuno Monteiro
  * Added deriver option `fields` to generate a `Yojson_meta` module containing
    all JSON key names.
    (#70)
    Steve Bleazard
  * Remove cppo that included support for versions no longer supported by
    `ppx_deriving_yojson`
    (#75)
    Rudi Grinberg

3.1
---

  * Fix ppx_deriving_yojson.runtime META file
    (#47)
    Étienne Millon
  * Support for inline records in variant types
    (#50)
    Gerd Stolpmann
  * OCaml 4.06 compatibility
    (#64, #66)
    Leonid Rozenberg, Gabriel Scherer

3.0
---

  * Use Result.result in generated code.
  * Compatibility with statically linked ppx drivers.
  * OCaml 4.03 compatibility.

2.3
---

  * Adapt to syntactic changes in 4.02.2.
  * Improve compatibility with libraries that shadow modules
    from standard library, such as Core.
  * Allow deserializing float values that appear as integer
    literals in the input JSON.
  * Suppress some warnings.

2.2
---

  * Add support for open types.

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
