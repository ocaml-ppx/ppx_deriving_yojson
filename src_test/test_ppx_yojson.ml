open OUnit2

type json = [%import: Yojson.Safe.json] [@@deriving show]

let show_error_or =
  let module M = struct
    type 'a error_or =
      [%import: 'a Ppx_deriving_yojson_runtime.error_or] [@@deriving show]
  end in
  M.show_error_or

let assert_roundtrip pp_obj to_json of_json obj str =
  let json = Yojson.Safe.from_string str in
  let cleanup json = Yojson.Safe.(json |> to_string |> from_string) in
  assert_equal ~printer:show_json json (cleanup (to_json obj));
  assert_equal ~printer:(show_error_or pp_obj) (Result.Ok obj) (of_json json)

let assert_failure pp_obj of_json err str =
  let json = Yojson.Safe.from_string str in
  assert_equal ~printer:(show_error_or pp_obj) (Result.Error err) (of_json json)

type u = unit         [@@deriving show, yojson]
type i1 = int         [@@deriving show, yojson]
type i2 = int32       [@@deriving show, yojson]
type i3 = Int32.t     [@@deriving show, yojson]
type i4 = int64       [@@deriving show, yojson]
type i5 = Int64.t     [@@deriving show, yojson]
type i6 = nativeint   [@@deriving show, yojson]
type i7 = Nativeint.t [@@deriving show, yojson]
type i8 = int64       [@encoding `string] [@@deriving show, yojson]
type i9 = nativeint   [@encoding `string] [@@deriving show, yojson]
type f  = float       [@@deriving show, yojson]
type b  = bool        [@@deriving show, yojson]
type c  = char        [@@deriving show, yojson]
type s  = string      [@@deriving show, yojson]
type y  = bytes       [@@deriving show, yojson]
type xr = int ref     [@@deriving show, yojson]
type xo = int option  [@@deriving show, yojson]
type xl = int list    [@@deriving show, yojson]
type xa = int array   [@@deriving show, yojson]
type xt = int * int   [@@deriving show, yojson]

type 'a p = 'a option
[@@deriving show, yojson]
type pv = [ `A | `B of int | `C of int * string ]
[@@deriving show, yojson]
type pva = [ `A ] and pvb = [ `B ]
[@@deriving show, yojson]
type 'a pvc = [ `C of 'a ]
[@@deriving show, yojson]
type pvd = [ pva | pvb | int pvc ]
[@@deriving show, yojson]

type v  = A | B of int | C of int * string
[@@deriving show, yojson]
type r  = { x : int; y : string }
[@@deriving show, yojson]

let test_unit ctxt =
  assert_roundtrip pp_u u_to_yojson u_of_yojson
    () "null"

let test_int ctxt =
  assert_roundtrip pp_i1 i1_to_yojson i1_of_yojson
                   42 "42";
  assert_roundtrip pp_i2 i2_to_yojson i2_of_yojson
                   42l "42";
  assert_roundtrip pp_i3 i3_to_yojson i3_of_yojson
                   42l "42";
  assert_roundtrip pp_i4 i4_to_yojson i4_of_yojson
                   42L "42";
  assert_roundtrip pp_i5 i5_to_yojson i5_of_yojson
                   42L "42";
  assert_roundtrip pp_i6 i6_to_yojson i6_of_yojson
                   42n "42";
  assert_roundtrip pp_i7 i7_to_yojson i7_of_yojson
                   42n "42";
  assert_roundtrip pp_i8 i8_to_yojson i8_of_yojson
                   42L "\"42\"";
  assert_roundtrip pp_i9 i9_to_yojson i9_of_yojson
                   42n "\"42\""

let test_int_edge ctxt =
  assert_roundtrip pp_i2 i2_to_yojson i2_of_yojson
                   0x7fffffffl "2147483647";
  assert_roundtrip pp_i2 i2_to_yojson i2_of_yojson
                   (Int32.neg 0x80000000l) "-2147483648";
  assert_roundtrip pp_i4 i4_to_yojson i4_of_yojson
                   0x7fffffffffffffffL "9223372036854775807";
  assert_roundtrip pp_i4 i4_to_yojson i4_of_yojson
                   (Int64.neg 0x8000000000000000L) "-9223372036854775808"

let test_float ctxt =
  assert_roundtrip pp_f f_to_yojson f_of_yojson
                   1.0 "1.0";
  assert_equal ~printer:(show_error_or pp_f)
               (Result.Ok 1.0)
               (f_of_yojson (`Int 1))

let test_bool ctxt =
  assert_roundtrip pp_b b_to_yojson b_of_yojson
                   true "true";
  assert_roundtrip pp_b b_to_yojson b_of_yojson
                   false "false"

let test_char ctxt =
  assert_roundtrip pp_c c_to_yojson c_of_yojson
                   'c' "\"c\"";
  assert_failure   pp_c c_of_yojson
                   "Test_ppx_yojson.c" "\"xxx\""

let test_string ctxt =
  assert_roundtrip pp_s s_to_yojson s_of_yojson
                   "foo" "\"foo\"";
  assert_roundtrip pp_y y_to_yojson y_of_yojson
                   (Bytes.of_string "foo") "\"foo\""

let test_ref ctxt =
  assert_roundtrip pp_xr xr_to_yojson xr_of_yojson
                   (ref 42) "42"

let test_option ctxt =
  assert_roundtrip pp_xo xo_to_yojson xo_of_yojson
                   (Some 42) "42";
  assert_roundtrip pp_xo xo_to_yojson xo_of_yojson
                   None "null"

let test_list ctxt =
  assert_roundtrip pp_xl xl_to_yojson xl_of_yojson
                   [] "[]";
  assert_roundtrip pp_xl xl_to_yojson xl_of_yojson
                   [42; 43] "[42, 43]"

let test_array ctxt =
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [||] "[]";
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [|42; 43|] "[42, 43]"

let test_tuple ctxt =
  assert_roundtrip pp_xt xt_to_yojson xt_of_yojson
                   (42, 43) "[42, 43]"

let test_ptyp ctxt =
  assert_roundtrip (pp_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   (Some 42) "42";
  assert_roundtrip (pp_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   None "null"

let test_pvar ctxt =
  assert_roundtrip pp_pv pv_to_yojson pv_of_yojson
                   `A "[\"A\"]";
  assert_roundtrip pp_pv pv_to_yojson pv_of_yojson
                   (`B 42) "[\"B\", 42]";
  assert_roundtrip pp_pv pv_to_yojson pv_of_yojson
                   (`C (42, "foo")) "[\"C\", 42, \"foo\"]";
  assert_roundtrip pp_pvd pvd_to_yojson pvd_of_yojson
                   `A "[\"A\"]";
  assert_roundtrip pp_pvd pvd_to_yojson pvd_of_yojson
                   `B "[\"B\"]";
  assert_roundtrip pp_pvd pvd_to_yojson pvd_of_yojson
                   (`C 1) "[\"C\", 1]";
  assert_equal ~printer:(show_error_or pp_pvd)
               (Result.Error "Test_ppx_yojson.pvd")
               (pvd_of_yojson (`List [`String "D"]))

let test_var ctxt =
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   A "[\"A\"]";
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   (B 42) "[\"B\", 42]";
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   (C (42, "foo")) "[\"C\", 42, \"foo\"]"

let test_rec ctxt =
  assert_roundtrip pp_r r_to_yojson r_of_yojson
                   {x=42; y="foo"} "{\"x\":42,\"y\":\"foo\"}"

type geo = {
  lat : float [@key "Latitude"]  ;
  lon : float [@key "Longitude"] ;
}
[@@deriving yojson, show]
let test_key ctxt =
  assert_roundtrip pp_geo geo_to_yojson geo_of_yojson
                   {lat=35.6895; lon=139.6917}
                   "{\"Latitude\":35.6895,\"Longitude\":139.6917}"

let test_field_err ctxt =
  assert_equal ~printer:(show_error_or pp_geo)
               (Result.Error "Test_ppx_yojson.geo.lat")
               (geo_of_yojson (`Assoc ["Longitude", (`Float 42.0)]))

type id = Yojson.Safe.json [@@deriving yojson]
let test_id ctxt =
  assert_roundtrip pp_json id_to_yojson id_of_yojson
                   (`Int 42) "42"

type custvar =
| Tea   of string [@name "tea"]
| Vodka [@name "vodka"]
[@@deriving yojson, show]
let test_custvar ctxt =
  assert_roundtrip pp_custvar custvar_to_yojson custvar_of_yojson
                   (Tea "oolong") "[\"tea\", \"oolong\"]";
  assert_roundtrip pp_custvar custvar_to_yojson custvar_of_yojson
                   Vodka "[\"vodka\"]"

type custpvar =
[ `Tea   of string [@name "tea"]
| `Beer  of string * float [@name "beer"]
| `Vodka [@name "vodka"]
] [@@deriving yojson, show]
let test_custpvar ctxt =
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   (`Tea "earl_grey") "[\"tea\", \"earl_grey\"]";
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   (`Beer ("guinness", 3.3)) "[\"beer\", \"guinness\", 3.3]";
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   `Vodka "[\"vodka\"]"

type default = {
  def : int [@default 42];
} [@@deriving yojson, show]
let test_default ctxt =
  assert_roundtrip pp_default default_to_yojson default_of_yojson
                   { def = 42 } "{}"

type bidi = int [@@deriving show, to_yojson, of_yojson]
let test_bidi ctxt =
  assert_roundtrip pp_bidi bidi_to_yojson bidi_of_yojson
                   42 "42"

let test_shortcut ctxt =
  assert_roundtrip pp_i1 [%to_yojson: int] [%of_yojson: int]
                   42 "42"

type nostrict = {
  nostrict_field : int;
}
[@@deriving show, yojson { strict = false }]
let test_nostrict ctxt =
  assert_equal ~printer:(show_error_or pp_nostrict)
               (Result.Ok { nostrict_field = 42 })
               (nostrict_of_yojson (`Assoc ["nostrict_field", (`Int 42);
                                            "some_other_field", (`Int 43)]))

module Opentype :
  sig
    type 'a opentype = .. [@@deriving yojson]
    type 'a opentype += A of 'a | B of string list [@@deriving yojson]
  end =
  struct
    type 'a opentype = .. [@@deriving yojson]
    type 'a opentype += A of 'a | B of string list [@@deriving yojson]
  end
type 'a Opentype.opentype +=
  | C of 'a Opentype.opentype * float
  | A = Opentype.A
   [@@deriving yojson]
let rec pp_opentype f fmt = function
  A x -> Format.fprintf fmt "A(%s)" (f x)
| Opentype.B l -> Format.fprintf fmt "B(%s)" (String.concat ", " l)
| C (x, v) ->
    Format.pp_print_string fmt "C(";
    pp_opentype f fmt x;
    Format.fprintf fmt ", %f)" v
| _ -> assert false

let test_opentype ctxt =
  let pp_ot = pp_opentype string_of_int in
  let to_yojson = Opentype.opentype_to_yojson i1_to_yojson in
  let of_yojson = Opentype.opentype_of_yojson i1_of_yojson in
  assert_roundtrip pp_ot to_yojson of_yojson
                   (Opentype.A 0) "[\"A\", 0]";
  assert_roundtrip pp_ot to_yojson of_yojson
                   (Opentype.B ["one"; "two"]) "[\"B\", [ \"one\", \"two\"] ]";
  assert_roundtrip pp_ot to_yojson of_yojson
                   (C (Opentype.A 42, 1.2)) "[\"C\", [\"A\", 42], 1.2]"

module Warnings = struct

  module W34 = struct

    [@@@ocaml.warning "@34"]


    module M1 : sig type u [@@deriving yojson] end = struct
      type internal = int list [@@deriving yojson]
      type u = int list    [@@deriving yojson]
    end
(* the deriver for type [u] supposedly use the derivier of type
       [internal]. Consider for instance the case where [u] is a map,
       and internal is a list of bindings. *)
    module M2 : sig type 'a u [@@deriving yojson] end = struct
      type 'a internal = 'a list [@@deriving yojson]
      type 'a u = 'a list    [@@deriving yojson]
    end

    (* the deriver for type [u] supposedly use the derivier of type
       [internal]. Consider for instance the case where [u] is a map,
       and internal is a list of bindings.  *)
    (* module M1 : sig type 'a u [@@deriving yojson] end = struct *)
    (*   type 'a internal = .. [@@deriving yojson] (\* Triggers the warning *\) *)
    (*   type 'a internal += A of 'a | B of string list  [@@deriving yojson] *)
    (*   type 'a u = 'a list    [@@deriving yojson] *)
    (* end *)
end

end


module TestShadowing = struct
  module List = struct
    let map () = ()
  end

  type t = int list [@@deriving yojson]

  module Array = struct
    let to_list () = ()
  end

  module Bytes = struct
    let to_string () = ()
  end

  type v = bytes [@@deriving yojson]

end


module Test_recursive_polyvariant = struct
  (* Regression test for
     https://github.com/whitequark/ppx_deriving_yojson/issues/24 *)
  type a = [ `B of string ]
      [@@deriving of_yojson]
  type b = [a | `C of b list]
      [@@deriving of_yojson]
  type c = [ a | b | `D of b list]
      [@@deriving of_yojson]
  let c_of_yojson yj : c Ppx_deriving_yojson_runtime.error_or = c_of_yojson yj
end

type 'a recursive1 = { lhs : string ; rhs : 'a }
 and foo = unit recursive1
 and bar = int recursive1
               [@@deriving show, yojson]

let test_recursive ctxt =
  assert_roundtrip (pp_recursive1 pp_i1)
                   (recursive1_to_yojson i1_to_yojson)
                   (recursive1_of_yojson i1_of_yojson)
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}";

  assert_roundtrip pp_foo foo_to_yojson foo_of_yojson
                   {lhs="x"; rhs=()} "{\"lhs\":\"x\",\"rhs\":null}" ;

  assert_roundtrip pp_bar bar_to_yojson bar_of_yojson
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}"

let suite = "Test ppx_yojson" >::: [
    "test_unit"      >:: test_unit;
    "test_int"       >:: test_int;
    "test_int_edge"  >:: test_int_edge;
    "test_float"     >:: test_float;
    "test_bool"      >:: test_bool;
    "test_char"      >:: test_char;
    "test_string"    >:: test_string;
    "test_ref"       >:: test_ref;
    "test_option"    >:: test_option;
    "test_list"      >:: test_list;
    "test_array"     >:: test_array;
    "test_tuple"     >:: test_tuple;
    "test_ptyp"      >:: test_ptyp;
    "test_pvar"      >:: test_pvar;
    "test_var"       >:: test_var;
    "test_rec"       >:: test_rec;
    "test_key"       >:: test_key;
    "test_id"        >:: test_id;
    "test_custvar"   >:: test_custvar;
    "test_custpvar"  >:: test_custpvar;
    "test_field_err" >:: test_field_err;
    "test_default"   >:: test_default;
    "test_bidi"      >:: test_bidi;
    "test_shortcut"  >:: test_shortcut;
    "test_nostrict"  >:: test_nostrict;
    "test_opentype"  >:: test_opentype;
    "test_recursive" >:: test_recursive;
  ]

let _ =
  run_test_tt_main suite
