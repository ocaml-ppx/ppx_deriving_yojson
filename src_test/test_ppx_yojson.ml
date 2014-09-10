open OUnit2

type json = [%import: Yojson.Safe.json] [@@deriving Show]
type 'a error_or = [ `Ok of 'a | `Error of string ] [@@deriving Show]

let assert_roundtrip pp_obj to_json of_json obj str =
  let json = Yojson.Safe.from_string str in
  let cleanup json = Yojson.Safe.(json |> to_string |> from_string) in
  assert_equal ~printer:show_json json (cleanup (to_json obj));
  assert_equal ~printer:(show_error_or pp_obj) (`Ok obj) (of_json json)

let assert_failure pp_obj of_json err str =
  let json = Yojson.Safe.from_string str in
  assert_equal ~printer:(show_error_or pp_obj) (`Error err) (of_json json)

type i1 = int         [@@deriving Show, Yojson]
type i2 = int32       [@@deriving Show, Yojson]
type i3 = Int32.t     [@@deriving Show, Yojson]
type i4 = int64       [@@deriving Show, Yojson]
type i5 = Int64.t     [@@deriving Show, Yojson]
type i6 = nativeint   [@@deriving Show, Yojson]
type i7 = Nativeint.t [@@deriving Show, Yojson]
type i8 = int64       [@encoding `string] [@@deriving Show, Yojson]
type i9 = nativeint   [@encoding `string] [@@deriving Show, Yojson]
type f  = float       [@@deriving Show, Yojson]
type b  = bool        [@@deriving Show, Yojson]
type c  = char        [@@deriving Show, Yojson]
type s  = string      [@@deriving Show, Yojson]
type y  = bytes       [@@deriving Show, Yojson]
type xr = int ref     [@@deriving Show, Yojson]
type xo = int option  [@@deriving Show, Yojson]
type xl = int list    [@@deriving Show, Yojson]
type xa = int array   [@@deriving Show, Yojson]
type xt = int * int   [@@deriving Show, Yojson]

type 'a p = 'a option
[@@deriving Show, Yojson]
type pv = [ `A | `B of int | `C of int * string ]
[@@deriving Show, Yojson]

type v  = A | B of int | C of int * string
[@@deriving Show, Yojson]
type r  = { x : int; y : string }
[@@deriving Show, Yojson]

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
let test_float ctxt =
  assert_roundtrip pp_f f_to_yojson f_of_yojson
                   1.0 "1.0"

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
                   [42] "[42]"

let test_array ctxt =
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [||] "[]";
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [|42|] "[42]"

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
                   (`C (42, "foo")) "[\"C\", 42, \"foo\"]"

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
  lat [@key "Latitude"]  : float;
  lon [@key "Longitude"] : float;
}
[@@deriving Yojson, Show]
let test_key ctxt =
  assert_roundtrip pp_geo geo_to_yojson geo_of_yojson
                   {lat=35.6895; lon=139.6917}
                   "{\"Latitude\":35.6895,\"Longitude\":139.6917}"

let suite = "Test ppx_yojson" >::: [
    "test_int"    >:: test_int;
    "test_float"  >:: test_float;
    "test_bool"   >:: test_bool;
    "test_char"   >:: test_char;
    "test_string" >:: test_string;
    "test_ref"    >:: test_ref;
    "test_option" >:: test_option;
    "test_list"   >:: test_list;
    "test_array"  >:: test_array;
    "test_tuple"  >:: test_tuple;
    "test_ptyp"   >:: test_ptyp;
    "test_pvar"   >:: test_pvar;
    "test_var"    >:: test_var;
    "test_rec"    >:: test_rec;
    "test_key"    >:: test_key;
  ]

let _ =
  run_test_tt_main suite
