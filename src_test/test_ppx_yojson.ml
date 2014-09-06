open OUnit2

type json = [%import: Yojson.Safe.json] [@@deriving Show]

let assert_roundtrip show_obj to_json from_json obj str =
  let json = Yojson.Safe.from_string str in
  let cleanup json = Yojson.Safe.(json |> to_string |> from_string) in
  assert_equal ~printer:show_json json (cleanup (to_json obj));
  (* assert_equal ~printer:show_obj obj (from_json json) *)

type i1 = int         [@@deriving Show, Yojson]
type i2 = int32       [@@deriving Show, Yojson]
type i3 = Int32.t     [@@deriving Show, Yojson]
type i4 = int64       [@@deriving Show, Yojson]
type i5 = Int64.t     [@@deriving Show, Yojson]
type i6 = nativeint   [@@deriving Show, Yojson]
type i7 = Nativeint.t [@@deriving Show, Yojson]
type f  = float       [@@deriving Show, Yojson]
type b  = bool        [@@deriving Show, Yojson]
type s  = string      [@@deriving Show, Yojson]
type y  = bytes       [@@deriving Show, Yojson]
type xo = int option  [@@deriving Show, Yojson]
type xl = int list    [@@deriving Show, Yojson]
type xa = int array   [@@deriving Show, Yojson]
type xr = int ref     [@@deriving Show, Yojson]
type xt = int * int   [@@deriving Show, Yojson]

type 'a p  = 'a option          [@@deriving Show, Yojson]
type    pv = [ `A | `B of int ] [@@deriving Show, Yojson]

type    v  = A | B of int * string   [@@deriving Show, Yojson]
type    r  = { x : int; y : string } [@@deriving Show, Yojson]

let test_int ctxt =
  assert_roundtrip show_i1 i1_to_yojson i1_of_yojson
                   42 "42";
  assert_roundtrip show_i2 i2_to_yojson i2_of_yojson
                   42l "42";
  assert_roundtrip show_i3 i3_to_yojson i3_of_yojson
                   42l "42";
  assert_roundtrip show_i4 i4_to_yojson i4_of_yojson
                   42L "42";
  assert_roundtrip show_i5 i5_to_yojson i5_of_yojson
                   42L "42";
  assert_roundtrip show_i6 i6_to_yojson i6_of_yojson
                   42n "42";
  assert_roundtrip show_i7 i7_to_yojson i7_of_yojson
                   42n "42"

let test_float ctxt =
  assert_roundtrip show_f f_to_yojson f_of_yojson
                   1.0 "1.0"

let test_string ctxt =
  assert_roundtrip show_s s_to_yojson s_of_yojson
                   "foo" "\"foo\"";
  assert_roundtrip show_y y_to_yojson y_of_yojson
                   (Bytes.of_string "foo") "\"foo\""

let test_option ctxt =
  assert_roundtrip show_xo xo_to_yojson xo_of_yojson
                   (Some 42) "42";
  assert_roundtrip show_xo xo_to_yojson xo_of_yojson
                   None "null"

let test_list ctxt =
  assert_roundtrip show_xl xl_to_yojson xl_of_yojson
                   [] "[]";
  assert_roundtrip show_xl xl_to_yojson xl_of_yojson
                   [42] "[42]"

let test_array ctxt =
  assert_roundtrip show_xa xa_to_yojson xa_of_yojson
                   [||] "[]";
  assert_roundtrip show_xa xa_to_yojson xa_of_yojson
                   [|42|] "[42]"

let test_ref ctxt =
  assert_roundtrip show_xr xr_to_yojson xr_of_yojson
                   (ref 42) "42"

let test_tuple ctxt =
  assert_roundtrip show_xt xt_to_yojson xt_of_yojson
                   (42, 43) "[42, 43]"

let test_ptyp ctxt =
  assert_roundtrip (show_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   (Some 42) "42";
  assert_roundtrip (show_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   None "null"

let test_pvar ctxt =
  assert_roundtrip show_pv pv_to_yojson pv_of_yojson
                   `A "[\"A\"]";
  assert_roundtrip show_pv pv_to_yojson pv_of_yojson
                   (`B 42) "[\"B\", 42]"

let test_var ctxt =
  assert_roundtrip show_v v_to_yojson v_of_yojson
                   A "[\"A\"]";
  assert_roundtrip show_v v_to_yojson v_of_yojson
                   (B (42, "foo")) "[\"B\", 42, \"foo\"]"

let test_rec ctxt =
  assert_roundtrip show_r r_to_yojson r_of_yojson
                   {x=42; y="foo"} "{\"x\":42,\"y\":\"foo\"}"

let suite = "Test ppx_yojson" >::: [
    "test_int"    >:: test_int;
    "test_float"  >:: test_float;
    "test_string" >:: test_string;
    "test_option" >:: test_option;
    "test_list"   >:: test_list;
    "test_array"  >:: test_array;
    "test_ref"    >:: test_ref;
    "test_ptyp"   >:: test_ptyp;
    "test_pvar"   >:: test_pvar;
    "test_var"    >:: test_var;
    "test_rec"    >:: test_rec;
  ]

let _ =
  run_test_tt_main suite
