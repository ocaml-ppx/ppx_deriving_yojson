open OUnit2

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
  | `Variant of string * json option ]
  [@@deriving show]

let show_error_or =
  let module M = struct
    type 'a error_or = ('a, string) Result.result [@@deriving show]
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
type rv = RA | RB of int | RC of int * string | RD of { z : string }
[@@deriving show, yojson]

let test_unit _ctxt =
  assert_roundtrip pp_u u_to_yojson u_of_yojson
    () "null"

let test_int _ctxt =
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

let test_int_edge _ctxt =
  assert_roundtrip pp_i2 i2_to_yojson i2_of_yojson
                   0x7fffffffl "2147483647";
  assert_roundtrip pp_i2 i2_to_yojson i2_of_yojson
                   (Int32.neg 0x80000000l) "-2147483648";
  assert_roundtrip pp_i4 i4_to_yojson i4_of_yojson
                   0x7fffffffffffffffL "9223372036854775807";
  assert_roundtrip pp_i4 i4_to_yojson i4_of_yojson
                   (Int64.neg 0x8000000000000000L) "-9223372036854775808"

let test_float _ctxt =
  assert_roundtrip pp_f f_to_yojson f_of_yojson
                   1.0 "1.0";
  assert_equal ~printer:(show_error_or pp_f)
               (Result.Ok 1.0)
               (f_of_yojson (`Int 1))

let test_bool _ctxt =
  assert_roundtrip pp_b b_to_yojson b_of_yojson
                   true "true";
  assert_roundtrip pp_b b_to_yojson b_of_yojson
                   false "false"

let test_char _ctxt =
  assert_roundtrip pp_c c_to_yojson c_of_yojson
                   'c' "\"c\"";
  assert_failure   pp_c c_of_yojson
                   "Test_ppx_yojson.c" "\"xxx\""

let test_string _ctxt =
  assert_roundtrip pp_s s_to_yojson s_of_yojson
                   "foo" "\"foo\"";
  assert_roundtrip pp_y y_to_yojson y_of_yojson
                   (Bytes.of_string "foo") "\"foo\""

let test_ref _ctxt =
  assert_roundtrip pp_xr xr_to_yojson xr_of_yojson
                   (ref 42) "42"

let test_option _ctxt =
  assert_roundtrip pp_xo xo_to_yojson xo_of_yojson
                   (Some 42) "42";
  assert_roundtrip pp_xo xo_to_yojson xo_of_yojson
                   None "null"

let test_poly _ctxt =
  assert_roundtrip pp_xo
    (([%to_yojson: 'a option] [%to_yojson: int]))
    (([%of_yojson: 'a option] [%of_yojson: int]))
                   (Some 42) "42"

let test_list _ctxt =
  assert_roundtrip pp_xl xl_to_yojson xl_of_yojson
                   [] "[]";
  assert_roundtrip pp_xl xl_to_yojson xl_of_yojson
                   [42; 43] "[42, 43]";
  let rec make_list i acc =
            if i = 0
            then (i mod 100 :: acc)
            else make_list (i - 1) (i mod 100 :: acc) in
  let lst =  make_list (500_000 - 1) [] in
  let buf = Buffer.create (5_000 * 390 + 4) in
  Buffer.add_string buf "[";
  Buffer.add_string buf (string_of_int (List.hd lst));
  List.iter (fun x -> Buffer.add_string buf ", "; Buffer.add_string buf (string_of_int x)) (List.tl lst);
  Buffer.add_string buf "]";
  let str = Bytes.to_string (Buffer.to_bytes buf) in
  assert_roundtrip pp_xl xl_to_yojson xl_of_yojson lst str

let test_array _ctxt =
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [||] "[]";
  assert_roundtrip pp_xa xa_to_yojson xa_of_yojson
                   [|42; 43|] "[42, 43]"

let test_tuple _ctxt =
  assert_roundtrip pp_xt xt_to_yojson xt_of_yojson
                   (42, 43) "[42, 43]"

let test_ptyp _ctxt =
  assert_roundtrip (pp_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   (Some 42) "42";
  assert_roundtrip (pp_p pp_i1) (p_to_yojson i1_to_yojson) (p_of_yojson i1_of_yojson)
                   None "null"

let test_pvar _ctxt =
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

let test_var _ctxt =
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   A "[\"A\"]";
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   (B 42) "[\"B\", 42]";
  assert_roundtrip pp_v v_to_yojson v_of_yojson
                   (C (42, "foo")) "[\"C\", 42, \"foo\"]"

let test_rec _ctxt =
  assert_roundtrip pp_r r_to_yojson r_of_yojson
                   {x=42; y="foo"} "{\"x\":42,\"y\":\"foo\"}"

let test_recvar _ctxt =
  assert_roundtrip pp_rv rv_to_yojson rv_of_yojson
                   RA "[\"RA\"]";
  assert_roundtrip pp_rv rv_to_yojson rv_of_yojson
                   (RB 42) "[\"RB\", 42]";
  assert_roundtrip pp_rv rv_to_yojson rv_of_yojson
                   (RC(42, "foo")) "[\"RC\", 42, \"foo\"]";
  assert_roundtrip pp_rv rv_to_yojson rv_of_yojson
                   (RD{z="foo"}) "[\"RD\", {\"z\": \"foo\"}]"

type geo = {
  lat : float [@key "Latitude"]  ;
  lon : float [@key "Longitude"] ;
}
[@@deriving yojson, show]
let test_key _ctxt =
  assert_roundtrip pp_geo geo_to_yojson geo_of_yojson
                   {lat=35.6895; lon=139.6917}
                   "{\"Latitude\":35.6895,\"Longitude\":139.6917}"

let test_field_err _ctxt =
  assert_equal ~printer:(show_error_or pp_geo)
               (Result.Error "Test_ppx_yojson.geo.lat")
               (geo_of_yojson (`Assoc ["Longitude", (`Float 42.0)]))

type id = Yojson.Safe.t [@@deriving yojson]
let test_id _ctxt =
  assert_roundtrip pp_json id_to_yojson id_of_yojson
                   (`Int 42) "42"

type custvar =
| Tea   of string [@name "tea"]
| Vodka [@name "vodka"]
[@@deriving yojson, show]
let test_custvar _ctxt =
  assert_roundtrip pp_custvar custvar_to_yojson custvar_of_yojson
                   (Tea "oolong") "[\"tea\", \"oolong\"]";
  assert_roundtrip pp_custvar custvar_to_yojson custvar_of_yojson
                   Vodka "[\"vodka\"]"

type custpvar =
[ `Tea   of string [@name "tea"]
| `Beer  of string * float [@name "beer"]
| `Vodka [@name "vodka"]
] [@@deriving yojson, show]
let test_custpvar _ctxt =
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   (`Tea "earl_grey") "[\"tea\", \"earl_grey\"]";
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   (`Beer ("guinness", 3.3)) "[\"beer\", \"guinness\", 3.3]";
  assert_roundtrip pp_custpvar custpvar_to_yojson custpvar_of_yojson
                   `Vodka "[\"vodka\"]"

type default = {
  def : int [@default 42];
} [@@deriving yojson, show]
let test_default _ctxt =
  assert_roundtrip pp_default default_to_yojson default_of_yojson
                   { def = 42 } "{}"

type bidi = int [@@deriving show, to_yojson, of_yojson]
let test_bidi _ctxt =
  assert_roundtrip pp_bidi bidi_to_yojson bidi_of_yojson
                   42 "42"

let test_shortcut _ctxt =
  assert_roundtrip pp_i1 [%to_yojson: int] [%of_yojson: int]
                   42 "42"

module CustomConversions = struct

  module IntMap = Map.Make(struct type t = int let compare = compare end)
  type mapEncoding = (int * string) list [@@deriving yojson]
  let map_to_yojson m = mapEncoding_to_yojson @@ IntMap.bindings m 
  let map_of_yojson json = 
    Result.(match mapEncoding_of_yojson json with
              | Ok lst -> Ok (List.fold_left (fun m (k, v) -> IntMap.add k v m) IntMap.empty lst)
              | Error s -> Error s)

  type k = string IntMap.t [@to_yojson map_to_yojson]
                           [@of_yojson map_of_yojson]
                           [@printer fun fmt a -> ()]
                           [@@deriving show, yojson]
  let test_bare _ctxt =
    assert_roundtrip pp_k k_to_yojson k_of_yojson
                     IntMap.(add 6 "foo" @@ empty)
                     {|[[6,"foo"]]|}

  type crecord = {
    mapping : string IntMap.t [@to_yojson map_to_yojson]
                              [@of_yojson map_of_yojson]
                              [@printer fun fmt a -> ()]
  } [@@deriving yojson, show]

  let test_record _ctxt =
    assert_roundtrip pp_crecord crecord_to_yojson crecord_of_yojson
                     IntMap.{ mapping = add 6 "foo" @@ empty }
                     {|{"mapping":[[6,"foo"]]}|}
  
  let suite = "Custom conversion attributes" >:::
    [ "test_record"      >:: test_record
    ; "test_bare"        >:: test_bare ]
end

type nostrict = {
  nostrict_field : int;
}
[@@deriving show, yojson { strict = false }]
let test_nostrict _ctxt =
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

let test_opentype _ctxt =
  let pp_ot = pp_opentype string_of_int in
  let to_yojson = Opentype.opentype_to_yojson i1_to_yojson in
  let of_yojson = Opentype.opentype_of_yojson i1_of_yojson in
  assert_roundtrip pp_ot to_yojson of_yojson
                   (Opentype.A 0) "[\"A\", 0]";
  assert_roundtrip pp_ot to_yojson of_yojson
                   (Opentype.B ["one"; "two"]) "[\"B\", [ \"one\", \"two\"] ]";
  assert_roundtrip pp_ot to_yojson of_yojson
                   (C (Opentype.A 42, 1.2)) "[\"C\", [\"A\", 42], 1.2]"


(* This will fail at type-check if we introduce features that increase
   the default generated signatures. It is representative of user code
   (there is plenty in OPAM) that uses our generated signatures, but
   manually implement this restricted function set.

   For example, the unconditional addition of of_yojson_exn has broken
   this test. *)
type outer_t = int [@@deriving yojson]
module Automatic_deriving_in_signature_only
  : sig type t [@@deriving yojson] end
  = struct
    type t = int
    let of_yojson = outer_t_of_yojson
    let to_yojson = outer_t_to_yojson
  end

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

module Test_extension_forms = struct
  let _ = [%to_yojson: unit], [%of_yojson: unit]
  let _ = [%to_yojson: int], [%of_yojson: int]
  let _ = [%to_yojson: int32], [%of_yojson: int32]
  let _ = [%to_yojson: Int32.t], [%of_yojson: Int32.t]
  let _ = [%to_yojson: int64], [%of_yojson: int64]
  let _ = [%to_yojson: Int64.t], [%of_yojson: Int64.t]
  let _ = [%to_yojson: nativeint], [%of_yojson: nativeint]
  let _ = [%to_yojson: Nativeint.t], [%of_yojson: Nativeint.t]
  let _ = [%to_yojson: int64], [%of_yojson: int64]
  let _ = [%to_yojson: nativeint], [%of_yojson: nativeint]
  let _ = [%to_yojson: float], [%of_yojson: float]
  let _ = [%to_yojson: bool], [%of_yojson: bool]
  let _ = [%to_yojson: char], [%of_yojson: char]
  let _ = [%to_yojson: string], [%of_yojson: string]
  let _ = [%to_yojson: bytes], [%of_yojson: bytes]
  let _ = [%to_yojson: int], [%of_yojson: int]
  let _ = [%to_yojson: int ref], [%of_yojson: int ref]
  let _ = [%to_yojson: int option], [%of_yojson: int option]
  let _ = [%to_yojson: int list], [%of_yojson: int list]
  let _ = [%to_yojson: int array], [%of_yojson: int array]
  let _ = [%to_yojson: int * int], [%of_yojson: int * int]

  let _ =  [%to_yojson: 'a option],
          [%of_yojson: 'a option]
  let _ = [%to_yojson: [ `A | `B of int | `C of int * string ]],
          [%of_yojson: [ `A | `B of int | `C of int * string ]]
  let _ = [%to_yojson: [ `C of 'a ]],
          [%of_yojson: [ `C of 'a ]]
  let _ = [%to_yojson: [ pva | pvb | int pvc ]],
          [%of_yojson: [ pva | pvb | int pvc ]]
end

(* this test checks that we can derive an _exn deserializer
   even if we use sub-types that are derived with {exn = false} *)
module Test_exn_depends_on_non_exn = struct
  module M : sig
    type t [@@deriving yojson { exn = false }]
  end = struct
    type t = int [@@deriving yojson { exn = false }]
  end
  open M
  type u = t * t [@@deriving yojson { exn = true }]
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

let test_recursive _ctxt =
  assert_roundtrip (pp_recursive1 pp_i1)
                   (recursive1_to_yojson i1_to_yojson)
                   (recursive1_of_yojson i1_of_yojson)
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}";

  assert_roundtrip pp_foo foo_to_yojson foo_of_yojson
                   {lhs="x"; rhs=()} "{\"lhs\":\"x\",\"rhs\":null}" ;

  assert_roundtrip pp_bar bar_to_yojson bar_of_yojson
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}"

let test_int_redefined ctxt =
  let module M = struct
    type int = Break_things

    let x = [%to_yojson: int] 1
  end
  in
  let expected = `Int 1 in
  assert_equal ~ctxt ~printer:show_json expected M.x

(* TODO: Make this work *)
(*
let test_list_redefined ctxt =
  let module M = struct
    type redef_list =
      | []
      | (::) of int * int

    type t = {field : int list} [@@deriving to_yojson]
    let x = {field = List.([1;2])}
  end
  in
  let expected = `List [`Int 1; `Int 2] in
  assert_equal ~ctxt ~printer:show_json expected M.x
*)

let test_equality_redefined ctxt =
  let module M = struct
    module Pervasives = struct
      let (=) : int -> int -> bool = fun a b -> a = b
      let _ = 1 = 1 (* just dummy usage of `=` to suppress compiler warning *)

      let never_gonna_be_in_pervasives = None
    end
    let (=) : int -> int -> bool = fun a b -> a = b
    let _ = 1 = 1 (* just dummy usage of `=` to suppress compiler warning *)

    type t = {field : int option [@default Pervasives.never_gonna_be_in_pervasives]} [@@deriving to_yojson]
    let x = {field = Some 42}
  end
  in
  let expected = `Assoc ([("field", `Int (42))]) in
  assert_equal ~ctxt ~printer:show_json expected M.(to_yojson x)

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
    "test_poly"      >:: test_poly;
    "test_list"      >:: test_list;
    "test_array"     >:: test_array;
    "test_tuple"     >:: test_tuple;
    "test_ptyp"      >:: test_ptyp;
    "test_pvar"      >:: test_pvar;
    "test_var"       >:: test_var;
    "test_rec"       >:: test_rec;
    "test_recvar"    >:: test_recvar;
    "test_key"       >:: test_key;
    "test_id"        >:: test_id;
    "test_custvar"   >:: test_custvar;
    "test_custpvar"  >:: test_custpvar;
    "test_field_err" >:: test_field_err;
    "test_default"   >:: test_default;
    "test_bidi"      >:: test_bidi;
    "test_custom"    >:  CustomConversions.suite;
    "test_shortcut"  >:: test_shortcut;
    "test_nostrict"  >:: test_nostrict;
    "test_opentype"  >:: test_opentype;
    "test_recursive" >:: test_recursive;
    "test_int_redefined" >:: test_int_redefined;
    "test_equality_redefined" >:: test_equality_redefined;
  ]

let _ =
  run_test_tt_main suite
