#if OCAML_VERSION < (4, 03, 0)
#define Type_Nonrecursive
#define Pconst_string Const_string
#define Pcstr_tuple(x) x
#else
#define Type_Nonrecursive Nonrecursive
#endif

open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "yojson"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let attr_int_encoding attrs =
  match Ppx_deriving.attr ~deriver "encoding" attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver (enum ["string"; "number"])) with
  | Some "string" -> `String
  | Some "number" | None -> `Int
  | _ -> assert false

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> x
  | None   -> default

let attr_key  = attr_string "key"
let attr_name = attr_string "name"

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver expr)

let parse_options options =
  let strict = ref true in
  options |> List.iter (fun (name, expr) ->
    match name with
    | "strict" -> strict := Ppx_deriving.Arg.(get_expr ~deriver bool) expr
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name);
  !strict

let rec ser_expr_of_typ typ =
  let attr_int_encoding typ =
    match attr_int_encoding typ with `String -> "String" | `Int -> "Intlit"
  in
  match typ with
  | [%type: unit]            -> [%expr fun x -> `Null]
  | [%type: int]             -> [%expr fun x -> `Int x]
  | [%type: float]           -> [%expr fun x -> `Float x]
  | [%type: bool]            -> [%expr fun x -> `Bool x]
  | [%type: string]          -> [%expr fun x -> `String x]
  | [%type: bytes]           -> [%expr fun x -> `String (Bytes.to_string x)]
  | [%type: char]            -> [%expr fun x -> `String (String.make 1 x)]
  | [%type: [%t? typ] ref]   -> [%expr fun x -> [%e ser_expr_of_typ typ] !x]
  | [%type: [%t? typ] list]  -> [%expr fun x -> `List (List.map [%e ser_expr_of_typ typ] x)]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr fun x -> `Intlit (Int32.to_string x)]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr fun x -> [%e Exp.variant (attr_int_encoding typ.ptyp_attributes)
                                    (Some [%expr (Int64.to_string x)])]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr fun x -> [%e Exp.variant (attr_int_encoding typ.ptyp_attributes)
                                    (Some [%expr (Nativeint.to_string x)])]]
  | [%type: [%t? typ] array] ->
    [%expr fun x -> `List (Array.to_list (Array.map [%e ser_expr_of_typ typ] x))]
  | [%type: [%t? typ] option] ->
    [%expr function None -> `Null | Some x -> [%e ser_expr_of_typ typ] x]
  | [%type: Yojson.Safe.json] -> [%expr fun x -> x]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
    let fwd = app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "to_yojson") lid)))
        (List.map ser_expr_of_typ args)
    in
    (* eta-expansion is necessary for let-rec *)
    [%expr fun x -> [%e fwd] x]

  | { ptyp_desc = Ptyp_tuple typs } ->
    [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
           `List ([%e
                      list (List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) typs)])];
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let cases =
      fields |> List.map (fun field ->
        match field with
        | Rtag (label, attrs, true (*empty*), []) ->
          Exp.case (Pat.variant label None)
                   [%expr `List [`String [%e str (attr_name label attrs)]]]
        | Rtag (label, attrs, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
          Exp.case (Pat.variant label (Some (ptuple (List.mapi (fun i _ -> pvar (argn i)) typs))))
                   [%expr `List ((`String [%e str (attr_name label attrs)]) :: [%e
                      list (List.mapi
                        (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) typs)])]
        | Rtag (label, attrs, false, [typ]) ->
          Exp.case (Pat.variant label (Some [%pat? x]))
                   [%expr `List [`String [%e str (attr_name label attrs)];
                                 [%e ser_expr_of_typ typ] x]]
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e ser_expr_of_typ typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                       deriver (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> [%expr ([%e evar ("poly_"^name)] : _ -> Yojson.Safe.json)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name)] x; [%e ser_expr_of_typ typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

(* http://desuchan.net/desu/src/1284751839295.jpg *)
let rec desu_fold ~path f typs =
  typs |>
  List.mapi (fun i typ -> i, app (desu_expr_of_typ ~path typ) [evar (argn i)]) |>
  List.fold_left (fun x (i, y) ->
    [%expr [%e y] >>= fun [%p pvar (argn i)] -> [%e x]])
    [%expr Result.Ok [%e f (List.mapi (fun i _ -> evar (argn i)) typs)]]
and desu_expr_of_typ ~path typ =
  let error = [%expr Result.Error [%e str (String.concat "." path)]] in
  let decode' cases =
    Exp.function_ (
      List.map (fun (pat, exp) -> Exp.case pat exp) cases @
      [Exp.case [%pat? _] error])
  in
  let decode pat exp = decode' [pat, exp] in
  match typ with
  | [%type: unit]   -> decode [%pat? `Null] [%expr Result.Ok ()]
  | [%type: int]    -> decode [%pat? `Int x]    [%expr Result.Ok x]
  | [%type: float]  ->
    decode' [[%pat? `Int x],    [%expr Result.Ok (float_of_int x)];
             [%pat? `Intlit x], [%expr Result.Ok (float_of_string x)];
             [%pat? `Float x],  [%expr Result.Ok x]]
  | [%type: bool]   -> decode [%pat? `Bool x]   [%expr Result.Ok x]
  | [%type: string] -> decode [%pat? `String x] [%expr Result.Ok x]
  | [%type: bytes]  -> decode [%pat? `String x] [%expr Result.Ok (Bytes.of_string x)]
  | [%type: char]   ->
    decode [%pat? `String x] [%expr if String.length x = 1 then Result.Ok x.[0] else [%e error]]
  | [%type: int32] | [%type: Int32.t] ->
    decode' [[%pat? `Int x],    [%expr Result.Ok (Int32.of_int x)];
             [%pat? `Intlit x], [%expr Result.Ok (Int32.of_string x)]]
  | [%type: int64] | [%type: Int64.t] ->
    begin match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr Result.Ok (Int64.of_string x)]
    | `Int ->
      decode' [[%pat? `Int x],    [%expr Result.Ok (Int64.of_int x)];
               [%pat? `Intlit x], [%expr Result.Ok (Int64.of_string x)]]
    end
  | [%type: nativeint] | [%type: Nativeint.t] ->
    begin match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr Result.Ok (Nativeint.of_string x)]
    | `Int ->
      decode' [[%pat? `Int x],    [%expr Result.Ok (Nativeint.of_int x)];
               [%pat? `Intlit x], [%expr Result.Ok (Nativeint.of_string x)]]
    end
  | [%type: [%t? typ] ref]   ->
    [%expr fun x -> [%e desu_expr_of_typ ~path:(path @ ["contents"]) typ] x >|= ref]
  | [%type: [%t? typ] option] ->
    [%expr function
           | `Null -> Result.Ok None
           | x     -> [%e desu_expr_of_typ ~path typ] x >>= fun x -> Result.Ok (Some x)]
  | [%type: [%t? typ] list]  ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs]
  | [%type: [%t? typ] array] ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs >|= Array.of_list]
  | [%type: Yojson.Safe.json] -> [%expr fun x -> Result.Ok x]
  | { ptyp_desc = Ptyp_tuple typs } ->
    decode [%pat? `List [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)]]
           (desu_fold ~path tuple typs)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let inherits, tags = List.partition (function Rinherit _ -> true | _ -> false) fields in
    let tag_cases = tags |> List.map (fun field ->
      match field with
      | Rtag (label, attrs, true (*empty*), []) ->
        Exp.case [%pat? `List [`String [%p pstr (attr_name label attrs)]]]
                 [%expr Result.Ok [%e Exp.variant label None]]
      | Rtag (label, attrs, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
        Exp.case [%pat? `List ((`String [%p pstr (attr_name label attrs)]) :: [%p
                    plist (List.mapi (fun i _ -> pvar (argn i)) typs)])]
                 (desu_fold ~path (fun x -> (Exp.variant label (Some (tuple x)))) typs)
      | Rtag (label, attrs, false, [typ]) ->
        Exp.case [%pat? `List [`String [%p pstr (attr_name label attrs)]; x]]
                 [%expr [%e desu_expr_of_typ ~path typ] x >>= fun x ->
                        Result.Ok [%e Exp.variant label (Some [%expr x])]]
      | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
        Exp.case [%pat? [%p Pat.type_ tname] as x]
                 [%expr [%e desu_expr_of_typ ~path typ] x]
      | _ ->
        raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                     deriver (Ppx_deriving.string_of_core_type typ))
    and inherits_case =
      let toplevel_typ = typ in
      inherits
      |> List.map (function Rinherit typ -> typ | _ -> assert false)
      |> List.fold_left (fun expr typ -> [%expr
        match [%e desu_expr_of_typ ~path typ] json with
        | (Result.Ok result) -> Result.Ok (result :> [%t toplevel_typ])
        | Result.Error _ -> [%e expr]]) error
      |> Exp.case [%pat? _]
    in
    [%expr fun (json : Yojson.Safe.json) ->
      [%e Exp.match_ [%expr json] (tag_cases @ [inherits_case])]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
     let fwd = app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_yojson") lid)))
             (List.map (desu_expr_of_typ ~path) args) in
     (* eta-expansion is necessary for recursive groups *)
     [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name } ->
    [%expr ([%e evar ("poly_"^name)] : Yojson.Safe.json -> _ error_or)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name)] x; [%e desu_expr_of_typ ~path typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_yojson_runtime") decls

let ser_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
                       (fun var -> [%type: [%t var] -> Yojson.Safe.json]) type_decl in
  polymorphize [%type: [%t typ] -> Yojson.Safe.json]

let ser_str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open -> begin
    let to_yojson_name = Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl in
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "to_yojson")) type_decl
    in
    match type_decl.ptype_manifest with
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = lid }, args) } as manifest) ->
      let ser = ser_expr_of_typ manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "to_yojson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      ([Str.module_ (Mb.mk (mknoloc mod_name) orig_mod)],
       [Vb.mk (pvar to_yojson_name)
              (polymorphize [%expr ([%e ser] : [%t typ] -> Yojson.Safe.json)])])
    | Some _ ->
      raise_errorf ~loc "%s: extensible type manifest should be a type name" deriver
    | None ->
      let poly_vars = List.rev
          (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
      in
      let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: [%t var] -> Yojson.Safe.json]) type_decl
      in
      let ty = Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Yojson.Safe.json]) in
      let default_fun =
        let type_path = String.concat "." (path @ [type_decl.ptype_name.txt]) in
        let e_type_path = Exp.constant (Pconst_string (type_path, None)) in
        [%expr fun _ ->
          invalid_arg ("to_yojson: Maybe a [@@deriving yojson] is missing when extending the type "^
                       [%e e_type_path])]
      in
      let poly_fun = polymorphize default_fun in
      let poly_fun =
        (Ppx_deriving.fold_left_type_decl (fun exp name -> Exp.newtype name exp) poly_fun type_decl)
      in
      let mod_name = "M_"^to_yojson_name in
      let typ = Type.mk ~kind:(Ptype_record [Type.field ~mut:Mutable (mknoloc "f") ty])
                              (mknoloc "t_to_yojson")
      in
      let record = Vb.mk (pvar "f") (Exp.record [lid "f", poly_fun] None) in
      let flid = lid (Printf.sprintf "%s.f" mod_name) in
      let field = Exp.field (Exp.ident flid) (flid) in
      let mod_ =
        Str.module_ (Mb.mk (mknoloc mod_name)
                    (Mod.structure [
          Str.type_ Type_Nonrecursive [typ];
          Str.value Nonrecursive [record];
        ]))
      in
      ([mod_],
       [Vb.mk (pvar to_yojson_name) [%expr fun x -> [%e field] x]])
  end
  | kind ->
    let serializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ -> assert false
      | Ptype_abstract, Some manifest -> ser_expr_of_typ manifest
      | Ptype_variant constrs, _ ->
        constrs
        |> List.map (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
          let json_name = attr_name name' pcd_attributes in
          match pcd_args with
          | Pcstr_tuple([]) ->
            Exp.case
              (pconstr name' [])
              [%expr `List [`String [%e str json_name]]]
          | Pcstr_tuple(args) ->
            let arg_exprs =
              List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) args
            in
            Exp.case
              (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
              [%expr `List ((`String [%e str json_name]) :: [%e list arg_exprs])]
#if OCAML_VERSION >= (4, 03, 0)
          | Pcstr_record _ ->
            raise_errorf ~loc "%s: record variants are not supported" deriver
#endif
          )
        |> Exp.function_
      | Ptype_record labels, _ ->
        let fields =
          labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
            let field  = Exp.field (evar "x") (mknoloc (Lident name)) in
            let result = [%expr [%e str (attr_key name pld_attributes)],
                [%e ser_expr_of_typ pld_type] [%e field]] in
            match attr_default (pld_type.ptyp_attributes @ pld_attributes) with
            | None ->
              [%expr [%e result] :: fields]
            | Some default ->
              [%expr if [%e field] = [%e default] then fields else [%e result] :: fields])
        in
        let assoc =
          List.fold_left (fun expr field -> [%expr let fields = [%e field] in [%e expr]])
            [%expr `Assoc fields] fields
        in
        [%expr fun x -> let fields = [] in [%e assoc]]
      | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    in
    let ty = ser_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var = pvar (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl) in
    ([],
     [Vb.mk (Pat.constraint_ var poly_type)
        (polymorphize [%expr ([%e wrap_runtime serializer])])])

let ser_str_of_type_ext ~options ~path ({ ptyext_path = { loc }} as type_ext) =
  ignore (parse_options options);
  let serializer =
    let pats =
      List.fold_right (fun { pext_name = { txt = name' }; pext_kind; pext_attributes } acc_cases ->
        match pext_kind with
        | Pext_rebind _ ->
          (* nothing to do, since the constructor must be handled in original
             constructor declaration *)
          acc_cases
        | Pext_decl (pext_args, _) ->
          let json_name = attr_name name' pext_attributes in
          let case =
            match pext_args with
            | Pcstr_tuple([]) ->
              Exp.case
                (pconstr name' [])
                [%expr `List [`String [%e str json_name]]]
            | Pcstr_tuple(args) ->
              let arg_exprs =
                List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) args
              in
              Exp.case
                (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
                [%expr `List ((`String [%e str json_name]) :: [%e list arg_exprs])]
#if OCAML_VERSION >= (4, 03, 0)
            | Pcstr_record _ ->
              raise_errorf ~loc "%s: record variants are not supported" deriver
#endif
          in
          case :: acc_cases) type_ext.ptyext_constructors []
    in
    let fallback_case =
      Exp.case [%pat? x]
               [%expr [%e Ppx_deriving.poly_apply_of_type_ext type_ext [%expr fallback]] x]
    in
    Exp.function_ (pats @ [fallback_case])
  in
  let mod_name =
    let mod_lid =
      Ppx_deriving.mangle_lid
        (`PrefixSuffix ("M", "to_yojson")) type_ext.ptyext_path.txt
    in
    String.concat "." (Longident.flatten mod_lid)
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let serializer = polymorphize (wrap_runtime serializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid serializer in
  let field = Exp.field (Exp.ident flid) (flid) in
  let body = [%expr let fallback = [%e field] in [%e set_field]] in
  [Str.value ?loc:None Nonrecursive [Vb.mk (Pat.construct (lid "()") None) body]]

let error_or typ = [%type: [%t typ] Ppx_deriving_yojson_runtime.error_or]

let desu_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
                       (fun var -> [%type: Yojson.Safe.json -> [%t error_or var]]) type_decl in
  polymorphize [%type: Yojson.Safe.json -> [%t error_or typ]]

let desu_str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let is_strict = parse_options options in
  let path = path @ [type_decl.ptype_name.txt] in
  let error path = [%expr Result.Error [%e str (String.concat "." path)]] in
  let top_error = error path in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open -> begin
    let of_yojson_name = Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl in
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "of_yojson")) type_decl
    in
    match type_decl.ptype_manifest with
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = lid }, args) } as manifest) ->
      let desu = desu_expr_of_typ ~path manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "of_yojson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      let poly_desu = polymorphize [%expr ([%e wrap_runtime desu] : Yojson.Safe.json -> _)] in
      ([Str.module_ (Mb.mk (mknoloc mod_name) orig_mod)],
       [Vb.mk (pvar of_yojson_name) poly_desu])
    | Some _ ->
      raise_errorf ~loc "%s: extensible type manifest should be a type name" deriver
    | None ->
      let poly_vars = List.rev
        (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
      in
      let polymorphize_desu = Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: Yojson.Safe.json -> [%t error_or var]]) type_decl in
      let ty = Typ.poly poly_vars
        (polymorphize_desu [%type: Yojson.Safe.json -> [%t error_or typ]])
      in
      let default_fun = Exp.function_ [Exp.case [%pat? _] top_error] in
      let poly_fun = polymorphize default_fun in
      let poly_fun =
        (Ppx_deriving.fold_left_type_decl (fun exp name -> Exp.newtype name exp) poly_fun type_decl)
      in
      let mod_name = "M_"^of_yojson_name in
      let typ = Type.mk ~kind:(Ptype_record [Type.field ~mut:Mutable (mknoloc "f") ty])
                        (mknoloc "t_of_yojson") in
      let record = Vb.mk (pvar "f") (Exp.record [lid "f", poly_fun] None) in
      let flid = lid (Printf.sprintf "%s.f" mod_name) in
      let field = Exp.field (Exp.ident flid) flid in
      let mod_ =
        Str.module_ (Mb.mk (mknoloc mod_name)
                    (Mod.structure [
          Str.type_ Type_Nonrecursive [typ];
          Str.value Nonrecursive [record];
        ]))
      in
      ([mod_],
       [Vb.mk (pvar of_yojson_name) [%expr fun x -> [%e field] x]])
  end
  | kind ->
    let desurializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ -> assert false
      | Ptype_abstract, Some manifest ->
        desu_expr_of_typ ~path manifest
      | Ptype_variant constrs, _ ->
        let cases = List.map (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
          match pcd_args with
          | Pcstr_tuple(args) ->
            Exp.case
              [%pat? `List ((`String [%p pstr (attr_name name' pcd_attributes)]) ::
                                     [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)])]
              (desu_fold ~path (fun x -> constr name' x) args)
#if OCAML_VERSION >= (4, 03, 0)
          | Pcstr_record _ ->
            raise_errorf ~loc "%s: record variants are not supported" deriver
#endif
          ) constrs
        in
        Exp.function_ (cases @ [Exp.case [%pat? _] top_error])
      | Ptype_record labels, _ ->
        let record = List.fold_left (fun expr i ->
          [%expr [%e evar (argn i)] >>= fun [%p pvar (argn i)] -> [%e expr]])
          [%expr Result.Ok [%e Exp.record (labels |> List.mapi (fun i { pld_name = { txt = name } } ->
                  mknoloc (Lident name), evar (argn i))) None]]
          (labels |> List.mapi (fun i _ -> i))
        in
        let default_case = if is_strict then top_error else [%expr loop xs _state] in
        let cases =
          (labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
              let path = path @ [name] in
              let thunks = labels |> List.mapi (fun j _ ->
                   if i = j then app (desu_expr_of_typ ~path pld_type) [evar "x"] else evar (argn j)) in
              Exp.case [%pat? ([%p pstr (attr_key name pld_attributes)], x) :: xs]
                [%expr loop xs [%e tuple thunks]])) @
          [Exp.case [%pat? []] record;
           Exp.case [%pat? _ :: xs] default_case]
        and thunks =
          labels |> List.map (fun { pld_name = { txt = name }; pld_type; pld_attributes } ->
            match attr_default (pld_type.ptyp_attributes @ pld_attributes) with
            | None   -> error (path @ [name])
            | Some x -> [%expr Result.Ok [%e x]])
        in
        [%expr
          function
          | `Assoc xs ->
            let rec loop xs ([%p ptuple (List.mapi (fun i _ -> pvar (argn i)) labels)] as _state) =
              [%e Exp.match_ [%expr xs] cases]
            in loop xs [%e tuple thunks]
          | _ -> [%e top_error]]
      | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    in
    let ty = desu_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var = pvar (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl) in
    ([],
     [Vb.mk (Pat.constraint_ var poly_type)
            (polymorphize [%expr ([%e wrap_runtime desurializer])])])

let desu_str_of_type_ext ~options ~path ({ ptyext_path = { loc } } as type_ext) =
  ignore(parse_options options);
  let desurializer =
    let pats =
      List.fold_right (fun { pext_name = { txt = name' }; pext_kind; pext_attributes } acc_cases ->
        match pext_kind with
        | Pext_rebind _ ->
          (* nothing to do since it must have been handled in the original
             constructor declaration *)
          acc_cases
        | Pext_decl (pext_args, _) ->
          let case =
            match pext_args with
            | Pcstr_tuple(args) ->
              Exp.case
                [%pat? `List ((`String [%p pstr (attr_name name' pext_attributes)]) ::
                                       [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)])]
                (desu_fold ~path (fun x -> constr name' x) args)
#if OCAML_VERSION >= (4, 03, 0)
            | Pcstr_record _ ->
              raise_errorf ~loc "%s: record variants are not supported" deriver
#endif
          in
          case :: acc_cases)
        type_ext.ptyext_constructors []
    in
    let any_case = Exp.case (Pat.var (mknoloc "x"))
      (app (Ppx_deriving.poly_apply_of_type_ext type_ext [%expr fallback])
       [[%expr x]])
    in
    (pats @ [any_case]) |> Exp.function_
  in
  let mod_name =
    let mod_lid =
      Ppx_deriving.mangle_lid
        (`PrefixSuffix ("M", "of_yojson")) type_ext.ptyext_path.txt
    in
    String.concat "." (Longident.flatten mod_lid)
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let desurializer = wrap_runtime (polymorphize desurializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid desurializer in
  let field = Exp.field (Exp.ident flid) flid in
  let body = [%expr let fallback = [%e field] in [%e set_field]] in
  [Str.value ?loc:None Nonrecursive [Vb.mk (Pat.construct (lid "()") None) body]]

let ser_sig_of_type ~options ~path type_decl =
  let to_yojson =
    Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl))
                      (ser_type_of_decl ~options ~path type_decl))
  in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "to_yojson")) type_decl
    in
    let poly_vars = List.rev
      (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: [%t var] -> Yojson.Safe.json]) type_decl
    in
    let ty = Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Yojson.Safe.json]) in
    let typ = Type.mk ~kind:(Ptype_record
       [Type.field ~mut:Mutable (mknoloc "f") ty]) (mknoloc "t_to_yojson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_to_yojson") []) in
    let mod_ =
      Sig.module_ (Md.mk (mknoloc mod_name)
                  (Mty.signature [
        Sig.type_ Type_Nonrecursive [typ];
        Sig.value record;
      ]))
    in
    [mod_; to_yojson]
  | _ -> [to_yojson]


let ser_sig_of_type_ext ~options ~path type_ext = []

let desu_sig_of_type ~options ~path type_decl =
  let of_yojson =
    Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
                      (desu_type_of_decl ~options ~path type_decl))
  in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "of_yojson")) type_decl
    in
    let poly_vars = List.rev
      (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize_desu = Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: Yojson.Safe.json -> [%t error_or var]]) type_decl in
    let ty = Typ.poly poly_vars
      (polymorphize_desu [%type: Yojson.Safe.json -> [%t error_or typ]])
    in
    let typ = Type.mk ~kind:(Ptype_record
       [Type.field ~mut:Mutable (mknoloc "f") ty]) (mknoloc "t_of_yojson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_of_yojson") []) in
    let mod_ =
      Sig.module_ (Md.mk (mknoloc mod_name)
                  (Mty.signature [
        Sig.type_ Type_Nonrecursive [typ];
        Sig.value record;
      ]))
    in
    [mod_; of_yojson]
  | _ -> [of_yojson]

let desu_sig_of_type_ext ~options ~path type_ext = []

let str_of_type ~options ~path type_decl =
  let (ser_pre, ser_vals) = ser_str_of_type ~options ~path type_decl in
  let (desu_pre, desu_vals) = desu_str_of_type ~options ~path type_decl in
  (ser_pre @ desu_pre, ser_vals @ desu_vals)

let str_of_type_ext ~options ~path type_ext =
  let ser_vals = ser_str_of_type_ext ~options ~path type_ext in
  let desu_vals = desu_str_of_type_ext ~options ~path type_ext in
  ser_vals @ desu_vals

let sig_of_type ~options ~path type_decl =
  (ser_sig_of_type ~options ~path type_decl) @
  (desu_sig_of_type ~options ~path type_decl)

let sig_of_type_ext ~options ~path type_ext =
  (ser_sig_of_type_ext ~options ~path type_ext) @
  (desu_sig_of_type_ext ~options ~path type_ext)

let structure f ~options ~path type_ =
  let (pre, vals) = f ~options ~path type_ in
  match vals with
  | [] -> pre
  | _  -> pre @ [Str.value ?loc:None Recursive vals]

let on_str_decls f ~options ~path type_decls =
  let (pre, vals) = List.split (List.map (f ~options ~path) type_decls) in
  (List.concat pre, List.concat vals)

let on_sig_decls f ~options ~path type_decls =
  List.concat (List.map (f ~options ~path) type_decls)

let () =
  Ppx_deriving.(register
   (create "yojson"
    ~type_decl_str:(structure (on_str_decls str_of_type))
    ~type_ext_str:str_of_type_ext
    ~type_decl_sig:(on_sig_decls sig_of_type)
    ~type_ext_sig:sig_of_type_ext
    ()
   ));
  Ppx_deriving.(register
   (create "to_yojson"
    ~core_type:ser_expr_of_typ
    ~type_decl_str:(structure (on_str_decls ser_str_of_type))
    ~type_ext_str:ser_str_of_type_ext
    ~type_decl_sig:(on_sig_decls ser_sig_of_type)
    ~type_ext_sig:ser_sig_of_type_ext
    ()
  ));
  Ppx_deriving.(register
   (create "of_yojson"
    ~core_type:(fun typ -> wrap_runtime (desu_expr_of_typ ~path:[] typ))
    ~type_decl_str:(structure (on_str_decls desu_str_of_type))
    ~type_ext_str:desu_str_of_type_ext
    ~type_decl_sig:(on_sig_decls desu_sig_of_type)
    ~type_ext_sig:desu_sig_of_type_ext
    ()
  ))
