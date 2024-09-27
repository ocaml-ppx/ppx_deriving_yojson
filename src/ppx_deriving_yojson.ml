open Ppxlib
open Ast_helper

module Ast_builder_default_loc = struct
  include Ppx_deriving.Ast_convenience

  let gen_def_loc f x =
    let loc = !Ast_helper.default_loc in
    f ~loc x

  let lid = gen_def_loc Ast_builder.Default.Located.lident
  let list = gen_def_loc Ast_builder.Default.elist
  let pstr = gen_def_loc Ast_builder.Default.pstring
  let plist = gen_def_loc Ast_builder.Default.plist
  let lam = gen_def_loc Ast_builder.Default.pexp_fun Nolabel None
end

open Ast_builder_default_loc

let disable_warning_39 () =
  let loc = !Ast_helper.default_loc in
  let name = { txt = "ocaml.warning"; loc } in
  Ast_helper.Attr.mk ~loc name (PStr [%str "-39"])


let mod_mknoloc x = mknoloc (Some x)

let deriver = "yojson"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"
let ct_attr_int_encoding = Attribute.declare "deriving.yojson.encoding" Attribute.Context.core_type
  Ast_pattern.(single_expr_payload (pexp_variant (map0 (string "string") ~f:`String) (none) ||| pexp_variant (map0 (string "number") ~f:`Int) (none))) (fun enc -> enc)

let label_attr_key = Attribute.declare "deriving.yojson.key" Attribute.Context.label_declaration
  Ast_pattern.(single_expr_payload (estring __)) (fun s -> s)
let attr_name context = Attribute.declare "deriving.yojson.name" context
  Ast_pattern.(single_expr_payload (estring __)) (fun s -> s)
let rtag_attr_name = attr_name Attribute.Context.rtag
let constr_attr_name = attr_name Attribute.Context.constructor_declaration
let ext_attr_name = attr_name Attribute.Context.extension_constructor

let ct_attr_ser = Attribute.declare "deriving.yojson.to_yojson" Attribute.Context.core_type
  Ast_pattern.(single_expr_payload __) (fun e -> e)
let ct_attr_desu = Attribute.declare "deriving.yojson.of_yojson" Attribute.Context.core_type
  Ast_pattern.(single_expr_payload __) (fun e -> e)

let attr_default context = Attribute.declare "deriving.yojson.default" context
  Ast_pattern.(single_expr_payload __) (fun e -> e)
let attr_default = (attr_default Attribute.Context.label_declaration, attr_default Attribute.Context.core_type)

let get_label_attribute (label_attr, ct_attr) label =
  match Attribute.get label_attr label with
  | Some _ as v -> v
  | None -> Attribute.get ct_attr label.pld_type

type options = {
  is_strict: bool;
  want_meta: bool;
  want_exn: bool;
}

let args () = Deriving.Args.(empty +> arg "strict" (ebool __) +> arg "meta" (ebool __) +> arg "exn" (ebool __))

let poly_fun names expr =
  List.fold_right (fun name expr ->
      let loc = name.Location.loc in
      let name = name.Location.txt in
      [%expr fun [%p pvar ("poly_"^name)] -> [%e expr]]
    ) names expr

let type_add_attrs typ attributes =
  { typ with ptyp_attributes = typ.ptyp_attributes @ attributes }

let rec ser_expr_of_typ ~quoter typ =
  match Attribute.get ct_attr_ser typ with
    | Some e -> Ppx_deriving.quote ~quoter e
    | None -> ser_expr_of_only_typ ~quoter typ
and ser_expr_of_only_typ ~quoter typ =
  let loc = typ.ptyp_loc in
  let attr_int_encoding typ =
    match Attribute.get ct_attr_int_encoding typ with Some `String -> "String" | Some `Int | None -> "Intlit"
  in
  let ser_expr_of_typ = ser_expr_of_typ ~quoter in
  match typ with
  | [%type: unit]            -> [%expr fun (x:Ppx_deriving_runtime.unit) -> `Null]
  | [%type: int]             -> [%expr fun (x:Ppx_deriving_runtime.int) -> `Int x]
  | [%type: float]           -> [%expr fun (x:Ppx_deriving_runtime.float) -> `Float x]
  | [%type: bool]            -> [%expr fun (x:Ppx_deriving_runtime.bool) -> `Bool x]
  | [%type: string]          -> [%expr fun (x:Ppx_deriving_runtime.string) -> `String x]
  | [%type: bytes]           -> [%expr fun x -> `String (Bytes.to_string x)]
  | [%type: char]            -> [%expr fun x -> `String (String.make 1 x)]
  | [%type: [%t? typ] ref]   -> [%expr fun x -> [%e ser_expr_of_typ typ] !x]
  | [%type: [%t? typ] list]  -> [%expr fun x -> `List (safe_map [%e ser_expr_of_typ typ] x)]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr fun x -> `Intlit (Int32.to_string x)]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr fun x -> [%e Exp.variant (attr_int_encoding typ)
                                    (Some [%expr (Int64.to_string x)])]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr fun x -> [%e Exp.variant (attr_int_encoding typ)
                                    (Some [%expr (Nativeint.to_string x)])]]
  | [%type: [%t? typ] array] ->
    [%expr fun x -> `List (Array.to_list (Array.map [%e ser_expr_of_typ typ] x))]
  | [%type: [%t? typ] option] ->
    [%expr function None -> `Null | Some x -> [%e ser_expr_of_typ typ] x]
  | [%type: Yojson.Safe.json]
  | [%type: Yojson.Safe.t] -> [%expr fun x -> x]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
    let ser_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "to_yojson") lid)) in
    let fwd = app (Ppx_deriving.quote ~quoter ser_fn) (List.map ser_expr_of_typ args) in
    (* eta-expansion is necessary for let-rec *)
    [%expr fun x -> [%e fwd] x]

  | { ptyp_desc = Ptyp_tuple typs } ->
    [%expr fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
           `List ([%e
                      list (List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) typs)])];
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let cases =
      fields |> List.map (fun (field: row_field) ->
        match field.prf_desc with
        | Rtag(label, true (*empty*), []) ->
          let label = label.txt in
          let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
          Exp.case (Pat.variant label None)
                   [%expr `List [`String [%e str name]]]
        | Rtag(label, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
          let label = label.txt in
          let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
          Exp.case (Pat.variant label (Some (ptuple (List.mapi (fun i _ -> pvar (argn i)) typs))))
                   [%expr `List ((`String [%e str name]) :: [%e
                      list (List.mapi
                        (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) typs)])]
        | Rtag(label, false, [typ]) ->
          let label = label.txt in
          let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
          Exp.case (Pat.variant label (Some [%pat? x]))
                   [%expr `List [`String [%e str name];
                                 [%e ser_expr_of_typ typ] x]]
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e ser_expr_of_typ typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                       deriver (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> [%expr ([%e evar ("poly_"^name)] : _ -> Yojson.Safe.t)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name.txt)] x; [%e ser_expr_of_typ typ] x]
  | { ptyp_desc = Ptyp_poly (names, typ) } ->
     poly_fun names (ser_expr_of_typ typ)
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

(* http://desuchan.net/desu/src/1284751839295.jpg *)
let rec desu_fold ~quoter ~loc ~path f typs =
  typs |>
  List.mapi (fun i typ -> i, app (desu_expr_of_typ ~quoter ~path typ) [evar (argn i)]) |>
  List.fold_left (fun x (i, y) ->
    let loc = x.pexp_loc in
    [%expr [%e y] >>= fun [%p pvar (argn i)] -> [%e x]])
    [%expr Ok [%e f (List.mapi (fun i _ -> evar (argn i)) typs)]]
and desu_expr_of_typ ~quoter ~path typ =
  match Attribute.get ct_attr_desu typ with
    | Some e -> Ppx_deriving.quote ~quoter e
    | None -> desu_expr_of_only_typ ~quoter ~path typ
and desu_expr_of_only_typ ~quoter ~path typ =
  let loc = typ.ptyp_loc in
  let error = [%expr Error [%e str (String.concat "." path)]] in
  let decode' cases =
    Exp.function_ (
      List.map (fun (pat, exp) -> Exp.case pat exp) cases @
      [Exp.case [%pat? _] error])
  in
  let decode pat exp = decode' [pat, exp] in
  let desu_expr_of_typ = desu_expr_of_typ ~quoter in
  match typ with
  | [%type: unit]   -> decode [%pat? `Null] [%expr Ok ()]
  | [%type: int]    -> decode [%pat? `Int x]    [%expr Ok x]
  | [%type: float]  ->
    decode' [[%pat? `Int x],    [%expr Ok (float_of_int x)];
             [%pat? `Intlit x], [%expr Ok (float_of_string x)];
             [%pat? `Float x],  [%expr Ok x]]
  | [%type: bool]   -> decode [%pat? `Bool x]   [%expr Ok x]
  | [%type: string] -> decode [%pat? `String x] [%expr Ok x]
  | [%type: bytes]  -> decode [%pat? `String x] [%expr Ok (Bytes.of_string x)]
  | [%type: char]   ->
    decode [%pat? `String x] [%expr if String.length x = 1 then Ok x.[0] else [%e error]]
  | [%type: int32] | [%type: Int32.t] ->
    decode' [[%pat? `Int x],    [%expr Ok (Int32.of_int x)];
             [%pat? `Intlit x], [%expr Ok (Int32.of_string x)]]
  | [%type: int64] | [%type: Int64.t] ->
    begin match Attribute.get ct_attr_int_encoding typ with
    | Some `String ->
      decode [%pat? `String x] [%expr Ok (Int64.of_string x)]
    | Some `Int | None ->
      decode' [[%pat? `Int x],    [%expr Ok (Int64.of_int x)];
               [%pat? `Intlit x], [%expr Ok (Int64.of_string x)]]
    end
  | [%type: nativeint] | [%type: Nativeint.t] ->
    begin match Attribute.get ct_attr_int_encoding typ with
    | Some `String ->
      decode [%pat? `String x] [%expr Ok (Nativeint.of_string x)]
    | Some `Int | None ->
      decode' [[%pat? `Int x],    [%expr Ok (Nativeint.of_int x)];
               [%pat? `Intlit x], [%expr Ok (Nativeint.of_string x)]]
    end
  | [%type: [%t? typ] ref]   ->
    [%expr fun x -> [%e desu_expr_of_typ ~path:(path @ ["contents"]) typ] x >|= ref]
  | [%type: [%t? typ] option] ->
    [%expr function
           | `Null -> Ok None
           | x     -> [%e desu_expr_of_typ ~path typ] x >>= fun x -> Ok (Some x)]
  | [%type: [%t? typ] list]  ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs]
  | [%type: [%t? typ] array] ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs >|= Array.of_list]
  | [%type: Yojson.Safe.t]
  | [%type: Yojson.Safe.json] -> [%expr fun x -> Ok x]
  | { ptyp_desc = Ptyp_tuple typs } ->
    decode [%pat? `List [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)]]
           (desu_fold ~quoter ~loc ~path tuple typs)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let inherits, tags = List.partition (fun field ->
      match field.prf_desc with
      Rinherit _ -> true
      | _ -> false) fields
    in
    let tag_cases = tags |> List.map (fun field ->
      match field.prf_desc with
      | Rtag(label, true (*empty*), []) ->
        let label = label.txt in
        let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
        Exp.case [%pat? `List [`String [%p pstr name]]]
                 [%expr Ok [%e Exp.variant label None]]
      | Rtag(label, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
        let label = label.txt in
        let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
        Exp.case [%pat? `List ((`String [%p pstr name]) :: [%p
                    plist (List.mapi (fun i _ -> pvar (argn i)) typs)])]
                 (desu_fold ~quoter ~loc ~path (fun x -> (Exp.variant label (Some (tuple x)))) typs)
      | Rtag(label, false, [typ]) ->
        let label = label.txt in
        let name = match Attribute.get rtag_attr_name field with Some s -> s | None -> label in
        Exp.case [%pat? `List [`String [%p pstr name]; x]]
                 [%expr [%e desu_expr_of_typ ~path typ] x >>= fun x ->
                        Ok [%e Exp.variant label (Some [%expr x])]]
      | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
        Exp.case [%pat? [%p Pat.type_ tname] as x]
                 [%expr [%e desu_expr_of_typ ~path typ] x]
      | _ ->
        raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                     deriver (Ppx_deriving.string_of_core_type typ))
    and inherits_case =
      let toplevel_typ = typ in
      inherits
      |> List.map (fun field ->
        match field.prf_desc with
        | Rinherit typ -> typ
        | _ -> assert false)
      |> List.fold_left (fun expr typ -> [%expr
        match [%e desu_expr_of_typ ~path typ] json with
        | (Ok result) -> Ok (result :> [%t toplevel_typ])
        | Error _ -> [%e expr]]) error
      |> Exp.case [%pat? _]
    in
    [%expr fun (json : Yojson.Safe.t) ->
      [%e Exp.match_ [%expr json] (tag_cases @ [inherits_case])]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
     let desu_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_yojson") lid)) in
     let fwd = app (Ppx_deriving.quote ~quoter desu_fn) (List.map (desu_expr_of_typ ~path) args) in
     (* eta-expansion is necessary for recursive groups *)
     [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name } ->
    [%expr ([%e evar ("poly_"^name)] : Yojson.Safe.t -> _ error_or)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name.txt)] x; [%e desu_expr_of_typ ~path typ] x]
  | { ptyp_desc = Ptyp_poly (names, typ) } ->
     poly_fun names (desu_expr_of_typ ~path typ)
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                 deriver (Ppx_deriving.string_of_core_type typ)

let sanitize ~quoter decls =
  Ppx_deriving.sanitize ~quoter ~module_:(Lident "Ppx_deriving_yojson_runtime") decls

let ser_type_of_decl ~options:_ ~path:_ type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
                       (fun var -> [%type: [%t var] -> Yojson.Safe.t]) type_decl in
  polymorphize [%type: [%t typ] -> Yojson.Safe.t]

let ser_str_of_record ~quoter ~loc varname labels =
  let fields =
    labels |> List.mapi (fun _i ({ pld_loc = loc; pld_name = { txt = name }; pld_type; pld_attributes } as label) ->
      let field  = Exp.field (evar varname) (mknoloc (Lident name)) in
      let key = match Attribute.get label_attr_key label with Some s -> s | None -> name in
      let result = [%expr [%e str key],
                    [%e ser_expr_of_typ ~quoter @@ type_add_attrs pld_type pld_attributes] [%e field]] in
      match get_label_attribute attr_default label with
      | None ->
          [%expr [%e result] :: fields]
      | Some default ->
          let default = [%expr ([%e default] : [%t pld_type])] in
          [%expr if [%e field] = [%e Ppx_deriving.quote ~quoter default] then fields else [%e result] :: fields])
  in
  let assoc =
    List.fold_left
      (fun expr field ->
        let loc = expr.pexp_loc in
        [%expr let fields = [%e field] in [%e expr]])
      [%expr `Assoc fields] fields
  in
  [%expr let fields = [] in [%e assoc]]


let ser_str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let quoter = Ppx_deriving.create_quoter () in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open -> begin
    let to_yojson_name = Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl in
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "to_yojson")) type_decl
    in
    match type_decl.ptype_manifest with
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = lid }, _args) } as manifest) ->
      let ser = ser_expr_of_typ ~quoter manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "to_yojson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      let poly_ser = polymorphize [%expr ([%e sanitize ~quoter ser] : [%t typ] -> Yojson.Safe.t)] in
      ([Str.module_ (Mb.mk (mod_mknoloc mod_name) orig_mod)],
       [Vb.mk (pvar to_yojson_name) poly_ser],
       [])
    | Some _ ->
      raise_errorf ~loc "%s: extensible type manifest should be a type name" deriver
    | None ->
      let poly_vars = List.rev
          (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
      in
      let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: [%t var] -> Yojson.Safe.t]) type_decl
      in
      let ty = Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Yojson.Safe.t]) in
      let default_fun =
        let type_path = String.concat "." (path @ [type_decl.ptype_name.txt]) in
        let e_type_path = Ast_builder.Default.estring ~loc:Location.none type_path in
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
        Str.module_ (Mb.mk (mod_mknoloc mod_name)
                    (Mod.structure [
          Str.type_ Nonrecursive [typ];
          Str.value Nonrecursive [record];
        ]))
      in
      ([mod_],
       [Vb.mk (pvar to_yojson_name) [%expr fun x -> [%e field] x]],
       [])
  end
  | kind ->
    let serializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ -> assert false
      | Ptype_abstract, Some manifest -> ser_expr_of_typ ~quoter manifest
      | Ptype_variant constrs, _ ->
        constrs
        |> List.map (fun ({ pcd_name = { txt = name' }; pcd_args; _ } as constr) ->
          let json_name = match Attribute.get constr_attr_name constr with Some s -> s | None -> name' in
          match pcd_args with
          | Pcstr_tuple([]) ->
            Exp.case
              (pconstr name' [])
              [%expr `List [`String [%e str json_name]]]
          | Pcstr_tuple(args) ->
            let arg_exprs =
              List.mapi (fun i typ -> app (ser_expr_of_typ ~quoter typ) [evar (argn i)]) args
            in
            Exp.case
              (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
              [%expr `List ((`String [%e str json_name]) :: [%e list arg_exprs])]
          | Pcstr_record labels ->
            let arg_expr = ser_str_of_record ~quoter ~loc (argn 0) labels in
            Exp.case
              (pconstr name' [pvar(argn 0)])
              [%expr `List ((`String [%e str json_name]) :: [%e list[arg_expr]])]
          )
        |> Exp.function_
      | Ptype_record labels, _ ->
        [%expr fun x -> [%e ser_str_of_record ~quoter ~loc "x" labels]]
      | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    in
    let ty = ser_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var_s = Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl in
    let var = pvar var_s in
    ([],
     [Vb.mk
        ~attrs:[disable_warning_39 ()]
        (Pat.constraint_ var poly_type)
        (polymorphize [%expr ([%e sanitize ~quoter serializer])])],
     [Str.value Nonrecursive [Vb.mk (pvar "_") [%expr [%e evar var_s]]] ]
     )

let ser_str_of_type_ext ~options:_ ~path:_ ({ ptyext_path = { loc }} as type_ext) =
  let quoter = Ppx_deriving.create_quoter () in
  let serializer =
    let pats =
      List.fold_right (fun ({ pext_name = { txt = name' }; pext_kind; _ } as ext) acc_cases ->
        match pext_kind with
        | Pext_rebind _ ->
          (* nothing to do, since the constructor must be handled in original
             constructor declaration *)
          acc_cases
        | Pext_decl (_, pext_args, _) ->
          let json_name = match Attribute.get ext_attr_name ext with Some s -> s | None -> name' in
          let case =
            match pext_args with
            | Pcstr_tuple([]) ->
              Exp.case
                (pconstr name' [])
                [%expr `List [`String [%e str json_name]]]
            | Pcstr_tuple(args) ->
              let arg_exprs =
                List.mapi (fun i typ -> app (ser_expr_of_typ ~quoter typ) [evar (argn i)]) args
              in
              Exp.case
                (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
                [%expr `List ((`String [%e str json_name]) :: [%e list arg_exprs])]
            | Pcstr_record _ ->
              raise_errorf ~loc "%s: record variants are not supported in extensible types" deriver
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
    Longident.name mod_lid
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let serializer = polymorphize (sanitize ~quoter serializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid serializer in
  let field = Exp.field (Exp.ident flid) (flid) in
  let body = [%expr let fallback = [%e field] in [%e set_field]] in
  [Str.value ?loc:None Nonrecursive [Vb.mk (Pat.construct (lid "()") None) body]]

let error_or typ =
  let loc = typ.ptyp_loc in
  [%type: [%t typ] Ppx_deriving_yojson_runtime.error_or]

let desu_type_of_decl_poly ~options:_ ~path:_ type_decl type_ =
  let loc = type_decl.ptype_loc in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
                       (fun var -> [%type: Yojson.Safe.t -> [%t error_or var]]) type_decl in
  polymorphize type_

let desu_type_of_decl ~options ~path type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  desu_type_of_decl_poly ~options ~path type_decl [%type: Yojson.Safe.t -> [%t error_or typ]]


let desu_str_of_record ~quoter ~loc ~is_strict ~error ~path wrap_record labels =
  let top_error = error path in
  let record =
    List.fold_left
      (fun expr i ->
        let loc = expr.pexp_loc in
        [%expr [%e evar (argn i)] >>= fun [%p pvar (argn i)] -> [%e expr]]
      )
      ( let r =
          Exp.record (labels |>
            List.mapi (fun i { pld_name = { txt = name } } ->
              mknoloc (Lident name), evar (argn i)))
            None in
        [%expr Ok [%e wrap_record r] ] )
      (labels |> List.mapi (fun i _ -> i)) in
  let default_case = if is_strict then top_error else [%expr loop xs _state] in
  let cases =
    (labels |> List.mapi (fun i ({ pld_loc = loc; pld_name = { txt = name }; pld_type; pld_attributes } as label) ->
        let path = path @ [name] in
        let thunks = labels |> List.mapi (fun j _ ->
             if i = j
             then app (desu_expr_of_typ ~quoter ~path @@ type_add_attrs pld_type pld_attributes) [evar "x"]
             else evar (argn j)) in
        let key = match Attribute.get label_attr_key label with Some s -> s | None -> name in
        Exp.case [%pat? ([%p pstr key], x) :: xs]
          [%expr loop xs [%e tuple thunks]])) @
    [Exp.case [%pat? []] record;
     Exp.case [%pat? _ :: xs] default_case]
  and thunks =
    labels |> List.map (fun ({ pld_name = { txt = name }; pld_type; _ } as label) ->
      match get_label_attribute attr_default label with
      | None   -> error (path @ [name])
      | Some default ->
        let default = [%expr ([%e default] : [%t pld_type])] in
        [%expr Ok [%e Ppx_deriving.quote ~quoter default]])
  in
  [%expr
    function
    | `Assoc xs ->
      let rec loop xs ([%p ptuple (List.mapi (fun i _ -> pvar (argn i)) labels)] as _state) =
        [%e Exp.match_ [%expr xs] cases]
      in loop xs [%e tuple thunks]
    | _ -> [%e top_error]]


let desu_str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let { is_strict; want_exn; _ } = options in
  let quoter = Ppx_deriving.create_quoter () in
  let path = path @ [type_decl.ptype_name.txt] in
  let error path = [%expr Error [%e str (String.concat "." path)]] in
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
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = lid }, _args) } as manifest) ->
      let desu = desu_expr_of_typ ~quoter ~path manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "of_yojson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      let poly_desu = polymorphize [%expr ([%e sanitize ~quoter desu] : Yojson.Safe.t -> _)] in
      ([Str.module_ (Mb.mk (mod_mknoloc mod_name) orig_mod)],
       [Vb.mk (pvar of_yojson_name) poly_desu],
       [])
    | Some _ ->
      raise_errorf ~loc "%s: extensible type manifest should be a type name" deriver
    | None ->
      let poly_vars = List.rev
        (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
      in
      let polymorphize_desu = Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: Yojson.Safe.t -> [%t error_or var]]) type_decl in
      let ty = Typ.poly poly_vars
        (polymorphize_desu [%type: Yojson.Safe.t -> [%t error_or typ]])
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
        Str.module_ (Mb.mk (mod_mknoloc mod_name)
                    (Mod.structure [
          Str.type_ Nonrecursive [typ];
          Str.value Nonrecursive [record];
        ]))
      in
      ([mod_],
       [Vb.mk (pvar of_yojson_name) [%expr fun x -> [%e field] x]],
       [])
  end
  | kind ->
    let desurializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ -> assert false
      | Ptype_abstract, Some manifest ->
        desu_expr_of_typ ~quoter ~path manifest
      | Ptype_variant constrs, _ ->
        let cases = List.map (fun ({ pcd_loc = loc; pcd_name = { txt = name' }; pcd_args; _ } as constr') ->
          match pcd_args with
          | Pcstr_tuple(args) ->
            let name = match Attribute.get constr_attr_name constr' with Some s -> s | None -> name' in
            Exp.case
              [%pat? `List ((`String [%p pstr name]) ::
                                     [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)])]
              (desu_fold ~quoter ~loc ~path (fun x -> constr name' x) args)
          | Pcstr_record labels ->
            let wrap_record r = constr name' [r] in
            let sub =
              desu_str_of_record ~quoter ~loc ~is_strict ~error ~path wrap_record labels in
            let name = match Attribute.get constr_attr_name constr' with Some s -> s | None -> name' in
            Exp.case
              [%pat? `List ((`String [%p pstr name]) ::
                              [%p plist [pvar (argn 0)]])]
              [%expr [%e sub] [%e evar (argn 0)] ]
          ) constrs
        in
        Exp.function_ (cases @ [Exp.case [%pat? _] top_error])
      | Ptype_record labels, _ ->
        desu_str_of_record ~quoter ~loc ~is_strict ~error ~path (fun r -> r) labels
      | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    in
    let ty = desu_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var_s = Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl in
    let var = pvar var_s in
    let var_s_exn = var_s ^ "_exn" in
    let { ptype_params; _ } = type_decl in
    let var_s_exn_args = List.mapi (fun i _ -> argn i |> evar) ptype_params in
    let var_s_exn_args = var_s_exn_args @ [evar "x"] in
    let var_s_exn_fun =
      let rec loop = function
      | [] -> sanitize ~quoter ([%expr match  [%e app (evar var_s) var_s_exn_args] with Ok x -> x | Error err -> raise (Failure err)])
      | hd::tl -> lam (pvar hd) (loop tl)
      in
      loop ((List.mapi (fun i _ -> argn i) ptype_params) @ ["x"])
    in
    ([],
     [Vb.mk ~attrs:[disable_warning_39 ()]
            (Pat.constraint_ var poly_type)
            (polymorphize [%expr ([%e sanitize ~quoter desurializer])]) ],
     [Str.value Nonrecursive [Vb.mk (pvar "_") [%expr [%e evar var_s]]]]
     @
     (if not want_exn then []
      else
        [Str.value Nonrecursive [Vb.mk (pvar var_s_exn) var_s_exn_fun]
        ;Str.value Nonrecursive [Vb.mk (pvar "_") [%expr [%e evar var_s_exn]]]])
     )

let desu_str_of_type_ext ~options:_ ~path ({ ptyext_path = { loc } } as type_ext) =
  let quoter = Ppx_deriving.create_quoter () in
  let desurializer =
    let pats =
      List.fold_right (fun ({ pext_name = { txt = name' }; pext_kind; _ } as ext) acc_cases ->
        match pext_kind with
        | Pext_rebind _ ->
          (* nothing to do since it must have been handled in the original
             constructor declaration *)
          acc_cases
        | Pext_decl (_, pext_args, _) ->
          let case =
            match pext_args with
            | Pcstr_tuple(args) ->
              let name = match Attribute.get ext_attr_name ext with Some s -> s | None -> name' in
              Exp.case
                [%pat? `List ((`String [%p pstr name]) ::
                                       [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)])]
                (desu_fold ~quoter ~loc ~path (fun x -> constr name' x) args)
            | Pcstr_record _ ->
              raise_errorf ~loc "%s: record variants are not supported in extensible types" deriver
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
    Longident.name mod_lid
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let desurializer = sanitize ~quoter (polymorphize desurializer) in
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
    let loc = typ.ptyp_loc in
    let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: [%t var] -> Yojson.Safe.t]) type_decl
    in
    let ty = Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Yojson.Safe.t]) in
    let typ = Type.mk ~kind:(Ptype_record
       [Type.field ~mut:Mutable (mknoloc "f") ty]) (mknoloc "t_to_yojson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_to_yojson") []) in
    let mod_ =
      Sig.module_ (Md.mk (mod_mknoloc mod_name)
                  (Mty.signature [
        Sig.type_ Nonrecursive [typ];
        Sig.value record;
      ]))
    in
    [mod_; to_yojson]
  | _ -> [to_yojson]


let ser_sig_of_type_ext ~options:_ ~path:_ _type_ext = []

let desu_sig_of_type ~options ~path type_decl =
  let { want_exn; _ } = options in
  let of_yojson =
    Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
                      (desu_type_of_decl ~options ~path type_decl))
  in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let loc = typ.ptyp_loc in
  let of_yojson_exn =
    Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson_exn") type_decl))
                      (desu_type_of_decl_poly ~options ~path type_decl [%type: Yojson.Safe.t -> [%t typ]]))
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
      (fun var -> [%type: Yojson.Safe.t -> [%t error_or var]]) type_decl in
    let ty = Typ.poly poly_vars
      (polymorphize_desu [%type: Yojson.Safe.t -> [%t error_or typ]])
    in
    let typ = Type.mk ~kind:(Ptype_record
       [Type.field ~mut:Mutable (mknoloc "f") ty]) (mknoloc "t_of_yojson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_of_yojson") []) in
    let mod_ =
      Sig.module_ (Md.mk (mod_mknoloc mod_name)
                  (Mty.signature [
        Sig.type_ Nonrecursive [typ];
        Sig.value record;
      ]))
    in
    [mod_; of_yojson]
  | _ ->
    [of_yojson]
    @ (if not want_exn then [] else [of_yojson_exn])

let desu_sig_of_type_ext ~options:_ ~path:_ _type_ext = []

let yojson_str_fields ~options ~path:_ type_decl =
  let { want_meta; _ } = options in
  match want_meta, type_decl.ptype_kind with
  | false, _ | true, Ptype_open -> []
  | true, kind ->
    match kind, type_decl.ptype_manifest with
    | Ptype_record labels, _ ->
      let loc = !Ast_helper.default_loc in
      let fields =
        labels |> List.map (fun ({ pld_name = { txt = name }; _ } as label) ->
          let key = match Attribute.get label_attr_key label with Some s -> s | None -> name in
          [%expr [%e str key]])
      in
      let flist = List.fold_right (fun n acc -> [%expr [%e n] :: [%e  acc]])
        fields [%expr []]
      in
        [
          Str.module_ (Mb.mk (mod_mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "Yojson_meta") type_decl))
                      (Mod.structure [
            Str.value Nonrecursive [Vb.mk (pvar "keys") [%expr [%e flist]]]
          ; Str.value Nonrecursive [Vb.mk (pvar "_") [%expr [%e evar "keys"]]]
          ]))
        ]
    | _ -> []

let yojson_sig_fields ~options ~path:_ type_decl =
  let { want_meta; _ } = options in
  match want_meta, type_decl.ptype_kind with
  | false, _ | true, Ptype_open -> []
  | true, kind ->
    match kind, type_decl.ptype_manifest with
    | Ptype_record _, _ ->
      let loc = !Ast_helper.default_loc in
      [
        Sig.module_ (Md.mk (mod_mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "Yojson_meta") type_decl))
                    (Mty.signature [
          Sig.value (Val.mk (mknoloc "keys") [%type: string list]) ]))
      ]
    | _ -> []

let str_of_type ~options ~path type_decl =
  let (ser_pre, ser_vals, ser_post) = ser_str_of_type ~options ~path type_decl in
  let (desu_pre, desu_vals, desu_post) = desu_str_of_type ~options ~path type_decl in
  let fields_post = yojson_str_fields ~options ~path type_decl in
  (ser_pre @ desu_pre, ser_vals @ desu_vals, ser_post @ desu_post @ fields_post)

let str_of_type_to_yojson ~options ~path type_decl =
  let (ser_pre, ser_vals, ser_post) = ser_str_of_type ~options ~path type_decl in
  let fields_post = yojson_str_fields ~options ~path type_decl in
  (ser_pre, ser_vals, ser_post @ fields_post)

let str_of_type_of_yojson ~options ~path type_decl =
  let (desu_pre, desu_vals, desu_post) = desu_str_of_type ~options ~path type_decl in
  let fields_post = yojson_str_fields ~options ~path type_decl in
  (desu_pre, desu_vals, desu_post @ fields_post)

let str_of_type_ext ~options ~path type_ext =
  let ser_vals = ser_str_of_type_ext ~options ~path type_ext in
  let desu_vals = desu_str_of_type_ext ~options ~path type_ext in
  ser_vals @ desu_vals

let sig_of_type ~options ~path type_decl =
  (ser_sig_of_type ~options ~path type_decl) @
  (desu_sig_of_type ~options ~path type_decl) @
  (yojson_sig_fields ~options ~path type_decl)

let sig_of_type_to_yojson ~options ~path type_decl =
  (ser_sig_of_type ~options ~path type_decl) @
  (yojson_sig_fields ~options ~path type_decl)

let sig_of_type_of_yojson ~options ~path type_decl =
  (desu_sig_of_type ~options ~path type_decl) @
  (yojson_sig_fields ~options ~path type_decl)

let sig_of_type_ext ~options ~path type_ext =
  (ser_sig_of_type_ext ~options ~path type_ext) @
  (desu_sig_of_type_ext ~options ~path type_ext)

let structure f ~options ~path type_ =
  let (pre, vals, post) = f ~options ~path type_ in
  match vals with
  | [] -> pre @ post
  | _  -> pre @ [Str.value ?loc:None Recursive vals] @ post

let on_str_decls f ~options ~path type_decls =
  let unzip3 l =
    List.fold_right (fun (v1, v2, v3) (a1,a2,a3) -> (v1::a1, v2::a2, v3::a3)) l ([],[],[])
  in
  let (pre, vals, post) = unzip3 (List.map (f ~options ~path) type_decls) in
  (List.concat pre, List.concat vals, List.concat post)

let on_sig_decls f ~options ~path type_decls =
  List.concat (List.map (f ~options ~path) type_decls)

(* Note: we are careful to call our sanitize function here, not Ppx_deriving.sanitize. *)
let ser_core_expr_of_typ typ =
  let quoter = Ppx_deriving.create_quoter () in
  let typ = Ppx_deriving.strong_type_of_type typ in
  sanitize ~quoter (ser_expr_of_typ ~quoter typ)

let desu_core_expr_of_typ typ =
  let quoter = Ppx_deriving.create_quoter () in
  let typ = Ppx_deriving.strong_type_of_type typ in
  sanitize ~quoter (desu_expr_of_typ ~quoter ~path:[] typ)

let make_gen f =
  let f' ~ctxt x strict meta exn =
    let is_strict = match strict with
      | Some strict -> strict
      | None -> true (* by default *)
    in
    let want_meta = match meta with
      | Some meta -> meta
      | None -> false (* by default *)
    in
    let want_exn = match exn with
      | Some exn -> exn
      | None -> false (* by default *)
    in
    let options = { is_strict; want_meta; want_exn } in
    let path =
      let code_path = Expansion_context.Deriver.code_path ctxt in
      (* Cannot use main_module_name from code_path because that contains .cppo suffix (via line directives), so it's actually not the module name. *)
      (* Ppx_deriving.module_from_input_name ported to ppxlib. *)
      let main_module_path = match Expansion_context.Deriver.input_name ctxt with
        | ""
        | "_none_" -> []
        | input_name ->
          match Filename.chop_suffix input_name ".ml" with
          | exception _ ->
            (* see https://github.com/ocaml-ppx/ppx_deriving/pull/196 *)
            []
          | path ->
            [String.capitalize_ascii (Filename.basename path)]
      in
      main_module_path @ Code_path.submodule_path code_path
    in
    f ~options ~path x
  in
  Deriving.Generator.V2.make (args ()) f'

let to_yojson: Deriving.t =
  Deriving.add
    "to_yojson"
    ~str_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        structure (on_str_decls str_of_type_to_yojson) ~options ~path type_decls
      ))
    ~sig_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        on_sig_decls sig_of_type_to_yojson ~options ~path type_decls
      ))
    ~str_type_ext:(make_gen ser_str_of_type_ext)
    ~sig_type_ext:(make_gen ser_sig_of_type_ext)

let of_yojson: Deriving.t =
  Deriving.add
    "of_yojson"
    ~str_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        structure (on_str_decls str_of_type_of_yojson) ~options ~path type_decls
      ))
    ~sig_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        on_sig_decls sig_of_type_of_yojson ~options ~path type_decls
      ))
    ~str_type_ext:(make_gen desu_str_of_type_ext)
    ~sig_type_ext:(make_gen desu_sig_of_type_ext)

(* Not just alias because yojson also has meta (without its own deriver name) *)
let yojson: Deriving.t =
  Deriving.add
    "yojson"
    ~str_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        structure (on_str_decls str_of_type) ~options ~path type_decls
      ))
    ~sig_type_decl:(make_gen (fun ~options ~path (_, type_decls) ->
        on_sig_decls sig_of_type ~options ~path type_decls
      ))
    ~str_type_ext:(make_gen str_of_type_ext)
    ~sig_type_ext:(make_gen sig_of_type_ext)

(* custom extensions such that "derive"-prefixed also works *)
let to_derive_extension =
  Extension.V3.declare "ppx_deriving_yojson.derive.to_yojson" Extension.Context.expression
    Ast_pattern.(ptyp __) (fun ~ctxt:_ -> ser_core_expr_of_typ)
let of_derive_extension =
  Extension.V3.declare "ppx_deriving_yojson.derive.of_yojson" Extension.Context.expression
    Ast_pattern.(ptyp __) (fun ~ctxt:_ -> desu_core_expr_of_typ)
let _derive_transformation =
  Driver.register_transformation
    deriver
    ~rules:[
      Context_free.Rule.extension to_derive_extension;
      Context_free.Rule.extension of_derive_extension;
    ]
