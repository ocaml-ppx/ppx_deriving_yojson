open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "yojson"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let attr_int_encoding attrs =
  match Ppx_deriving.attr ~prefix "encoding" attrs |>
        Ppx_deriving.Arg.(payload ~name:"yojson" (enum ["string"; "number"])) with
  | Some "string" -> `String
  | Some "number" | None -> `Int
  | _ -> assert false

let attr_string name default attrs =
  match Ppx_deriving.attr ~prefix name attrs |>
        Ppx_deriving.Arg.(payload ~name:"yojson" string) with
  | Some x -> x
  | None   -> default

let attr_key  = attr_string "key"
let attr_name = attr_string "name"

let attr_default attrs =
  Ppx_deriving.attr ~prefix "default" attrs |>  (* TODO vvv replace with expr *)
  Ppx_deriving.Arg.(payload ~name:"yojson" (fun x -> `Ok x))

(* TODO remove after ppx_tools 0.99.3 *)
let tuple l  = match l with [x] -> x | xs -> Exp.tuple xs
let ptuple l = match l with [x] -> x | xs -> Pat.tuple xs

let rec ser_expr_of_typ typ =
  let attr_int_encoding typ =
    match attr_int_encoding typ with `String -> "String" | `Int -> "Intlit"
  in
  match typ with
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
    app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "to_yojson") lid)))
        (List.map ser_expr_of_typ args)
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
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e ser_expr_of_typ typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "Cannot derive yojson for %s"
                       (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> [%expr ([%e evar ("poly_"^name)] : 'a -> Yojson.Safe.json)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name)] x; [%e ser_expr_of_typ typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "Cannot derive yojson for %s"
                 (Ppx_deriving.string_of_core_type typ)

(* http://desuchan.net/desu/src/1284751839295.jpg *)
let rec desu_fold ~path f typs =
  typs |>
  List.mapi (fun i typ -> i, app (desu_expr_of_typ ~path typ) [evar (argn i)]) |>
  List.fold_left (fun x (i, y) ->
    [%expr [%e y] >>= fun [%p pvar (argn i)] -> [%e x]])
    [%expr `Ok [%e f (List.mapi (fun i _ -> evar (argn i)) typs)]]
and desu_expr_of_typ ~path typ =
  let error = [%expr `Error [%e str (String.concat "." path)]] in
  let decode' cases =
    Exp.function_ (
      List.map (fun (pat, exp) -> Exp.case pat exp) cases @
      [Exp.case [%pat? _] error])
  in
  let decode pat exp = decode' [pat, exp] in
  match typ with
  | [%type: int]    -> decode [%pat? `Int x]    [%expr `Ok x]
  | [%type: float]  -> decode [%pat? `Float x]  [%expr `Ok x]
  | [%type: bool]   -> decode [%pat? `Bool x]   [%expr `Ok x]
  | [%type: string] -> decode [%pat? `String x] [%expr `Ok x]
  | [%type: bytes]  -> decode [%pat? `String x] [%expr `Ok (Bytes.of_string x)]
  | [%type: char]   ->
    decode [%pat? `String x] [%expr if String.length x = 1 then `Ok x.[0] else [%e error]]
  | [%type: int32] | [%type: Int32.t] ->
    decode' [[%pat? `Int x],    [%expr `Ok (Int32.of_int x)];
             [%pat? `Intlit x], [%expr `Ok (Int32.of_string x)]]
  | [%type: int64] | [%type: Int64.t] ->
    begin match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr `Ok (Int64.of_string x)]
    | `Int ->
      decode' [[%pat? `Int x],    [%expr `Ok (Int64.of_int x)];
               [%pat? `Intlit x], [%expr `Ok (Int64.of_string x)]]
    end
  | [%type: nativeint] | [%type: Nativeint.t] ->
    begin match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr `Ok (Nativeint.of_string x)]
    | `Int ->
      decode' [[%pat? `Int x],    [%expr `Ok (Nativeint.of_int x)];
               [%pat? `Intlit x], [%expr `Ok (Nativeint.of_string x)]]
    end
  | [%type: [%t? typ] ref]   ->
    [%expr fun x -> [%e desu_expr_of_typ ~path:(path @ ["contents"]) typ] x >|= ref]
  | [%type: [%t? typ] option] ->
    [%expr function
           | `Null -> `Ok None
           | x     -> [%e desu_expr_of_typ ~path typ] x >>= fun x -> `Ok (Some x)]
  | [%type: [%t? typ] list]  ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs]
  | [%type: [%t? typ] array] ->
    decode [%pat? `List xs]
           [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs >|= Array.of_list]
  | [%type: Yojson.Safe.json] -> [%expr fun x -> `Ok x]
  | { ptyp_desc = Ptyp_tuple typs } ->
    decode [%pat? `List [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)]]
           (desu_fold ~path tuple typs)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
    let inherits, tags = List.partition (function Rinherit _ -> true | _ -> false) fields in
    let tag_cases =
      tags |> List.map (fun field ->
        match field with
        | Rtag (label, attrs, true (*empty*), []) ->
          Exp.case [%pat? `List [`String [%p pstr (attr_name label attrs)]]]
                   [%expr `Ok [%e Exp.variant label None]]
        | Rtag (label, attrs, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
          Exp.case [%pat? `List ((`String [%p pstr (attr_name label attrs)]) :: [%p
                      plist (List.mapi (fun i _ -> pvar (argn i)) typs)])]
                   (desu_fold ~path (fun x -> (Exp.variant label (Some (tuple x)))) typs)
        | Rtag (label, attrs, false, [typ]) ->
          Exp.case [%pat? `List [`String [%p pstr (attr_name label attrs)]; x]]
                   [%expr [%e desu_expr_of_typ ~path typ] x >>= fun x ->
                          `Ok [%e Exp.variant label (Some [%expr x])]]
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e desu_expr_of_typ ~path typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "Cannot derive yojson for %s"
                       (Ppx_deriving.string_of_core_type typ))
    and inherits_case =
      inherits |>
      List.map (function Rinherit typ -> typ | _ -> assert false) |>
      List.fold_left (fun expr typ ->
        [%expr
          match [%e desu_expr_of_typ ~path typ] json with
          | (`Ok _) as result -> result
          | `Error _ -> [%e expr]]) error |>
      Exp.case [%pat? _]
    in
    [%expr fun (json : Yojson.Safe.json) ->
      [%e Exp.match_ [%expr json] (tag_cases @ [inherits_case])]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
    app (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_yojson") lid)))
        (List.map (desu_expr_of_typ ~path) args)
  | { ptyp_desc = Ptyp_var name } ->
    [%expr ([%e evar ("poly_"^name)] : Yojson.Safe.json -> [ `Ok of 'a | `Error of string ])]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name)] x; [%e desu_expr_of_typ ~path typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "Cannot derive yojson for %s"
                 (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let path = path @ [type_decl.ptype_name.txt] in
  let wrap_decls decls = [%expr let open Ppx_deriving_yojson_runtime in [%e decls]] in
  let error path = [%expr `Error [%e str (String.concat "." path)]] in
  let top_error = error path in
  let serializer, desurializer =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      ser_expr_of_typ manifest, wrap_decls (desu_expr_of_typ ~path manifest)
    | Ptype_variant constrs, _ ->
      (* ser *)
      constrs |>
      List.map (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
        let args = List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) pcd_args in
        let json_name = attr_name name' pcd_attributes in
        let result =
          match args with
          | []   -> [%expr `List [`String [%e str json_name]]]
          | args -> [%expr `List ((`String [%e str json_name]) :: [%e list args])]
        in
        Exp.case (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result) |>
      Exp.function_,
      (* desu *)
      List.map (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
        Exp.case [%pat? `List ((`String [%p pstr (attr_name name' pcd_attributes)]) ::
                          [%p plist (List.mapi (fun i _ -> pvar (argn i)) pcd_args)])]
                 (desu_fold ~path (fun x -> constr name' x) pcd_args)) constrs @
      [Exp.case [%pat? _] top_error] |>
      Exp.function_ |>
      wrap_decls
    | Ptype_record labels, _ ->
      (* ser *)
      let fields =
        labels |>
        List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
          let field  = Exp.field (evar "x") (mknoloc (Lident name)) in
          let result = [%expr [%e str (attr_key name pld_attributes)],
                              [%e ser_expr_of_typ pld_type] [%e field]] in
          match attr_default pld_type.ptyp_attributes with
          | None ->
            [%expr [%e result] :: fields]
          | Some default ->
            [%expr if [%e field] = [%e default] then fields else [%e result] :: fields])
      in
      let assoc =
        List.fold_left (fun expr field -> [%expr let fields = [%e field] in [%e expr]])
          [%expr `Assoc fields] fields
      in
      [%expr fun x -> let fields = [] in [%e assoc]],
      (* desu *)
      let record =
        List.fold_left (fun expr i ->
            [%expr [%e evar (argn i)] >>= fun [%p pvar (argn i)] -> [%e expr]])
          [%expr `Ok [%e Exp.record (labels |> List.mapi (fun i { pld_name = { txt = name } } ->
                            mknoloc (Lident name), evar (argn i))) None]]
          (labels |> List.mapi (fun i _ -> i))
      in
      let cases =
        (labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
          let path = path @ [name] in
          let thunks = labels |> List.mapi (fun j _ ->
            if i = j then app (desu_expr_of_typ ~path pld_type) [evar "x"] else evar (argn j)) in
          Exp.case [%pat? ([%p pstr (attr_key name pld_attributes)], x) :: xs]
                   [%expr loop xs [%e tuple thunks]])) @
        [Exp.case [%pat? []] record;
         Exp.case [%pat? _]  top_error]
      and thunks =
        labels |> List.map (fun { pld_name = { txt = name }; pld_type } ->
          match attr_default pld_type.ptyp_attributes with
          | None   -> error (path @ [name])
          | Some x -> [%expr `Ok [%e x]])
      in
      [%expr
        function
        | `Assoc xs ->
          let rec loop xs [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) labels)] =
            [%e Exp.match_ [%expr xs] cases]
          in loop xs [%e tuple thunks]
        | _ -> [%e top_error]] |>
      wrap_decls
    | Ptype_abstract, None -> raise_errorf ~loc "Cannot derive yojson for fully abstract type"
    | Ptype_open, _        -> raise_errorf ~loc "Cannot derive yojson for open type"
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl))
               (polymorphize [%expr ([%e serializer] : _ -> Yojson.Safe.json)]);
   Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
               (polymorphize [%expr ([%e desurializer] : Yojson.Safe.json -> _)])]

let sig_of_type ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let error_or typ = [%type: [ `Ok of [%t typ] | `Error of string ]] in
  let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
                            (fun var -> [%type: [%t var] -> Yojson.Safe.json]) type_decl
  and polymorphize_desu = Ppx_deriving.poly_arrow_of_type_decl
                            (fun var -> [%type: Yojson.Safe.json -> [%t error_or var]]) type_decl in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl))
              (polymorphize_ser  [%type: [%t typ] -> Yojson.Safe.json]));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
              (polymorphize_desu [%type: Yojson.Safe.json -> [%t error_or typ]]))]

let () =
  Ppx_deriving.(register "yojson" {
    core_type = (fun { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "[%%derive.yojson] is not supported");
    structure = (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls));
  })
