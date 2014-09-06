open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let prefix = "yojson"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let rec ser_expr_of_typ typ =
  match typ with
  | [%type: int]             -> [%expr fun x -> `Int x]
  | [%type: int32]     | [%type: Int32.t]     -> [%expr fun x -> `Intlit (Int32.to_string x)]
  | [%type: int64]     | [%type: Int64.t]     -> [%expr fun x -> `Intlit (Int64.to_string x)]
  | [%type: nativeint] | [%type: Nativeint.t] -> [%expr fun x -> `Intlit (Nativeint.to_string x)]
  | [%type: float]           -> [%expr fun x -> `Float x]
  | [%type: bool]            -> [%expr fun x -> `Bool x]
  | [%type: string]          -> [%expr fun x -> `String x]
  | [%type: bytes]           -> [%expr fun x -> `String (Bytes.to_string x)]
  | [%type: char]            -> [%expr fun x -> `String ("\"" ^ (String.make 1 x) ^ "\"")]
  | [%type: [%t? typ] ref]   -> [%expr fun x -> [%e ser_expr_of_typ typ] !x]
  | [%type: [%t? typ] list]  -> [%expr fun x -> `List (List.map [%e ser_expr_of_typ typ] x)]
  | [%type: [%t? typ] array] ->
    [%expr fun x -> `List (Array.to_list (Array.map [%e ser_expr_of_typ typ] x))]
  | [%type: [%t? typ] option] ->
    [%expr function None -> `Null | Some x -> [%e ser_expr_of_typ typ] x]
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
        | Rtag (label, _, true (*empty*), []) ->
          Exp.case (Pat.variant label None)
                   [%expr `List [`String [%e str label]]]
        | Rtag (label, _, false, [{ ptyp_desc = Ptyp_tuple typs }]) ->
          Exp.case (Pat.variant label (Some (ptuple (List.mapi (fun i _ -> pvar (argn i)) typs))))
                   [%expr `List ((`String [%e str label]) :: [%e
                      list (List.mapi
                        (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) typs)])]
        | Rtag (label, _, false, [typ]) ->
          Exp.case (Pat.variant label (Some [%pat? x]))
                   [%expr `List [`String [%e str label]; [%e ser_expr_of_typ typ] x]]
        | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) ->
          Exp.case [%pat? [%p Pat.type_ tname] as x]
                   [%expr [%e ser_expr_of_typ typ] x]
        | _ ->
          raise_errorf ~loc:ptyp_loc "Cannot derive Yojson for %s"
                       (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name } -> [%expr ([%e evar ("poly_"^name)] : 'a -> Yojson.Safe.json)]
  | { ptyp_desc = Ptyp_alias (typ, name) } ->
    [%expr fun x -> [%e evar ("poly_"^name)] x; [%e ser_expr_of_typ typ] x]
  | { ptyp_loc } ->
    raise_errorf ~loc:ptyp_loc "Cannot derive Yojson for %s"
                 (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let serializer =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> ser_expr_of_typ manifest
    | Ptype_variant constrs, _ ->
      constrs |>
      List.map (fun { pcd_name = { txt = name' }; pcd_args } ->
        let args = List.mapi (fun i typ -> app (ser_expr_of_typ typ) [evar (argn i)]) pcd_args in
        let result =
          match args with
          | []   -> [%expr `List [`String [%e str name']]]
          | args -> [%expr `List ((`String [%e str name']) :: [%e list args])]
        in
        Exp.case (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) pcd_args)) result) |>
      Exp.function_
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i { pld_name = { txt = name }; pld_type } ->
          [%expr [%e str name],
                 [%e ser_expr_of_typ pld_type] [%e Exp.field (evar "x") (mknoloc (Lident name))]])
      in
      [%expr fun x -> `Assoc [%e list fields]]
    | Ptype_abstract, None -> raise_errorf ~loc "Cannot derive Yojson for fully abstract type"
    | Ptype_open, _        -> raise_errorf ~loc "Cannot derive Yojson for open type"
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl))
               (polymorphize serializer);
   Vb.mk (pvar (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
               (polymorphize [%expr fun _ -> assert false])]

let sig_of_type ~options ~path type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let error_or typ = [%type: [ `Ok of [%t typ] | `Error of string ]] in
  let polymorphize_ser = Ppx_deriving.poly_arrow_of_type_decl
                          (fun var -> [%type: [%t var] -> Yojson.Safe.json]) type_decl
  and polymorphize_des = Ppx_deriving.poly_arrow_of_type_decl
                          (fun var -> [%type: Yojson.Safe.json -> [%t error_or var]]) type_decl in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "to_yojson") type_decl))
              (polymorphize_ser [%type: [%t typ] -> Yojson.Safe.json]));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_yojson") type_decl))
              (polymorphize_des [%type: Yojson.Safe.json -> [%t error_or typ]]))]

let () =
  Ppx_deriving.(register "Yojson" {
    core_type = (fun { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "[%%derive.Yojson] is not supported");
    structure = (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls));
  })
