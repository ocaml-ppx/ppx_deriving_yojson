open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    let std_deriver deriver =
      (Findlib.query "ppx_deriving").Findlib.location ^ "/" ^ deriver
    in
    flag ["ocaml"; "compile"; "use_yojson"] &
      S[A"-ppx"; A"ocamlfind ppx_import/ppx_import";
        A"-ppx"; A("ocamlfind ppx_deriving/ppx_deriving "^
                   "src/ppx_deriving_yojson.cma "^
                   (std_deriver "ppx_deriving_show.cma"))];

  | _ -> ())
