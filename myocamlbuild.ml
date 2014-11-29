open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    let std_deriver deriver =
      (Findlib.query "ppx_deriving").Findlib.location ^ "/" ^ deriver
    in
    flag ["ocaml"; "compile"; "use_cconv"] &
      S[
        A"-ppx"; A("ocamlfind ppx_deriving/ppx_deriving "^
                   "src/ppx_deriving_cconv.cma "
                   )];

  | _ -> ())
