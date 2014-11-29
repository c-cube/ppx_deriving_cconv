(* Largely inspired from ppx_deriving_yojson *)

open Longident
open Location
open Asttypes
open Parsetree
module AH = Ast_helper
module AC = Ast_convenience

let deriver = "cconv"
let raise_errorf = Ppx_deriving.raise_errorf

let encode_prefix = `Prefix "enc"
let decode_prefix = `Prefix "dec"

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
    | _ -> raise_errorf ~loc:expr.pexp_loc
      "%s does not support option %s" deriver name);
  !strict

(* fold right, with index of element *)
let fold_right_i f l acc =
  let rec fold' f acc i l = match l with
  | [] -> acc
  | x::tail ->
      let acc = fold' f acc (i+1) tail in
      f i x acc
  in
  fold' f acc 0 l

(* generate a [typ CConv.Encode.encoder] for the given [typ].
  @param self an option contains the type being defined, and a reference
    indicating whether a self-reference was used *)
let encode_of_typ ~self typ =
  let rec encode_of_typ typ = match typ with
  | [%type: int]             -> [%expr CConv.Encode.int]
  | [%type: float]           -> [%expr CConv.Encode.float]
  | [%type: bool]            -> [%expr CConv.Encode.bool]
  | [%type: string]          -> [%expr CConv.Encode.string]
  | [%type: bytes]           -> [%expr CConv.Encode.(map Bytes.to_string string)]
  | [%type: char]            -> [%expr CConv.Encode.(map (String.make 1) string)]
  | [%type: [%t? typ] ref]   -> [%expr CConv.Encode.(map (!) [%e encode_of_typ typ])]
  | [%type: [%t? typ] list]  -> [%expr CConv.Encode.(list [%e encode_of_typ typ])]
  | [%type: int32] | [%type: Int32.t] ->
      [%expr CConv.Encode.(map Int32.to_int int)]
  | [%type: int64] | [%type: Int64.t] ->
      [%expr CConv.Encode.(map Int64.to_string string)]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      [%expr CConv.Encode.(map Nativeint.to_string string)]
  | [%type: [%t? typ] array] ->
      [%expr CConv.Encode.(array [%e encode_of_typ typ])]
  | [%type: [%t? typ] option] ->
      [%expr CConv.Encode.(option [%e encode_of_typ typ])]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      begin match self with
      | Some (name, used) when Longident.last lid=name ->
          (* typ is actually a recursive reference to the type
            being defined. Use a "self" variables that will be bound
            with [CConv.Encode.record_fix] or [CConv.Encode.sum_fix] *)
          used := true;
          AC.evar "self"
      | _ ->
          AC.app
            (AH.Exp.ident (mknoloc (Ppx_deriving.mangle_lid encode_prefix lid)))
            (List.map encode_of_typ args)
      end
  | { ptyp_desc = Ptyp_tuple typs } ->
      (* encode tuple, by destructuring it *)
      [%expr CConv.Encode.tuple
        (fun [%p AC.ptuple (List.mapi (fun i _ -> AC.pvar (argn i)) typs)] ->
          [%e fold_right_i
            (fun i typ acc ->
              [%expr CConv.Encode.hcons
                [%e encode_of_typ typ]
                [%e AC.evar (argn i)]
                [%e acc]
              ]
            ) typs [%expr CConv.Encode.hnil]
          ]
        )
      ]
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for poly variants" deriver
  | { ptyp_desc = Ptyp_var name } ->
      [%expr ([%e AC.evar ("poly_"^name)] : 'a CConv.Encode.encoder)]
  | { ptyp_desc = Ptyp_alias (typ, name) } -> encode_of_typ typ
  | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)
  in
  encode_of_typ typ

(* make an encoder from a type declaration *)
let encode_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let encoder =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> encode_of_typ ~self:None manifest
    | Ptype_variant constrs, _ ->
        let self_used = ref false in
        let self = Some (type_decl.ptype_name.txt, self_used) in
        (* pattern matching *)
        let cases = List.map
          (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
            (* first, encode arguments *)
            let args = fold_right_i
              (fun i typ acc ->
                [%expr CConv.Encode.hcons
                  [%e encode_of_typ ~self typ]
                  [%e AC.evar (argn i)]
                  [%e acc]
                ]
              ) pcd_args [%expr CConv.Encode.hnil]
            in
            let enc_name = attr_name name' pcd_attributes in
            (* result is name,arguments *)
            let result = AC.tuple [AC.str enc_name; args] in
            (* the pattern case itself *)
            AH.Exp.case
              (AC.pconstr name' (List.mapi (fun i _ -> AC.pvar (argn i)) pcd_args))
              result
          ) constrs
        in
        let f = AH.Exp.function_ cases in
        if !self_used
        then [%expr CConv.Encode.sum_fix (fun self -> [%e f]) ]
        else [%expr CConv.Encode.sum [%e f]]
    | Ptype_record labels, _ ->
        let self_used = ref false in
        let self = Some (type_decl.ptype_name.txt, self_used) in
        (* build the function  record->hlist  (here, its body). The record
            is named "r". *)
        let destruct = fold_right_i
          (fun i field tail ->
            [%expr CConv.Encode.field
              [%e AC.str field.pld_name.txt]
              [%e (encode_of_typ ~self field.pld_type)]
              [%e AH.Exp.field [%expr r] (AC.lid field.pld_name.txt)]
              [%e tail]
            ]
          ) labels [%expr CConv.Encode.record_end]
        in
        let destruct = [%expr fun r -> [%e destruct]] in
        if !self_used
        then [%expr CConv.Encode.record_fix (fun self -> [%e destruct])]
        else [%expr CConv.Encode.record [%e destruct]]
    | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
        raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [AH.Vb.mk
    (AC.pvar (Ppx_deriving.mangle_type_decl encode_prefix type_decl))
    (polymorphize [%expr ([%e encoder] : _ CConv.Encode.encoder)])]

(* signature of the generated encoder *)
let encode_sig_of_type ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize_enc =
    Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: [%t var] CConv.Encode.encoder])
      type_decl
  in
  [AH.Sig.value
    (AH.Val.mk (mknoloc (Ppx_deriving.mangle_type_decl encode_prefix type_decl))
    (polymorphize_enc  [%type: [%t typ] CConv.Encode.encoder]))
  ]

(* generate a [typ CConv.Encode.decode] for the given [typ].
  @param self an option contains the type being defined, and a reference
    indicating whether a self-reference was used *)
let decode_of_typ ~self typ =
  let rec decode_of_typ typ = match typ with
  | [%type: int]             -> [%expr CConv.Decode.int]
  | [%type: float]           -> [%expr CConv.Decode.float]
  | [%type: bool]            -> [%expr CConv.Decode.bool]
  | [%type: string]          -> [%expr CConv.Decode.string]
  | [%type: bytes]           -> [%expr CConv.Decode.(map Bytes.to_string string)]
  | [%type: char]            -> [%expr CConv.Decode.(map (String.make 1) string)]
  | [%type: [%t? typ] ref]   -> [%expr CConv.Decode.(map (!) [%e decode_of_typ typ])]
  | [%type: [%t? typ] list]  -> [%expr CConv.Decode.(list [%e decode_of_typ typ])]
  | [%type: int32] | [%type: Int32.t] ->
      [%expr CConv.Decode.(map Int32.of_int int)]
  | [%type: int64] | [%type: Int64.t] ->
      [%expr CConv.Decode.(map Int64.of_string string)]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      [%expr CConv.Decode.(map Nativeint.of_string string)]
  | [%type: [%t? typ] array] ->
      [%expr CConv.Decode.(array [%e decode_of_typ typ])]
  | [%type: [%t? typ] option] ->
      [%expr CConv.Decode.(option [%e decode_of_typ typ])]
  | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
      begin match self with
      | Some (name, used) when Longident.last lid=name ->
          (* typ is actually a recursive reference to the type
            being defined. Use a "self" variables that will be bound
            with [CConv.Decode.record_fix] or [CConv.Decode.sum_fix] *)
          used := true;
          AC.evar "self"
      | _ ->
          AC.app
            (AH.Exp.ident (mknoloc (Ppx_deriving.mangle_lid decode_prefix lid)))
            (List.map decode_of_typ args)
      end
  | { ptyp_desc = Ptyp_tuple typs } ->
      (* decode tuple, matching on the list *)
      [%expr CConv.Decode.(tuple {tuple_accept=fun src args ->
        match args with
        | [%p   (* didn't find how to build pattern [v1; v2; ...; vn] *)
            fold_right_i
            (fun i ty pat -> [%pat? [%p AC.pvar (argn i)] :: [%p pat]])
            typs [%pat? []]
          ] ->
          [%e AC.tuple (List.mapi
            (fun i ty ->
              [%expr CConv.Decode.apply src
                [%e decode_of_typ ty]
                [%e AC.evar (argn i)]
              ]
            ) typs
          )]
        | _ ->
          CConv.report_error "expected %d-ary tuple"
            [%e AC.int (List.length typs)]
      })]
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for poly variants" deriver
  | { ptyp_desc = Ptyp_var name } ->
      [%expr ([%e AC.evar ("poly_"^name)] : 'a CConv.Decode.decoder)]
  | { ptyp_desc = Ptyp_alias (typ, name) } -> decode_of_typ typ
  | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)
  in
  decode_of_typ typ

(* make an decoder from a type declaration *)
let decode_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let decoder =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> decode_of_typ ~self:None manifest
    | Ptype_variant constrs, _ ->
        let self_used = ref false in
        let self = Some (type_decl.ptype_name.txt, self_used) in
        (* generate pattern matching cases *)
        let cases = List.map
          (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
            AH.Exp.case
              [%pat?
                ([%p AC.pstr name'],
                 [%p AC.plist (List.mapi (fun i ty -> AC.pvar (argn i)) pcd_args)]
                )
              ]
              (AC.constr name'
                (List.mapi
                  (fun i ty ->
                    [%expr CConv.Decode.apply src
                      [%e decode_of_typ ~self ty]
                      [%e AC.evar (argn i)]
                    ]
                  ) pcd_args
                )
              )
          ) constrs
        and last_case = AH.Exp.case
          (AH.Pat.any ()) [%expr CConv.report_error "expected sum"]
        in
        let sum_decoder = [%expr fun src name args ->
          [%e AH.Exp.match_
            [%expr (name,args)]
            (cases @ [last_case])
          ]
        ] in
        if !self_used
        then [%expr CConv.Decode.sum_fix (fun self -> {sum_accept=[%e sum_decoder]}) ]
        else [%expr CConv.Decode.sum {sum_accept=[%e sum_decoder]}]
    | Ptype_record labels, _ ->
        let self_used = ref false in
        let self = Some (type_decl.ptype_name.txt, self_used) in
        (* build a list of
            let field = record_get "field" (decode field) src args in ... *)
        let bindings = fold_right_i
          (fun i field tail ->
            [%expr
              let [%p AC.pvar field.pld_name.txt] =
                CConv.Decode.record_get
                [%e AC.str field.pld_name.txt]
                [%e (decode_of_typ ~self field.pld_type)]
                src
                args in
              [%e tail]
            ]
          ) labels
          (AC.record (* build the record *)
            (List.map
              (fun field ->
                let name = field.pld_name.txt in
                name, AC.evar name
              ) labels
            )
          )
        in
        let record_decoder = [%expr
          {record_accept=fun src args -> [%e bindings] }
        ] in
        if !self_used
        then [%expr CConv.Decode.record_fix (fun self -> [%e record_decoder])]
        else [%expr CConv.Decode.record [%e record_decoder]]
    | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
        raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  [AH.Vb.mk
    (AC.pvar (Ppx_deriving.mangle_type_decl decode_prefix type_decl))
    (polymorphize [%expr ([%e decoder] : _ CConv.Decode.decoder)])]

(* signature of the generated encoder *)
let decode_sig_of_type ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize_enc =
    Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: [%t var] CConv.Encode.decoder])
      type_decl
  in
  [AH.Sig.value
    (AH.Val.mk (mknoloc (Ppx_deriving.mangle_type_decl encode_prefix type_decl))
    (polymorphize_enc  [%type: [%t typ] CConv.Encode.decoder]))
  ]

let str_of_type ~options ~path type_decl =
  encode_of_type ~options ~path type_decl @
  decode_of_type ~options ~path type_decl

let sig_of_type ~options ~path type_decl =
  encode_sig_of_type ~options ~path type_decl @
  decode_sig_of_type ~options ~path type_decl

let () =
  Ppx_deriving.(register "cconv" {
    core_type = None;
    structure = (fun ~options ~path type_decls ->
      [AH.Str.value Nonrecursive
        (List.concat (List.map (str_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls));
  });
  Ppx_deriving.(register "encode" {
    core_type = Some (encode_of_typ ~self:None);
    structure = (fun ~options ~path type_decls ->
      [AH.Str.value Nonrecursive
        (List.concat (List.map (encode_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (encode_sig_of_type ~options ~path) type_decls));
  });
  Ppx_deriving.(register "decode" {
    core_type = Some (fun typ -> (decode_of_typ None typ));
    structure = (fun ~options ~path type_decls ->
      [AH.Str.value Nonrecursive
        (List.concat (List.map (decode_of_type ~options ~path) type_decls))]);
    signature = (fun ~options ~path type_decls ->
      List.concat (List.map (decode_sig_of_type ~options ~path) type_decls));
  });
  ()
