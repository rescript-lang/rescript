type attributePayload =
  | BoolPayload of bool
  | ConstructPayload of string
  | FloatPayload of string
  | IdentPayload of Longident.t
  | IntPayload of string
  | StringPayload of string
  | TuplePayload of attributePayload list
  | UnrecognizedPayload

let tagIsGenType s = s = "genType" || s = "gentype"
let tagIsGenTypeImport s = s = "genType.import" || s = "gentype.import"
let tagIsGenTypeOpaque s = s = "genType.opaque" || s = "gentype.opaque"

let tagIsOneOfTheGenTypeAnnotations s =
  tagIsGenType s || tagIsGenTypeImport s || tagIsGenTypeOpaque s

let rec getAttributePayload checkText (attributes : Typedtree.attributes) =
  let rec fromExpr (expr : Parsetree.expression) =
    match expr with
    | {pexp_desc = Pexp_constant (Pconst_string (s, _))} ->
      Some (StringPayload s)
    | {pexp_desc = Pexp_constant (Pconst_integer (n, _))} -> Some (IntPayload n)
    | {pexp_desc = Pexp_constant (Pconst_float (s, _))} -> Some (FloatPayload s)
    | {
     pexp_desc = Pexp_construct ({txt = Lident (("true" | "false") as s)}, _);
     _;
    } ->
      Some (BoolPayload (s = "true"))
    | {pexp_desc = Pexp_construct ({txt = Longident.Lident "[]"}, None)} -> None
    | {pexp_desc = Pexp_construct ({txt = Longident.Lident "::"}, Some e)} ->
      fromExpr e
    | {pexp_desc = Pexp_construct ({txt}, _); _} ->
      Some (ConstructPayload (txt |> Longident.flatten |> String.concat "."))
    | {pexp_desc = Pexp_tuple exprs | Pexp_array exprs} ->
      let payloads =
        exprs |> List.rev
        |> List.fold_left
             (fun payloads expr ->
               match expr |> fromExpr with
               | Some payload -> payload :: payloads
               | None -> payloads)
             []
      in
      Some (TuplePayload payloads)
    | {pexp_desc = Pexp_ident {txt}} -> Some (IdentPayload txt)
    | _ -> None
  in
  match attributes with
  | [] -> None
  | ({Asttypes.txt}, payload) :: tl ->
    if checkText txt then
      match payload with
      | PStr [] -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_eval (expr, _)} :: _) -> expr |> fromExpr
      | PStr ({pstr_desc = Pstr_extension _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_value _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_primitive _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_type _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_typext _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_exception _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_module _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_recmodule _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_modtype _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_open _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_include _} :: _) -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_attribute _} :: _) -> Some UnrecognizedPayload
      | PPat _ -> Some UnrecognizedPayload
      | PSig _ -> Some UnrecognizedPayload
      | PTyp _ -> Some UnrecognizedPayload
    else getAttributePayload checkText tl

let hasAttribute checkText (attributes : Typedtree.attributes) =
  getAttributePayload checkText attributes <> None

let isOcamlSuppressDeadWarning attributes =
  match
    attributes
    |> getAttributePayload (fun x -> x = "ocaml.warning" || x = "warning")
  with
  | Some (StringPayload s) ->
    let numeric =
      match Str.search_forward (Str.regexp (Str.quote "-32")) s 0 with
      | _ -> true
      | exception Not_found -> false
    in
    let textual =
      match
        Str.search_forward
          (Str.regexp (Str.quote "-unused-value-declaration"))
          s 0
      with
      | _ -> true
      | exception Not_found -> false
    in
    numeric || textual
  | _ -> false
