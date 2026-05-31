type attribute_payload =
  | BoolPayload of bool
  | ConstructPayload of string
  | FloatPayload of string
  | IdentPayload of Longident.t
  | IntPayload of string
  | StringPayload of string
  | TuplePayload of attribute_payload list
  | UnrecognizedPayload

let tag_is_gen_type s = s = "genType" || s = "gentype"
let tag_is_gen_type_import s = s = "genType.import" || s = "gentype.import"
let tag_is_gen_type_opaque s = s = "genType.opaque" || s = "gentype.opaque"

let tag_is_one_of_the_gen_type_annotations s =
  tag_is_gen_type s || tag_is_gen_type_import s || tag_is_gen_type_opaque s

let rec get_attribute_payload check_text (attributes : Typedtree.attributes) =
  let rec from_expr (expr : Parsetree.expression) =
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
      from_expr e
    | {pexp_desc = Pexp_construct ({txt}, _); _} ->
      Some (ConstructPayload (txt |> Longident.flatten |> String.concat "."))
    | {pexp_desc = Pexp_tuple exprs | Pexp_array exprs} ->
      let payloads =
        exprs |> List.rev
        |> List.fold_left
             (fun payloads expr ->
               match expr |> from_expr with
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
    if check_text txt then
      match payload with
      | PStr [] -> Some UnrecognizedPayload
      | PStr ({pstr_desc = Pstr_eval (expr, _)} :: _) -> expr |> from_expr
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
    else get_attribute_payload check_text tl

let has_attribute check_text (attributes : Typedtree.attributes) =
  get_attribute_payload check_text attributes <> None

let is_ocaml_suppress_dead_warning attributes =
  match
    attributes
    |> get_attribute_payload (fun x -> x = "ocaml.warning" || x = "warning")
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
