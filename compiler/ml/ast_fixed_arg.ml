open Parsetree

let of_payload (payload : payload) : fixed_value Location.loc option =
  match Ast_payload.unwrap_payload_expression payload with
  (* A payload with attributes stays an attribute, which keeps them. *)
  | PStr
      [
        {
          pstr_desc = Pstr_eval ({pexp_desc; pexp_loc; pexp_attributes = []}, []);
          pstr_loc = loc;
        };
      ] -> (
    let fixed txt = Some {Location.txt; loc} in
    match pexp_desc with
    | Pexp_constant (Pconst_integer (source, None)) ->
      fixed (Pfixed_int {txt = source; loc = pexp_loc})
    | Pexp_constant (Pconst_string payload) -> fixed (Pfixed_string payload)
    | Pexp_template {source_segments = [source]; values = []} ->
      fixed (Pfixed_template source)
    | Pexp_tagged_template
        {
          tag =
            {pexp_desc = Pexp_ident {txt = Lident "json"}; pexp_attributes = []};
          raw_sources = [source];
          values = [];
        } ->
      fixed (Pfixed_json source)
    | _ -> None)
  | _ -> None

let to_expression ?(loc = Location.none) (value : fixed_value) : expression =
  match value with
  | Pfixed_int source ->
    Ast_helper.Exp.constant ~loc:source.loc (Pconst_integer (source.txt, None))
  | Pfixed_string payload ->
    Ast_helper.Exp.constant ~loc (Pconst_string payload)
  | Pfixed_template source -> Ast_helper.Exp.template ~loc [source] []
  | Pfixed_json source ->
    Ast_helper.Exp.tagged_template ~loc
      (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "json") loc))
      [source] []
  | Pfixed_raw source ->
    Ast_helper.Exp.constant ~loc
      (Ast_helper.Const.string (Ast_raw.js_source source))

let to_attribute ?(loc = Location.none) (value : fixed_value Location.loc) :
    attribute =
  ( Location.mkloc "as" loc,
    PStr
      [
        Ast_helper.Str.eval ~loc:value.loc
          (to_expression ~loc:value.loc value.txt);
      ] )

(* [@as(value) _] on a direct argument becomes [Parg_fixed], which prints as
   [%raw(...)]. The [_]'s other attributes become the argument's. An argument
   with an invalid payload or a second [@as] stays as written, so external
   processing reports it as before. *)
let normalize_arg (arg : arg) : arg =
  match arg with
  | Parg_type
      {attrs; lbl; typ = {ptyp_desc = Ptyp_any; ptyp_loc; ptyp_attributes}} -> (
    let rec split before = function
      | [] -> None
      | (({txt = "as"; loc}, payload) : attribute) :: after ->
        Some (List.rev before, loc, payload, after)
      | attr :: after -> split (attr :: before) after
    in
    let is_as (({txt}, _) : attribute) = txt = "as" in
    match split [] ptyp_attributes with
    | None -> arg
    | Some (_, _, _, after) when List.exists is_as after -> arg
    | Some (before, as_loc, payload, after) -> (
      match of_payload payload with
      | None -> arg
      | Some value ->
        let loc =
          if as_loc = Location.none then ptyp_loc
          else {as_loc with loc_end = ptyp_loc.loc_end}
        in
        Parg_fixed {attrs = attrs @ before @ after; lbl; value; loc}))
  | Parg_type _ | Parg_fixed _ -> arg

let closing_loc (loc : Location.t) =
  if loc.loc_ghost then loc
  else
    {
      loc with
      loc_start = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - 1};
    }

let as_name_loc (loc : Location.t) =
  if loc.loc_ghost then loc
  else
    {
      loc with
      loc_end = {loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3};
    }

let quote_js_string semantic =
  "\"" ^ String_literal.encode_js_string semantic ^ "\""

(* Spell a ReScript integer literal as a JavaScript one. Underscore separators
   need ES2021, and a decimal literal with a leading zero is a legacy octal
   literal, which strict-mode JavaScript rejects. *)
let js_integer_source source =
  let source = String.concat "" (String.split_on_char '_' source) in
  let sign, digits =
    if source <> "" && source.[0] = '-' then
      ("-", String.sub source 1 (String.length source - 1))
    else ("", source)
  in
  let length = String.length digits in
  let has_radix_prefix =
    length > 1
    && digits.[0] = '0'
    &&
    match digits.[1] with
    | 'x' | 'X' | 'o' | 'O' | 'b' | 'B' -> true
    | _ -> false
  in
  if has_radix_prefix then sign ^ digits
  else
    let rec first_significant i =
      if i < length - 1 && digits.[i] = '0' then first_significant (i + 1)
      else i
    in
    let start = first_significant 0 in
    sign ^ String.sub digits start (length - start)

let js_source (value : fixed_value) =
  match value with
  | Pfixed_int source -> js_integer_source source.txt
  | Pfixed_string payload ->
    quote_js_string (String_literal.string_semantic payload)
  | Pfixed_template source -> (
    match String_literal.decode_js_template_escapes source.txt with
    | Some semantic -> quote_js_string semantic
    | None -> "`" ^ source.txt ^ "`")
  | Pfixed_json source -> source.txt
  | Pfixed_raw source -> Ast_raw.js_source source

let normalize_external_type (typ : core_type) : core_type =
  match typ.ptyp_desc with
  | Ptyp_arrow {params; ret} ->
    {
      typ with
      ptyp_desc = Ptyp_arrow {params = List.map normalize_arg params; ret};
    }
  | _ -> typ
