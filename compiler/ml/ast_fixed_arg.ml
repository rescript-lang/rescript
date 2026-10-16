open Parsetree

let of_payload (payload : payload) : fixed_value Location.loc option =
  match payload with
  | PStr [{pstr_desc = Pstr_eval ({pexp_desc; pexp_loc}, _); pstr_loc = loc}]
    -> (
    let fixed txt = Some {Location.txt; loc} in
    match pexp_desc with
    | Pexp_constant (Pconst_integer (source, None)) ->
      fixed (Pfixed_int {txt = source; loc = pexp_loc})
    | Pexp_constant (Pconst_string payload) -> fixed (Pfixed_string payload)
    | Pexp_template {source_segments = [source]; values = []} ->
      fixed (Pfixed_template source)
    | Pexp_tagged_template
        {
          tag = {pexp_desc = Pexp_ident {txt = Lident "json"}};
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

let to_attribute ?(loc = Location.none) (value : fixed_value Location.loc) :
    attribute =
  ( Location.mkloc "as" loc,
    PStr
      [
        Ast_helper.Str.eval ~loc:value.loc
          (to_expression ~loc:value.loc value.txt);
      ] )

(* [@as(value) _] on a direct argument becomes [Parg_fixed]. An argument with
   an invalid payload stays as written, so external processing reports it as
   before. So do the rare arguments that [Parg_fixed] could not print back as
   written: a [_] with other attributes too, and a braced payload such as
   [@as({1})]. *)
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
    match split [] ptyp_attributes with
    | Some ([], as_loc, payload, []) -> (
      match of_payload payload with
      | None -> arg
      | Some value ->
        let loc =
          if as_loc = Location.none then ptyp_loc
          else {as_loc with loc_end = ptyp_loc.loc_end}
        in
        Parg_fixed {attrs; lbl; value; loc})
    | Some _ | None -> arg)
  | Parg_type _ | Parg_fixed _ -> arg

let underscore_loc (loc : Location.t) =
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

let normalize_external_type (typ : core_type) : core_type =
  match typ.ptyp_desc with
  | Ptyp_arrow {params; ret} ->
    {
      typ with
      ptyp_desc = Ptyp_arrow {params = List.map normalize_arg params; ret};
    }
  | _ -> typ
