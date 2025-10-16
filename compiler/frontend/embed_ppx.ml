open Parsetree

let get_module_name () = Ext_filename.module_name !Location.input_name

let payload_expr (payload : Ast_payload.t) : expression option =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (e, _attrs); _}] -> Some e
  | _ -> None

let get_config_id (e : expression) : string option =
  match e.pexp_desc with
  | Pexp_record (fields, None) ->
    let rec find = function
      | [] -> None
      | ({lid; x = v; _} : Parsetree.expression Parsetree.record_element)
        :: rest ->
        let name = String.concat "." (Longident.flatten lid.txt) in
        if name = "id" then
          match v.pexp_desc with
          | Pexp_constant (Pconst_string (s, _)) -> Some s
          | _ -> None
        else find rest
    in
    find fields
  | _ -> None

let rewrite (ast : structure) : structure =
  let counts : (string, int) Hashtbl.t = Hashtbl.create 7 in
  let bump tag =
    let v =
      match Hashtbl.find_opt counts tag with
      | Some i -> i
      | None -> 0
    in
    let v' = v + 1 in
    Hashtbl.replace counts tag v';
    v'
  in
  let module_name = get_module_name () in

  let suffix_from_payload_expr ~base_tag ~bump (e : expression) : string =
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (_, _)) ->
      (* String payload: no config id, use occurrence index *)
      string_of_int (bump base_tag)
    | Pexp_record (_, None) -> (
      match get_config_id e with
      | Some id ->
        if Ext_embed.is_valid_embed_id id then id
        else
          Location.raise_errorf ~loc:e.pexp_loc "%s"
            Ext_embed.invalid_id_error_message
      | None ->
        Location.raise_errorf ~loc:e.pexp_loc "%s"
          Ext_embed.missing_id_error_message)
    | _ ->
      Location.raise_errorf ~loc:e.pexp_loc "%s"
        Ext_embed.invalid_payload_error_message
  in

  let target_for ~module_name ~base_tag ~bump (e : expression) : string =
    let tag_norm = Ext_embed.normalize_tag_for_symbol base_tag in
    let suffix = suffix_from_payload_expr ~base_tag ~bump e in
    Printf.sprintf "%s__embed_%s_%s" module_name tag_norm suffix
  in

  let module_expr (self : Ast_mapper.mapper) (m : module_expr) : module_expr =
    match m.pmod_desc with
    | Pmod_extension ({txt = tag; _}, payload) -> (
      let base_tag_opt = Ext_embed.get_embed_tag tag in
      match base_tag_opt with
      | None -> Ast_mapper.default_mapper.module_expr self m
      | Some base_tag -> (
        match payload_expr payload with
        | None -> Ast_mapper.default_mapper.module_expr self m
        | Some e ->
          let target = target_for ~module_name ~base_tag ~bump e in
          Ast_helper.Mod.ident ~loc:m.pmod_loc
            {txt = Longident.Lident target; loc = m.pmod_loc}))
    | _ -> Ast_mapper.default_mapper.module_expr self m
  in
  let expr (self : Ast_mapper.mapper) (e : expression) : expression =
    match e.pexp_desc with
    | Pexp_extension ({txt = tag; _}, payload) -> (
      let base_tag_opt = Ext_embed.get_embed_tag tag in
      match base_tag_opt with
      | None -> Ast_mapper.default_mapper.expr self e
      | Some base_tag -> (
        match payload_expr payload with
        | None -> Ast_mapper.default_mapper.expr self e
        | Some ex ->
          let target = target_for ~module_name ~base_tag ~bump ex in
          Ast_helper.Exp.ident ~loc:e.pexp_loc
            {
              txt = Longident.Ldot (Longident.Lident target, "default");
              loc = e.pexp_loc;
            }))
    | _ -> Ast_mapper.default_mapper.expr self e
  in
  let mapper : Ast_mapper.mapper =
    {Ast_mapper.default_mapper with expr; module_expr}
  in
  mapper.structure mapper ast

let rewrite_implementation (ast : structure) : structure = rewrite ast
