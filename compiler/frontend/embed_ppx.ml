open Parsetree

let normalize_tag (tag : string) : string =
  let buf = Bytes.create (String.length tag) in
  let j = ref 0 in
  String.iter
    (fun c ->
      let c' =
        if
          (Char.code c >= 48 && Char.code c <= 57)
          || (Char.code c >= 65 && Char.code c <= 90)
          || (Char.code c >= 97 && Char.code c <= 122)
        then c
        else '_'
      in
      Bytes.unsafe_set buf !j c';
      incr j)
    tag;
  Bytes.sub_string buf 0 !j

let get_module_name () = Ext_filename.module_name !Location.input_name

let sanitize_suffix (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let prev_underscore = ref false in
  String.iter
    (fun ch ->
      let c =
        match ch with
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> Some ch
        | _ -> Some '_'
      in
      match c with
      | Some '_' ->
        if not !prev_underscore then (
          Buffer.add_char buf '_';
          prev_underscore := true)
      | Some c ->
        Buffer.add_char buf c;
        prev_underscore := false
      | None -> ())
    s;
  let out = Buffer.contents buf in
  if out = "" then "1" else out

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
          let tag_norm = normalize_tag base_tag in
          let suffix =
            match get_config_id e with
            | Some id -> sanitize_suffix id
            | None -> string_of_int (bump base_tag)
          in
          let target =
            Printf.sprintf "%s__embed_%s_%s" module_name tag_norm suffix
          in
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
          let tag_norm = normalize_tag base_tag in
          let suffix =
            match get_config_id ex with
            | Some id -> sanitize_suffix id
            | None -> string_of_int (bump base_tag)
          in
          let target =
            Printf.sprintf "%s__embed_%s_%s" module_name tag_norm suffix
          in
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
