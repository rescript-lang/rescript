open Parsetree

let is_enabled () = !Js_config.collect_embeds

let should_collect_tag (name : string) : bool =
  List.mem name !Js_config.embed_tags

let csv_hash (tag : string) (s : string) : string =
  Digest.(to_hex (string (tag ^ "\n" ^ s)))

let pos_to_json (p : Lexing.position) =
  Ext_json_noloc.kvs
    [
      ("line", Ext_json_noloc.flo (string_of_int p.pos_lnum));
      ("column", Ext_json_noloc.flo (string_of_int (p.pos_cnum - p.pos_bol)));
    ]

let loc_to_json (loc : Location.t) =
  Ext_json_noloc.kvs
    [("start", pos_to_json loc.loc_start); ("end", pos_to_json loc.loc_end)]

(* Convert a restricted subset of expressions to JSON for config embeds *)
let rec expr_to_json (e : Parsetree.expression) : Ext_json_noloc.t option =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (s, _)) -> Some (Ext_json_noloc.str s)
  | Pexp_constant (Pconst_integer (s, _)) -> Some (Ext_json_noloc.flo s)
  | Pexp_constant (Pconst_float (s, _)) -> Some (Ext_json_noloc.flo s)
  | Pexp_construct ({txt = Longident.Lident "true"}, None) ->
    Some Ext_json_noloc.true_
  | Pexp_construct ({txt = Longident.Lident "false"}, None) ->
    Some Ext_json_noloc.false_
  | Pexp_array exprs ->
    let xs =
      Ext_list.filter_map exprs (fun e -> expr_to_json e) |> Array.of_list
    in
    Some (Ext_json_noloc.arr xs)
  | Pexp_record (fields, None) ->
    let fields_json =
      Ext_list.filter_map fields
        (fun
          ({lid; x = e; _} : Parsetree.expression Parsetree.record_element) ->
          let key = String.concat "." (Longident.flatten lid.txt) in
          match expr_to_json e with
          | Some v -> Some (key, v)
          | None -> None)
    in
    (* Ensure stable ordering by sorting keys *)
    let fields_json =
      List.sort (fun (a, _) (b, _) -> Stdlib.compare a b) fields_json
    in
    Some (Ext_json_noloc.kvs fields_json)
  | _ -> None

let payload_to_data (payload : Ast_payload.t) :
    (Ext_json_noloc.t * Location.t) option =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (e, _attrs); _}] -> (
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (txt, _)) ->
      Some (Ext_json_noloc.str txt, e.pexp_loc)
    | _ -> (
      match expr_to_json e with
      | Some json -> Some (json, e.pexp_loc)
      | None -> None))
  | _ -> None

let write_structure_index ~outprefix ~sourcefile (ast : structure) : unit =
  if is_enabled () then (
    let entries = ref [] in
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
    let add_entry ~tag ~context ~(data : Ext_json_noloc.t) ~(loc : Location.t) =
      let occurrence_index = bump tag in
      let data_str =
        match data with
        | Ext_json_noloc.Arr _ | Ext_json_noloc.Obj _ ->
          Ext_json_noloc.to_string data
        | _ -> Ext_json_noloc.to_string data
      in
      let literal_hash = csv_hash tag data_str in
      let entry =
        Ext_json_noloc.kvs
          [
            ("tag", Ext_json_noloc.str tag);
            ("context", Ext_json_noloc.str context);
            ( "occurrenceIndex",
              Ext_json_noloc.flo (string_of_int occurrence_index) );
            ("range", loc_to_json loc);
            ("data", data);
            ("literalHash", Ext_json_noloc.str literal_hash);
          ]
      in
      entries := entry :: !entries
    in
    let normalize_tag (tag : string) : string =
      match Ext_embed.get_embed_tag tag with
      | Some t -> t
      | None -> tag
    in
    let current_mod_context : string option ref = ref None in
    let with_context ctx f =
      let prev = !current_mod_context in
      current_mod_context := ctx;
      (try f ()
       with e ->
         current_mod_context := prev;
         raise e);
      current_mod_context := prev
    in
    let iter : Ast_iterator.iterator =
      let default_it = Ast_iterator.default_iterator in
      {
        default_it with
        module_expr =
          (fun self m ->
            (match m.pmod_desc with
            | Pmod_extension ({txt = tag; _}, payload) ->
              let base_tag = normalize_tag tag in
              if should_collect_tag base_tag then
                match payload_to_data payload with
                | Some (data, loc) ->
                  let context =
                    Option.value ~default:"module" !current_mod_context
                  in
                  add_entry ~tag:base_tag ~context ~data ~loc
                | None ->
                  Location.raise_errorf ~loc:m.pmod_loc
                    "%%%s expects a string literal or a JSON-serializable \
                     record literal"
                    tag
              else ()
            | _ -> ());
            let prev = !current_mod_context in
            current_mod_context := None;
            default_it.module_expr self m;
            current_mod_context := prev);
        structure_item =
          (fun self si ->
            match si.pstr_desc with
            | Pstr_module {pmb_expr; _} ->
              with_context None (fun () -> self.module_expr self pmb_expr)
            | Pstr_recmodule mbs ->
              List.iter
                (fun ({pmb_expr; _} : module_binding) ->
                  with_context None (fun () -> self.module_expr self pmb_expr))
                mbs
            | Pstr_include {pincl_mod; _} ->
              with_context (Some "include") (fun () ->
                  self.module_expr self pincl_mod)
            | _ -> default_it.structure_item self si);
        expr =
          (fun self e ->
            (match e.pexp_desc with
            | Pexp_extension ({txt = tag; _}, payload) ->
              let base_tag = normalize_tag tag in
              if should_collect_tag base_tag then
                match payload_to_data payload with
                | Some (data, loc) ->
                  add_entry ~tag:base_tag ~context:"expr" ~data ~loc
                | None ->
                  Location.raise_errorf ~loc:e.pexp_loc
                    "%%%s expects a string literal or a JSON-serializable \
                     record literal"
                    tag
              else ()
            | _ -> ());
            default_it.expr self e);
      }
    in
    iter.structure iter ast;
    let entries_json =
      !entries |> List.rev |> Array.of_list |> Ext_json_noloc.arr
    in
    let modulename = Ext_filename.module_name outprefix in
    let source_path = sourcefile in
    let json =
      Ext_json_noloc.kvs
        [
          ("version", Ext_json_noloc.flo "1");
          ("module", Ext_json_noloc.str modulename);
          ("sourcePath", Ext_json_noloc.str source_path);
          ("embeds", entries_json);
        ]
    in
    Ext_json_noloc.to_file (outprefix ^ ".embeds.json") json)
