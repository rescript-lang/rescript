open Parsetree

let mkdirp path =
  let rec loop p =
    if Sys.file_exists p then ()
    else
      let parent = Filename.dirname p in
      if parent <> p then loop parent;
      try Unix.mkdir p 0o777 with Unix.Unix_error (_, _, _) -> ()
  in
  loop path

let is_enabled () = !Js_config.collect_embeds

let should_collect_tag (name : string) : bool =
  if !Js_config.embed_collect_all then true
  else List.mem name !Js_config.embed_tags

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

let normalize_slashes (s : string) : string =
  if Sys.win32 || Sys.cygwin then
    String.map (fun c -> if c = '\\' then '/' else c) s
  else s

let rel_to_cwd (file : string) : string =
  let abs = Ext_path.absolute_cwd_path file in
  let from = Sys.getcwd () in
  let rel = Ext_path.rel_normalized_absolute_path ~from abs in
  let s = if rel = "" then Filename.basename abs else rel in
  normalize_slashes s

let string_lit_of_payload (payload : Ast_payload.t) :
    (string * Location.t) option =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (e, _attrs); _}] -> (
    match e.pexp_desc with
    | Pexp_constant (Pconst_string (txt, _)) -> Some (txt, e.pexp_loc)
    | _ -> None)
  | _ -> None

let write_structure_index ~outprefix ~sourcefile (ast : structure) : unit =
  if not (is_enabled ()) then ()
  else
    (* Skip generated embed files to prevent nested/embed loops *)
    let is_generated =
      try
        (* Fast path: any source under a __generated__ folder *)
        String.contains sourcefile '/'
        && Ext_string.contain_substring sourcefile "/__generated__/"
        ||
        (* Slower path: check for header markers in source text *)
        let ic = open_in sourcefile in
        let l1 = input_line ic in
        let l2 = try input_line ic with End_of_file -> "" in
        close_in_noerr ic;
        Ext_string.contain_substring l1 "@sourceHash"
        || Ext_string.contain_substring l2 "rewatch-embed:"
      with _ -> false
    in
    if is_generated then
      (* Do not emit any embed index for generated files *)
      ()
    else
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
      let add_entry ~tag ~context ~(txt : string) ~(loc : Location.t) =
        let occurrence_index = bump tag in
        let literal_hash = csv_hash tag txt in
        let entry =
          Ext_json_noloc.kvs
            [
              ("tag", Ext_json_noloc.str tag);
              ("context", Ext_json_noloc.str context);
              ( "occurrenceIndex",
                Ext_json_noloc.flo (string_of_int occurrence_index) );
              ("range", loc_to_json loc);
              ("embedString", Ext_json_noloc.str txt);
              ("literalHash", Ext_json_noloc.str literal_hash);
            ]
        in
        entries := entry :: !entries
      in
      let normalize_tag (tag : string) : string =
        match Ext_embed.get_embed_tag tag with Some t -> t | None -> tag
      in
      let rec walk_mod (m : module_expr) (context_for_mod : string option) =
        match m.pmod_desc with
        | Pmod_extension ({txt = tag; loc = _}, payload) -> (
          let base_tag = normalize_tag tag in
          if should_collect_tag base_tag then (
            match string_lit_of_payload payload with
            | Some (txt, loc) ->
              let context =
                match context_for_mod with
                | Some c -> c
                | None -> "module"
              in
            add_entry ~tag:base_tag ~context ~txt ~loc
            | None ->
              Location.raise_errorf ~loc:m.pmod_loc
                "%%%s expects a single string literal" tag)
          else ())
        | Pmod_structure s -> walk_str s
        | Pmod_functor (_name, _arg, body) -> walk_mod body None
        | Pmod_apply (m1, m2) ->
          walk_mod m1 None;
          walk_mod m2 None
        | _ -> ()
      and walk_str (s : structure) =
        List.iter
          (fun (si : structure_item) ->
            match si.pstr_desc with
            | Pstr_module {pmb_expr; _} -> walk_mod pmb_expr None
            | Pstr_recmodule mbs ->
              List.iter
                (fun ({pmb_expr; _} : module_binding) -> walk_mod pmb_expr None)
                mbs
            | Pstr_include {pincl_mod; _} -> walk_mod pincl_mod (Some "include")
            | _ -> ())
          s
      in
      walk_str ast;
      let iter : Ast_iterator.iterator =
        let default_it = Ast_iterator.default_iterator in
        {
          default_it with
          expr =
            (fun self e ->
              (match e.pexp_desc with
              | Pexp_extension ({txt = tag; _}, payload) -> (
                let base_tag = normalize_tag tag in
                if should_collect_tag base_tag then (
                match string_lit_of_payload payload with
                | Some (txt, loc) -> add_entry ~tag:base_tag ~context:"expr" ~txt ~loc
                | None ->
                  Location.raise_errorf ~loc:e.pexp_loc
                    "%%%s expects a single string literal" tag)
                else ())
              | _ -> ());
              default_it.expr self e);
        }
      in
      iter.structure iter ast;
      let entries_json =
        !entries |> List.rev |> Array.of_list |> Ext_json_noloc.arr
      in
      let modulename = Ext_filename.module_name outprefix in
      let source_path = rel_to_cwd sourcefile in
      let json =
        Ext_json_noloc.kvs
          [
            ("version", Ext_json_noloc.flo "1");
            ("module", Ext_json_noloc.str modulename);
            ("sourcePath", Ext_json_noloc.str source_path);
            ("embeds", entries_json);
          ]
      in
      let out_dir = Filename.dirname (outprefix ^ Literals.suffix_ast) in
      mkdirp out_dir;
      Ext_json_noloc.to_file (outprefix ^ ".embeds.json") json
