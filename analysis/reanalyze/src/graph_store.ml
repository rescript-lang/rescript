module DeclId = struct
  type t = string

  let compare = String.compare
  let to_string x = x
end

module DeclIdSet = Set.Make (struct
  type t = DeclId.t

  let compare = DeclId.compare
end)

module StringSet = Common.StringSet
module FileSet = Common.FileSet
module Decl = DeadCommon.Decl

let string_lists_equal a b =
  let rec aux xs ys =
    match (xs, ys) with
    | [], [] -> true
    | x :: xs', y :: ys' -> String.equal x y && aux xs' ys'
    | _ -> false
  in
  aux a b

let normalize_component name =
  if String.length name > 0 && name.[0] = '+' then
    String.sub name 1 (String.length name - 1)
  else name

let normalize_path path = List.map normalize_component path

let path_matches_decl decl_path target_path =
  let decl_path = normalize_path decl_path in
  let target_path = normalize_path target_path in
  if
    string_lists_equal decl_path target_path
    || string_lists_equal decl_path (List.rev target_path)
  then true
  else
    match (decl_path, target_path) with
    | d_head :: _, t_head :: _ when String.equal d_head t_head -> true
    | _ -> (
        match (List.rev decl_path, List.rev target_path) with
        | d_last :: _, t_last :: _ -> String.equal d_last t_last
        | _ -> false)

let path_key path = String.concat "." (normalize_path path)

let string_contains ~needle haystack =
  let len_h = String.length haystack and len_n = String.length needle in
  let rec loop i =
    if len_n = 0 then true
    else if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let canonicalize_file path =
  let is_abs = String.length path > 0 && path.[0] = '/' in
  let segments = String.split_on_char '/' path in
  let rec collapse acc = function
    | "lib" :: "bs" :: rest -> collapse acc rest
    | seg :: rest -> collapse (seg :: acc) rest
    | [] -> List.rev acc
  in
  let collapsed = collapse [] segments in
  let body = String.concat "/" collapsed in
  if body = "" then path else if is_abs then "/" ^ body else body

let files_match a b =
  let ca = canonicalize_file a and cb = canonicalize_file b in
  String.equal ca cb || String.equal (Filename.basename ca) (Filename.basename cb)

let positions_equal (a : Summary.position) (b : Summary.position) =
  files_match a.file b.file && a.line = b.line && a.column = b.column

let chop_suffix_opt s suffix =
  if String.length s >= String.length suffix
     && String.equal (String.sub s (String.length s - String.length suffix) (String.length suffix)) suffix
  then Some (String.sub s 0 (String.length s - String.length suffix))
  else None

type file_role =
  | Interface of string
  | Implementation of string
  | Other_file of string

let classify_file file =
  let file = canonicalize_file file in
  match
    [
      (".resi", `Interface);
      (".rei", `Interface);
      (".mli", `Interface);
      (".res", `Implementation);
      (".re", `Implementation);
      (".ml", `Implementation);
    ]
  with
  | [] -> Other_file file
  | suffixes ->
      let rec loop = function
        | [] -> Other_file file
        | (suffix, kind) :: rest -> (
            match chop_suffix_opt file suffix with
            | Some base -> (
                match kind with
                | `Interface -> Interface base
                | `Implementation -> Implementation base)
            | None -> loop rest)
      in
      loop suffixes

let alias_candidate file_a file_b =
  match (classify_file file_a, classify_file file_b) with
  | Interface base_a, Implementation base_b
  | Implementation base_a, Interface base_b ->
      String.equal base_a base_b
  | _ -> false

let same_module file_a file_b =
  match (classify_file file_a, classify_file file_b) with
  | Interface base_a, Interface base_b
  | Interface base_a, Implementation base_b
  | Implementation base_a, Interface base_b
  | Implementation base_a, Implementation base_b ->
      String.equal base_a base_b
  | _ -> false

let trace_unknown =
  match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
  | Some ("1" | "true" | "on" | "yes") -> true
  | _ -> false

let format_summary_pos (pos : Summary.position) =
  Printf.sprintf "%s:%d:%d" pos.file pos.line pos.column

let should_debug =
  let debug_filter = Sys.getenv_opt "GRAPH_DEBUG_FILE" in
  fun file ->
    match debug_filter with
    | Some needle when string_contains ~needle file -> true
    | Some needle ->
        string_contains ~needle (Filename.basename file)
    | None -> false

type node = {
  id: DeclId.t;
  file: string;
  decl: Common.decl;
  summary: Summary.decl;
}

type position_key = string * int * int

type pending_value_edge = DeclId.t * bool
type pending_source_value_edge = {
  target: DeclId.t;
  add_file_edge: bool;
  summary_file: string;
}

type t = {
  nodes: (DeclId.t, node) Hashtbl.t;
  file_to_decls: (string, DeclId.t list) Hashtbl.t;
  position_index: (position_key, DeclId.t list) Hashtbl.t;
  path_index: (string, DeclId.t list) Hashtbl.t;
  annotation_index: (position_key, Summary.annotation_snapshot) Hashtbl.t;
  forward_value: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  reverse_value: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  forward_type: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  reverse_type: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  file_digests: (string, string) Hashtbl.t;
  dirty_files: (string, unit) Hashtbl.t;
  pending_value: (position_key, pending_value_edge list) Hashtbl.t;
  pending_type: (position_key, DeclId.t list) Hashtbl.t;
  file_edges: (string, StringSet.t) Hashtbl.t;
  unknown_value_targets: (DeclId.t, unit) Hashtbl.t;
  pending_unknown_value: (position_key, unit) Hashtbl.t;
  pending_unknown_value_by_path: (string, unit) Hashtbl.t;
  unknown_path_targets: (string, unit) Hashtbl.t;
  pending_value_by_source:
    (position_key, pending_source_value_edge list) Hashtbl.t;
  unknown_type_targets: (DeclId.t, unit) Hashtbl.t;
  pending_unknown_type: (position_key, unit) Hashtbl.t;
  mutable file_order: (string, int) Hashtbl.t;
  mutable file_order_valid: bool;
}

let rec find_target_by_file t ids preferred_file =
  match ids with
  | [] -> None
  | id :: rest -> (
      match Hashtbl.find_opt t.nodes id with
      | Some node when files_match node.file preferred_file -> Some id
      | _ -> find_target_by_file t rest preferred_file)

let select_target_id t ids preferred_file =
  match find_target_by_file t ids preferred_file with
  | Some id -> id
  | None -> (
      match ids with
      | id :: _ -> id
      | [] -> failwith "select_target_id: empty list")

let remove_path_index t id =
  let removals = ref [] in
  Hashtbl.iter
    (fun key ids ->
      let filtered = List.filter (fun existing_id -> existing_id <> id) ids in
      if List.length filtered <> List.length ids then
        if filtered = [] then removals := key :: !removals
        else Hashtbl.replace t.path_index key filtered)
    t.path_index;
  List.iter (fun key -> Hashtbl.remove t.path_index key) !removals

let create () =
  {
    nodes = Hashtbl.create 256;
    file_to_decls = Hashtbl.create 256;
    position_index = Hashtbl.create 512;
    path_index = Hashtbl.create 512;
    annotation_index = Hashtbl.create 512;
    forward_value = Hashtbl.create 256;
    reverse_value = Hashtbl.create 256;
    forward_type = Hashtbl.create 64;
    reverse_type = Hashtbl.create 64;
    file_digests = Hashtbl.create 64;
    dirty_files = Hashtbl.create 16;
    pending_value = Hashtbl.create 64;
    pending_type = Hashtbl.create 64;
    file_edges = Hashtbl.create 32;
    unknown_value_targets = Hashtbl.create 128;
    pending_unknown_value = Hashtbl.create 64;
    pending_unknown_value_by_path = Hashtbl.create 64;
    unknown_path_targets = Hashtbl.create 64;
    pending_value_by_source = Hashtbl.create 64;
    unknown_type_targets = Hashtbl.create 64;
    pending_unknown_type = Hashtbl.create 32;
    file_order = Hashtbl.create 32;
    file_order_valid = false;
  }

let mark_dirty t file = Hashtbl.replace t.dirty_files file ()

let get_set tbl key =
  match Hashtbl.find_opt tbl key with
  | Some set -> set
  | None -> DeclIdSet.empty

let add_edge tbl src dest =
  let existing = get_set tbl src in
  if DeclIdSet.mem dest existing then ()
  else Hashtbl.replace tbl src (DeclIdSet.add dest existing)

let remove_edge tbl src dest =
  match Hashtbl.find_opt tbl src with
  | None -> ()
  | Some set ->
      let updated = DeclIdSet.remove dest set in
      if DeclIdSet.is_empty updated then Hashtbl.remove tbl src
      else Hashtbl.replace tbl src updated
let describe_node t id =
  match Hashtbl.find_opt t.nodes id with
  | Some node -> Common.Path.toString node.decl.path
  | None -> id


let connect_alias_nodes t id_a id_b =
  add_edge t.forward_value id_a id_b;
  add_edge t.reverse_value id_b id_a;
  add_edge t.forward_value id_b id_a;
  add_edge t.reverse_value id_a id_b

let column_of_lexing pos =
  let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  if column < 0 then 0 else column

let position_key (pos : Summary.position) =
  (canonicalize_file pos.file, pos.line, pos.column)

let lexing_position_key (pos : Lexing.position) =
  (canonicalize_file pos.Lexing.pos_fname, pos.Lexing.pos_lnum, column_of_lexing pos)

let find_decl_by_position t key =
  match Hashtbl.find_opt t.position_index key with
  | Some (id :: _) -> Some id
  | _ -> None

let annotation_snapshot_of_lexing_position t pos =
  Hashtbl.find_opt t.annotation_index (lexing_position_key pos)

let decl_id_for summary (decl : Summary.decl) =
  let pos = decl.loc.start_ in
  let kind_tag =
    match decl.decl_kind with
    | Value _ -> "v"
    | Exception -> "exn"
    | RecordLabel -> "label"
    | VariantCase -> "variant"
  in
  Printf.sprintf "%s#%d#%d#%d#%s#%s" summary.Summary.source_file pos.line pos.column pos.cnum
    kind_tag decl.name

let add_position_index t key id =
  let existing = match Hashtbl.find_opt t.position_index key with Some ids -> ids | None -> [] in
  Hashtbl.replace t.position_index key (id :: existing)

let remove_position_index t key id =
  match Hashtbl.find_opt t.position_index key with
  | None -> ()
  | Some ids ->
      let filtered = ids |> List.filter (fun entry -> not (DeclId.compare entry id = 0)) in
      (match filtered with
      | [] -> Hashtbl.remove t.position_index key
      | _ -> Hashtbl.replace t.position_index key filtered)

let mark_unknown_path t key =
  if trace_unknown then
    Printf.eprintf "[graph_store] mark unknown path=%s\n" key;
  Hashtbl.replace t.unknown_path_targets key ()
let has_unknown_path t key = Hashtbl.mem t.unknown_path_targets key

let mark_unknown_value_ref t id =
  Hashtbl.replace t.unknown_value_targets id ();
  (match Hashtbl.find_opt t.nodes id with
  | Some node when trace_unknown || should_debug node.file ->
      Printf.eprintf
        "[graph_store] [%s] unknown_value_count=%d target=%s\n"
        node.file (Hashtbl.length t.unknown_value_targets)
        (Common.Path.toString node.decl.path)
  | _ -> ())

let mark_unknown_value_ref_and_aliases t id =
  mark_unknown_value_ref t id;
  match Hashtbl.find_opt t.nodes id with
  | Some node ->
      let key = path_key node.summary.path in
      mark_unknown_path t key;
      let aliases =
        Hashtbl.find_opt t.path_index key |> Option.value ~default:[]
      in
      List.iter
        (fun other_id ->
          if other_id <> id then mark_unknown_value_ref t other_id)
        aliases
  | None -> ()

let has_unknown_value_ref t id =
  let result = Hashtbl.mem t.unknown_value_targets id in
  (match Hashtbl.find_opt t.nodes id with
  | Some node when trace_unknown || should_debug node.file ->
      Printf.eprintf
        "[graph_store] [%s] query unknown_value target=%s result=%b\n"
        node.file (Common.Path.toString node.decl.path) result
  | _ -> ());
  result
let unknown_value_ids t =
  Hashtbl.fold (fun id () acc -> id :: acc) t.unknown_value_targets []
let log_pending_unknown key msg =
  if trace_unknown then
    let file, line, column = key in
    Printf.eprintf "[graph_store] pending_unknown %s key=(%s,%d,%d)\n" msg file line column

let add_pending_unknown_value t key =
  log_pending_unknown key "add";
  Hashtbl.replace t.pending_unknown_value key ()
let add_pending_unknown_value_by_path t key =
  if trace_unknown then Printf.eprintf "[graph_store] pending_unknown add path=%s\n" key;
  Hashtbl.replace t.pending_unknown_value_by_path key ()
let resolve_pending_unknown_value_by_path t key id =
  if Hashtbl.mem t.pending_unknown_value_by_path key then (
    Hashtbl.remove t.pending_unknown_value_by_path key;
    if trace_unknown then
      Printf.eprintf "[graph_store] pending_unknown resolve path=%s id=%s\n" key id;
    mark_unknown_value_ref_and_aliases t id)

let mark_unknown_type_ref t id =
  Hashtbl.replace t.unknown_type_targets id ()

let mark_unknown_type_ref_and_aliases t id =
  mark_unknown_type_ref t id;
  if trace_unknown then
    Printf.eprintf "[graph_store] mark unknown type target=%s\n" id;
  match Hashtbl.find_opt t.nodes id with
  | Some node ->
      let key = path_key node.summary.path in
      mark_unknown_path t key;
      let aliases =
        Hashtbl.find_opt t.path_index key |> Option.value ~default:[]
      in
      List.iter
        (fun other_id ->
          if other_id <> id then mark_unknown_type_ref t other_id)
        aliases
  | None -> ()

let has_unknown_type_ref t id = Hashtbl.mem t.unknown_type_targets id
let add_pending_unknown_type t key =
  if trace_unknown then
    let file, line, column = key in
    Printf.eprintf
      "[graph_store] pending_unknown_type add key=(%s,%d,%d)\n"
      file line column;
  Hashtbl.replace t.pending_unknown_type key ()

let record_file_edge t ~from_file ~to_file =
  let existing = match Hashtbl.find_opt t.file_edges from_file with Some s -> s | None -> StringSet.empty in
  let updated = StringSet.add to_file existing in
  if not (StringSet.equal updated existing) then (
    Hashtbl.replace t.file_edges from_file updated;
    t.file_order_valid <- false)

let source_has_side_effects t id =
  match Hashtbl.find_opt t.nodes id with
  | Some node -> (
      match node.decl.declKind with
      | Common.DeclKind.Value {isToplevel; sideEffects} ->
          if trace_unknown then
            Printf.eprintf
              "[graph_store] side_effect_check id=%s path=%s isTop=%b side=%b\n"
              id (describe_node t id) isToplevel sideEffects;
          isToplevel && sideEffects
      | _ -> false)
  | None -> false

let is_exception_target t id =
  match Hashtbl.find_opt t.nodes id with
  | Some node -> (
      match node.decl.declKind with
      | Common.DeclKind.Exception -> true
      | _ -> false)
  | None -> false

let add_value_edge_between t ~source_file ~reason ~src ~target ~add_file_edge =
  add_edge t.forward_value src target;
  add_edge t.reverse_value target src;
  if should_debug source_file then
    Printf.eprintf "[graph_store] [%s] add edge %s -> %s (%s)\n" source_file
      (describe_node t src) (describe_node t target) reason;
  if source_has_side_effects t src then (
    if trace_unknown then
      Printf.eprintf
        "[graph_store] [%s] side_effect_source %s -> %s (%s)\n"
        source_file (describe_node t src) (describe_node t target) reason;
    mark_unknown_value_ref_and_aliases t target);
  if is_exception_target t target then mark_unknown_value_ref_and_aliases t target;
  if add_file_edge then
    match (Hashtbl.find_opt t.nodes src, Hashtbl.find_opt t.nodes target) with
    | Some from_node, Some to_node
      when not (String.equal from_node.file to_node.file) ->
        record_file_edge t ~from_file:from_node.file ~to_file:to_node.file
    | _ -> ()

let add_pending_source_value t key entry =
  let existing =
    Hashtbl.find_opt t.pending_value_by_source key |> Option.value ~default:[]
  in
  Hashtbl.replace t.pending_value_by_source key (entry :: existing);
  if trace_unknown then (
    let file, line, column = key in
    Printf.eprintf
      "[graph_store] pending_source add key=(%s,%d,%d) target=%s\n" file line
      column (DeclId.to_string entry.target))

let resolve_pending_source_value t key source_id =
  match Hashtbl.find_opt t.pending_value_by_source key with
  | None -> ()
  | Some pending ->
      Hashtbl.remove t.pending_value_by_source key;
      List.iter
        (fun {target; add_file_edge; summary_file} ->
          add_value_edge_between t ~source_file:summary_file ~reason:"pending-source"
            ~src:source_id ~target ~add_file_edge)
        pending

let remove_all_edges t id =
  (match Hashtbl.find_opt t.forward_value id with
  | Some succs ->
      DeclIdSet.iter
        (fun dest ->
          remove_edge t.reverse_value dest id)
        succs;
      Hashtbl.remove t.forward_value id
  | None -> ());
  (match Hashtbl.find_opt t.forward_type id with
  | Some succs ->
      DeclIdSet.iter
        (fun dest ->
          remove_edge t.reverse_type dest id)
        succs;
      Hashtbl.remove t.forward_type id
  | None -> ());
  (match Hashtbl.find_opt t.reverse_value id with
  | Some preds ->
      DeclIdSet.iter
        (fun src ->
          remove_edge t.forward_value src id)
        preds;
      Hashtbl.remove t.reverse_value id
  | None -> ());
  (match Hashtbl.find_opt t.reverse_type id with
  | Some preds ->
      DeclIdSet.iter
        (fun src ->
          remove_edge t.forward_type src id)
        preds;
      Hashtbl.remove t.reverse_type id
  | None -> ())

let add_path_index t ~file path id =
  let key = path_key path in
  let existing = Hashtbl.find_opt t.path_index key |> Option.value ~default:[] in
  if trace_unknown then
    Printf.eprintf "[graph_store] [%s] add path index %s -> %s\n" file key id;
  Hashtbl.replace t.path_index key (id :: existing);
  (match Hashtbl.find_opt t.nodes id with
  | Some node ->
      List.iter
        (fun other_id ->
          if other_id <> id then
            match Hashtbl.find_opt t.nodes other_id with
            | Some other_node ->
                if alias_candidate node.file other_node.file then (
                  if trace_unknown then
                    Printf.eprintf
                      "[graph_store] alias link %s <-> %s\n"
                      node.file other_node.file;
                  connect_alias_nodes t id other_id)
            | None -> ())
        existing
  | None -> ());
  resolve_pending_unknown_value_by_path t key id;
  if has_unknown_path t key then
    match Hashtbl.find_opt t.nodes id with
    | Some node -> (
        match node.decl.declKind with
        | Common.DeclKind.Value _ -> mark_unknown_value_ref_and_aliases t id
        | _ -> mark_unknown_type_ref_and_aliases t id)
    | None -> ()

let remove_file t file =
  match Hashtbl.find_opt t.file_to_decls file with
  | None -> ()
  | Some decls ->
      Hashtbl.remove t.file_to_decls file;
      List.iter
        (fun id ->
          remove_all_edges t id;
          (match Hashtbl.find_opt t.nodes id with
          | Some node ->
              let key = position_key node.summary.loc.start_ in
              remove_position_index t key id;
              Hashtbl.remove t.annotation_index key
          | None -> ());
          remove_path_index t id;
          Hashtbl.remove t.nodes id;
          Hashtbl.remove t.unknown_value_targets id;
          Hashtbl.remove t.unknown_type_targets id)
        decls;
      let edges_removed = ref false in
      if Hashtbl.mem t.file_edges file then (
        Hashtbl.remove t.file_edges file;
        edges_removed := true);
      let pending_updates = ref [] in
      let pending_removals = ref [] in
      Hashtbl.iter
        (fun src dests ->
          if StringSet.mem file dests then (
            let updated = StringSet.remove file dests in
            edges_removed := true;
            if StringSet.is_empty updated then pending_removals := src :: !pending_removals
            else pending_updates := (src, updated) :: !pending_updates))
        t.file_edges;
      List.iter (fun src -> Hashtbl.remove t.file_edges src) !pending_removals;
      List.iter (fun (src, set) -> Hashtbl.replace t.file_edges src set) !pending_updates;
      if !edges_removed then t.file_order_valid <- false;
      let remove_pending table =
        let keys = ref [] in
        Hashtbl.iter
          (fun key _ ->
            let file_key, _, _ = key in
            if String.equal file_key file then
              keys := key :: !keys)
          table;
        List.iter (fun key -> Hashtbl.remove table key) !keys
      in
      remove_pending t.pending_unknown_value;
      remove_pending t.pending_unknown_type;
      remove_pending t.pending_value_by_source

let resolve_pending_value t key target_id =
  match Hashtbl.find_opt t.pending_value key with
  | None -> ()
  | Some pending ->
      List.iter
        (fun (src, add_file_edge) ->
          let source_file =
            match Hashtbl.find_opt t.nodes src with
            | Some node -> node.file
            | None -> (
                match Hashtbl.find_opt t.nodes target_id with
                | Some node -> node.file
                | None -> "")
          in
          add_value_edge_between t ~source_file ~reason:"pending-target" ~src
            ~target:target_id ~add_file_edge)
        pending;
      Hashtbl.remove t.pending_value key;
      if Hashtbl.mem t.pending_unknown_value key then (
        Hashtbl.remove t.pending_unknown_value key;
        mark_unknown_value_ref_and_aliases t target_id)

let resolve_pending_type t key target_id =
  match Hashtbl.find_opt t.pending_type key with
  | None -> ()
  | Some pending ->
      if trace_unknown then
        let file, line, column = key in
        Printf.eprintf
          "[graph_store] resolve pending type target=%s key=(%s,%d,%d) \
           sources=%d\n"
          target_id file line column (List.length pending);
      List.iter
        (fun src ->
          if trace_unknown then
            Printf.eprintf
              "[graph_store] resolve pending type edge %s -> %s\n"
              src target_id;
          add_edge t.forward_type src target_id;
          add_edge t.reverse_type target_id src)
        pending;
      Hashtbl.remove t.pending_type key;
      if Hashtbl.mem t.pending_unknown_type key then (
        if trace_unknown then
          let file, line, column = key in
          Printf.eprintf
            "[graph_store] resolve pending unknown type key=(%s,%d,%d) \
             target=%s\n"
            file line column target_id;
        Hashtbl.remove t.pending_unknown_type key;
        mark_unknown_type_ref_and_aliases t target_id)

let add_decls t summary =
  let decl_ids =
    summary.Summary.decls
    |> List.map (fun decl ->
           let id = decl_id_for summary decl in
           let common_decl = Summary.to_common_decl decl in
           let node = {id; file = summary.source_file; decl = common_decl; summary = decl} in
           let start_key = position_key decl.loc.start_ in
           let () =
             if trace_unknown then
               let s_file, s_line, s_column = start_key in
               Printf.eprintf
                 "[graph_store] [%s] index decl %s key=(%s,%d,%d)\n"
                 summary.source_file decl.name s_file s_line s_column
             else ()
           in
           let () =
             if should_debug summary.source_file then
               let end_key = position_key decl.loc.end_ in
               let s_file, s_line, s_column = start_key in
               let e_file, e_line, e_column = end_key in
               Printf.eprintf
                 "[graph_store] [%s] add decl %s key=(%s,%d,%d) end=(%s,%d,%d)\n"
                 summary.source_file decl.name s_file s_line s_column e_file
                 e_line e_column
           in
           Hashtbl.replace t.nodes id node;
          add_path_index t ~file:summary.source_file decl.Summary.path id;
           add_position_index t start_key id;
           Hashtbl.replace t.annotation_index start_key decl.annotations;
           resolve_pending_value t start_key id;
           resolve_pending_type t start_key id;
           if Hashtbl.mem t.pending_unknown_value start_key then (
             Hashtbl.remove t.pending_unknown_value start_key;
            log_pending_unknown start_key "resolve";
            mark_unknown_value_ref_and_aliases t id);
           if Hashtbl.mem t.pending_unknown_type start_key then (
             Hashtbl.remove t.pending_unknown_type start_key;
             mark_unknown_type_ref_and_aliases t id);
          resolve_pending_source_value t start_key id;
           id)
  in
  Hashtbl.replace t.file_to_decls summary.source_file decl_ids;
  decl_ids

let position_in_range (range : Summary.range) (pos : Summary.position) =
  if not (files_match range.start_.file pos.file) then false
  else
    let start_cnum = range.start_.cnum
    and end_cnum = range.end_.cnum
    and cnum = pos.cnum in
    cnum >= start_cnum && cnum <= end_cnum

let line_fallback_tolerance = 5

let find_local_decl ?(allow_line_fallback = false) t summary decl_ids pos =
  let key = position_key pos in
  let log_hit reason id =
    if trace_unknown then
      Printf.eprintf
        "[graph_store] [%s] find_local_decl %s hit pos=%s id=%s\n"
        summary.Summary.source_file reason (format_summary_pos pos) id
  in
  let result =
    match find_decl_by_position t key with
    | Some id ->
        log_hit "position_index" id;
        Some id
    | None ->
        let rec loop decls ids =
          match (decls, ids) with
          | decl :: rest, id :: id_rest ->
              if position_in_range decl.Summary.loc pos then (
                log_hit "range" id;
                Some id)
              else loop rest id_rest
          | _ -> None
        in
        let result = loop summary.Summary.decls decl_ids in
        (match (result, allow_line_fallback) with
        | None, true ->
            let line_candidate =
              let rec loop decls ids candidate =
                match (decls, ids) with
                | decl :: rest, id :: id_rest ->
                    let same_file =
                      files_match decl.Summary.loc.start_.file pos.file
                    in
                    let candidate =
                      if same_file && decl.Summary.loc.start_.line <= pos.line then
                        Some (id, decl.Summary.loc.start_.line)
                      else candidate
                    in
                    loop rest id_rest candidate
                | _ -> candidate
              in
              loop summary.Summary.decls decl_ids None
            in
            (match line_candidate with
            | Some (id, line) ->
                let delta = pos.line - line in
                if delta >= 0 && delta <= line_fallback_tolerance then (
                  log_hit
                    (Printf.sprintf "line_fallback line=%d delta=%d" line delta)
                    id;
                  Some id)
                else None
            | None ->
                (if trace_unknown || should_debug summary.source_file then
                   let file, line, column = key in
                   Printf.eprintf
                     "[graph_store] [%s] failed to locate decl for pos %s \
                      (key=%s,%d,%d)\n"
                     summary.source_file
                     (format_summary_pos pos)
                     file line column);
                None)
        | _ -> result)
  in
  (match result with
  | None when trace_unknown ->
      Printf.eprintf
        "[graph_store] [%s] find_local_decl final failure pos=%s allow_line=%b\n"
        summary.source_file (format_summary_pos pos) allow_line_fallback
  | _ -> ());
  result

let find_decl_ids_by_exact_start summary decl_ids (pos : Summary.position) =
  let rec loop decls ids acc =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let start = decl.Summary.loc.start_ in
        let same_file = files_match start.Summary.file pos.file in
        let acc =
          if same_file
             && start.Summary.line = pos.line
             && start.Summary.column = pos.column
          then id :: acc
          else acc
        in
        loop rest id_rest acc
    | _ -> acc
  in
  loop summary.Summary.decls decl_ids []

let find_decl_ids_by_line summary decl_ids (pos : Summary.position) =
  let rec loop decls ids acc =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let start = decl.Summary.loc.start_ in
        let same_file = files_match start.Summary.file pos.file in
        let acc =
          if same_file && start.Summary.line = pos.line then id :: acc else acc
        in
        loop rest id_rest acc
    | _ -> acc
  in
  loop summary.Summary.decls decl_ids []

let find_decl_ids_by_range summary decl_ids (pos : Summary.position) =
  let rec loop decls ids acc =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let acc =
          if position_in_range decl.Summary.loc pos then id :: acc else acc
        in
        loop rest id_rest acc
    | _ -> acc
  in
  loop summary.Summary.decls decl_ids []

let find_decl_ids_by_path summary decl_ids target_path =
  if trace_unknown then
    Printf.eprintf
      "[graph_store] [%s] path lookup target=%s decls=%d\n"
      summary.Summary.source_file
      (String.concat "." target_path)
      (List.length summary.Summary.decls);
  let rec loop decls ids acc =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let matches = path_matches_decl decl.Summary.path target_path in
        if trace_unknown then
          Printf.eprintf
            "[graph_store] [%s] path candidate %s vs %s -> %b\n"
            summary.Summary.source_file
            (String.concat "." decl.Summary.path)
            (String.concat "." target_path) matches;
        let acc = if matches then id :: acc else acc in
        loop rest id_rest acc
    | _ -> acc
  in
  loop summary.Summary.decls decl_ids []

let find_decl_by_preceding_line summary decl_ids (pos : Summary.position) =
  let rec loop decls ids candidate =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let start = decl.Summary.loc.start_ in
        let same_file = files_match start.Summary.file pos.file in
        let candidate =
          if same_file && start.Summary.line <= pos.line then Some id else candidate
        in
        loop rest id_rest candidate
    | _ -> candidate
  in
  loop summary.Summary.decls decl_ids None

let find_decl_ids_by_name summary decl_ids target_name =
  let rec loop decls ids acc =
    match (decls, ids) with
    | decl :: rest, id :: id_rest ->
        let acc =
          if String.equal decl.Summary.name target_name then id :: acc else acc
        in
        loop rest id_rest acc
    | _ -> acc
  in
  loop summary.Summary.decls decl_ids []

let add_value_reference t summary decl_ids ref =
  let source_file = summary.Summary.source_file in
  let debug = should_debug source_file in
  let describe id = describe_node t id in
  let source_opt =
    match find_local_decl t summary decl_ids ref.Summary.loc_from with
    | Some id -> Some (id, `Direct)
    | None -> (
        match
          find_local_decl ~allow_line_fallback:true t summary decl_ids
            ref.Summary.loc_from
        with
        | Some id -> Some (id, `Fallback)
        | None -> None)
  in
  match source_opt with
  | None ->
      let key = position_key ref.loc_to in
      let source_key = position_key ref.Summary.loc_from in
      let enqueue_pending target =
        add_pending_source_value t source_key
          {target; add_file_edge = ref.add_file_edge; summary_file = source_file}
      in
      if trace_unknown then
        Printf.eprintf
          "[graph_store] [%s] trace value-ref source %s -> %s missing target=%s\n"
          source_file
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to)
          (match ref.Summary.target_path with
          | Some path -> String.concat "." path
          | None -> "<none>");
      if debug then
        let file, line, column = key in
        Printf.eprintf
          "[graph_store] [%s] missing value-ref source %s -> %s (key=%s,%d,%d)\n"
          source_file
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to)
          file line column;
      if trace_unknown then
        Printf.eprintf
          "[graph_store] [%s] missing ref target_path_present=%b for %s -> %s\n"
          source_file
          (Option.is_some ref.Summary.target_path)
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to);
      let same_target_file =
        files_match ref.Summary.loc_to.file summary.source_file
      in
      let path_matches =
        match ref.Summary.target_path with
        | Some path ->
            let local =
              if same_target_file then find_decl_ids_by_path summary decl_ids path
              else []
            in
            let global =
              Hashtbl.find_opt t.path_index (path_key path)
              |> Option.value ~default:[]
            in
            let combined =
              if same_target_file then local @ global else global
              |> List.sort_uniq String.compare
            in
            if trace_unknown then
              Printf.eprintf
                "[graph_store] [%s] target path %s -> %s path=%s local=%d \
                 global=%d combined=%d\n"
                source_file
                (format_summary_pos ref.Summary.loc_from)
                (format_summary_pos ref.Summary.loc_to)
                (String.concat "." path)
                (List.length local) (List.length global)
                (List.length combined);
            combined
        | None ->
            if trace_unknown then
              Printf.eprintf
                "[graph_store] [%s] target path for %s -> %s = <none>\n"
                source_file
                (format_summary_pos ref.Summary.loc_from)
                (format_summary_pos ref.Summary.loc_to);
            []
      in
      let name_matches =
        match ref.Summary.target_path with
        | Some path when same_target_file -> (
            match List.rev path with
            | name :: _ -> find_decl_ids_by_name summary decl_ids name
            | [] -> [])
        | _ -> []
      in
      if trace_unknown then
        Printf.eprintf
          "[graph_store] [%s] path matches for %s -> %s count=%d\n"
          source_file
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to)
          (List.length path_matches);
      if trace_unknown then
        Printf.eprintf
          "[graph_store] [%s] name matches for %s -> %s count=%d\n"
          source_file
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to)
          (List.length name_matches);
      if trace_unknown then
        let exists = Hashtbl.mem t.position_index key in
        Printf.eprintf
          "[graph_store] [%s] position_index mem=%b for %s -> %s\n"
          source_file exists
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to);
      let pending_path_key =
        Option.map path_key ref.Summary.target_path
      in
      (match Hashtbl.find_opt t.position_index key with
      | Some (target :: _) ->
          mark_unknown_value_ref_and_aliases t target;
          enqueue_pending target;
          if trace_unknown then
            Printf.eprintf
              "[graph_store] [%s] trace value-ref missing source %s -> %s resolved via index %s\n"
              source_file
              (format_summary_pos ref.Summary.loc_from)
              (format_summary_pos ref.Summary.loc_to)
              (describe target);
          if debug then
            Printf.eprintf
              "[graph_store] [%s] marked unknown value ref for %s\n"
              source_file (describe target)
      | _ ->
          if trace_unknown then
            let file, line, column = key in
            Printf.eprintf
              "[graph_store] [%s] target index miss %s -> %s key=(%s,%d,%d)\n"
              source_file
              (format_summary_pos ref.Summary.loc_from)
              (format_summary_pos ref.Summary.loc_to)
              file line column;
          if trace_unknown then (
            let k_file, k_line, k_column = key in
            Printf.eprintf
              "[graph_store] [%s] no index entry for %s -> %s key=(%s,%d,%d)\n"
              source_file
              (format_summary_pos ref.Summary.loc_from)
              (format_summary_pos ref.Summary.loc_to)
              k_file k_line k_column;
            Hashtbl.iter
              (fun (file, line, column) ids ->
                if String.equal file k_file && line = k_line then
                  Printf.eprintf
                    "[graph_store] [%s] candidate key=(%s,%d,%d) ids=%d\n"
                    source_file file line column (List.length ids))
              t.position_index);
          let fallback_target =
            if same_target_file then
              find_local_decl ~allow_line_fallback:true t summary decl_ids
                ref.Summary.loc_to
            else None
          in
          (match fallback_target with
          | Some target ->
              mark_unknown_value_ref_and_aliases t target;
              enqueue_pending target;
              if debug then
                Printf.eprintf
                  "[graph_store] [%s] marked unknown value ref via fallback for %s\n"
                  source_file (describe target)
          | None ->
              let exact_matches =
                if same_target_file then
                  find_decl_ids_by_exact_start summary decl_ids ref.Summary.loc_to
                else []
              in
              let name_matches =
                match ref.Summary.target_path with
                | Some path when same_target_file -> (
                    match List.rev path with
                    | name :: _ -> find_decl_ids_by_name summary decl_ids name
                    | [] -> [])
                | _ -> []
              in
              let line_matches =
                match (path_matches, name_matches, exact_matches) with
                | [], [], [] ->
                    if same_target_file then
                      find_decl_ids_by_line summary decl_ids ref.Summary.loc_to
                    else []
                | _ -> []
              in
              let range_matches =
                match
                  (path_matches, name_matches, exact_matches, line_matches)
                with
                | [], [], [], [] ->
                    if same_target_file then
                      find_decl_ids_by_range summary decl_ids ref.Summary.loc_to
                    else []
                | _ -> []
              in
              let preceding_match =
                match
                  ( path_matches,
                    name_matches,
                    exact_matches,
                    line_matches,
                    range_matches )
                with
                | [], [], [], [], [] ->
                    find_decl_by_preceding_line summary decl_ids ref.Summary.loc_to
                | _ -> None
              in
              let target_opt =
                match path_matches with
                | id :: _ -> Some id
                | [] -> (
                  match name_matches with
                  | id :: _ -> Some id
                  | [] -> (
                    match exact_matches with
                    | id :: _ -> Some id
                    | [] -> (
                      match line_matches with
                      | id :: _ -> Some id
                      | [] -> (
                        match range_matches with
                        | id :: _ -> Some id
                        | [] -> preceding_match))))
              in
              (match target_opt with
              | Some target ->
                  mark_unknown_value_ref_and_aliases t target;
                  enqueue_pending target;
                  if trace_unknown && path_matches <> [] then
                    Printf.eprintf
                      "[graph_store] [%s] trace value-ref inferred target %s -> %s via path %s\n"
                      source_file
                      (format_summary_pos ref.Summary.loc_from)
                      (format_summary_pos ref.Summary.loc_to)
                      (describe target);
                  if debug then
                    Printf.eprintf
                      "[graph_store] [%s] marked unknown value ref via line fallback for %s\n"
                      source_file (describe target)
              | None ->
                  if trace_unknown || debug then
                    let file, line, column = key in
                    Printf.eprintf
                      "[graph_store] [%s] pending target for %s (key=%s,%d,%d) \
                       path_matches=%d name_matches=%d exact_matches=%d \
                       line_matches=%d range_matches=%d preceding=%b\n"
                      source_file
                      (format_summary_pos ref.Summary.loc_to)
                      file line column (List.length path_matches)
                      (List.length name_matches)
                      (List.length exact_matches)
                      (List.length line_matches)
                      (List.length range_matches)
                      (Option.is_some preceding_match);
                  add_pending_unknown_value t key;
                  Option.iter
                    (fun path_key ->
                      if trace_unknown then
                        Printf.eprintf
                          "[graph_store] [%s] pending target path=%s for %s -> %s\n"
                          source_file path_key
                          (format_summary_pos ref.Summary.loc_from)
                          (format_summary_pos ref.Summary.loc_to);
                      add_pending_unknown_value_by_path t path_key)
                    pending_path_key)))
  | Some (src, _) ->
      if trace_unknown then
        Printf.eprintf
          "[graph_store] [%s] trace value-ref source %s -> %s resolved via %s\n"
          source_file
          (format_summary_pos ref.Summary.loc_from)
          (format_summary_pos ref.Summary.loc_to)
          (describe src);
      let key = position_key ref.loc_to in
      (match Hashtbl.find_opt t.position_index key with
      | Some (target :: _) ->
          add_value_edge_between t ~source_file ~reason:"index" ~src ~target
            ~add_file_edge:ref.add_file_edge
      | _ ->
          let fallback_target =
            if files_match ref.Summary.loc_to.file summary.source_file then
              find_local_decl ~allow_line_fallback:true t summary decl_ids
                ref.Summary.loc_to
            else None
          in
          (match fallback_target with
          | Some target ->
              if trace_unknown then
                Printf.eprintf
                  "[graph_store] [%s] trace value-ref fallback %s -> %s via %s\n"
                  source_file
                  (format_summary_pos ref.Summary.loc_from)
                  (format_summary_pos ref.Summary.loc_to)
                  (describe target);
              add_value_edge_between t ~source_file ~reason:"fallback" ~src
                ~target ~add_file_edge:ref.add_file_edge
          | None ->
          if debug then
            let file, line, column = key in
            Printf.eprintf
              "[graph_store] [%s] unresolved value-ref target %s (key=%s,%d,%d)\n"
              source_file
              (format_summary_pos ref.Summary.loc_to)
              file line column;
          let pending =
            match Hashtbl.find_opt t.pending_value key with
            | Some list -> (src, ref.add_file_edge) :: list
            | None -> [ (src, ref.add_file_edge) ]
          in
          Hashtbl.replace t.pending_value key pending))

let add_type_reference t summary decl_ids ref =
  let source_file = summary.Summary.source_file in
  let debug = should_debug source_file in
  let describe_node id =
    match Hashtbl.find_opt t.nodes id with
    | Some node -> Common.Path.toString node.decl.path
    | None -> id
  in
  let log_edge reason src target =
    if debug then
      Printf.eprintf
        "[graph_store] [%s] add type edge %s -> %s (%s)\n"
        source_file (describe_node src) (describe_node target) reason
  in
  let source_opt =
    match find_local_decl t summary decl_ids ref.Summary.pos_from with
    | Some id -> Some id
    | None ->
        find_local_decl ~allow_line_fallback:true t summary decl_ids ref.Summary.pos_from
  in
  let ghost_source = String.equal ref.Summary.pos_from.file "_none_" in
  if ghost_source then (
    let key = position_key ref.pos_to in
    let candidates =
      let exact =
        find_decl_ids_by_exact_start summary decl_ids ref.Summary.pos_to
      in
      if exact <> [] then exact
      else find_decl_ids_by_line summary decl_ids ref.Summary.pos_to
    in
    if candidates <> [] then
      List.iter
        (fun target ->
          mark_unknown_type_ref_and_aliases t target;
          if debug then
            Printf.eprintf
              "[graph_store] [%s] marked unknown type ref (ghost) for %s\n"
              source_file (describe_node target))
        candidates
    else (
      add_pending_unknown_type t key;
      if debug then
        let file, line, column = key in
        Printf.eprintf
          "[graph_store] [%s] pending unknown type target key=%s,%d,%d (ghost)\n"
          source_file file line column))
  else
    match source_opt with
    | None ->
      let key = position_key ref.pos_to in
      if debug then
        let file, line, column = key in
        Printf.eprintf
          "[graph_store] [%s] missing type-ref source %s (target key=%s,%d,%d)\n"
          source_file
          (format_summary_pos ref.Summary.pos_from)
          file line column;
      let index_entry = Hashtbl.find_opt t.position_index key in
      let candidates =
        match index_entry with
        | Some ids -> ids
        | None ->
            let exact =
              find_decl_ids_by_exact_start summary decl_ids ref.Summary.pos_to
            in
            if exact <> [] then exact
            else
              let same_line =
                find_decl_ids_by_line summary decl_ids ref.Summary.pos_to
              in
              if same_line <> [] then same_line
              else
                let fallback =
                  match
                    find_local_decl ~allow_line_fallback:true t summary decl_ids
                      ref.Summary.pos_to
                  with
                  | Some id -> [id]
                  | None -> []
                in
                fallback
      in
      if candidates <> [] then
        List.iter
          (fun target ->
            mark_unknown_type_ref_and_aliases t target;
            if debug then
              Printf.eprintf
                "[graph_store] [%s] marked unknown type ref for %s\n"
                source_file (describe_node target))
          candidates
      else (
        add_pending_unknown_type t key;
        if debug then
          let file, line, column = key in
          Printf.eprintf
            "[graph_store] [%s] pending unknown type target key=%s,%d,%d\n"
            source_file file line column)
    | Some src ->
      let key = position_key ref.pos_to in
      let has_side_effects = source_has_side_effects t src in
      let fuzzy_source =
        match Hashtbl.find_opt t.nodes src with
        | Some node ->
            let start_pos = node.summary.loc.start_ in
            let same_file =
              files_match start_pos.file ref.Summary.pos_from.file
            in
            let cross_module =
              not (same_module start_pos.file ref.Summary.pos_to.file)
            in
            (same_file && not (positions_equal start_pos ref.Summary.pos_from))
            || cross_module
        | None -> false
      in
      (match Hashtbl.find_opt t.position_index key with
      | Some ids ->
          let target = select_target_id t ids ref.Summary.pos_to.file in
          add_edge t.forward_type src target;
          add_edge t.reverse_type target src;
          log_edge "index" src target;
          if has_side_effects || fuzzy_source then (
            if trace_unknown then
              Printf.eprintf
                "[graph_store] [%s] mark type target=%s source=%s side=%b \
                 fuzzy=%b\n"
                source_file (describe_node target) (describe_node src)
                has_side_effects fuzzy_source;
            mark_unknown_type_ref_and_aliases t target
          )
      | _ ->
          let pending =
            match Hashtbl.find_opt t.pending_type key with
            | Some list -> src :: list
            | None -> [src]
          in
          if debug then
            let file, line, column = key in
            Printf.eprintf
              "[graph_store] [%s] pending type target (%s) key=%s,%d,%d\n"
              source_file (describe_node src) file line column;
          Hashtbl.replace t.pending_type key pending;
          if trace_unknown then
            Printf.eprintf
              "[graph_store] [%s] pending type source %s side_effects=%b \
               fuzzy=%b\n"
              source_file (describe_node src) has_side_effects fuzzy_source;
          if has_side_effects || fuzzy_source then add_pending_unknown_type t key)

let add_summary t summary =
  let file = summary.Summary.source_file in
  let digest_changed =
    match Hashtbl.find_opt t.file_digests file with
    | Some existing when String.equal existing summary.digest -> false
    | _ -> true
  in
  if not digest_changed then ()
  else (
    if should_debug file then
      Printf.eprintf
        "[graph_store] [%s] summary refs: value=%d type=%d decls=%d\n"
        file
        (List.length summary.value_references)
        (List.length summary.type_references)
        (List.length summary.decls);
    remove_file t file;
    let decl_ids = add_decls t summary in
    List.iter (add_value_reference t summary decl_ids) summary.value_references;
    List.iter (add_type_reference t summary decl_ids) summary.type_references;
    if should_debug file then (
      let describe id =
        match Hashtbl.find_opt t.nodes id with
        | Some node -> Common.Path.toString node.decl.path
        | None -> id
      in
      let unknowns =
        Hashtbl.fold (fun id _ acc -> describe id :: acc) t.unknown_type_targets []
      in
      Printf.eprintf
        "[graph_store] [%s] unknown type targets: [%s]\n"
        file (String.concat ", " unknowns));
    Hashtbl.replace t.file_digests file summary.digest;
    mark_dirty t file;
    t.file_order_valid <- false)

let get_dirty_files t =
  let files = Hashtbl.fold (fun file () acc -> file :: acc) t.dirty_files [] in
  Hashtbl.reset t.dirty_files;
  files

let find_node t id = Hashtbl.find_opt t.nodes id

let find_node_by_position t pos =
  match Hashtbl.find_opt t.position_index (lexing_position_key pos) with
  | Some (id :: _) -> Hashtbl.find_opt t.nodes id
  | _ -> None

let file_decls t file = match Hashtbl.find_opt t.file_to_decls file with Some ids -> ids | None -> []

let successors t ~kind id =
  match kind with
  | `Value -> get_set t.forward_value id
  | `Type -> get_set t.forward_type id

let reverse_successors t ~kind id =
  match kind with
  | `Value -> get_set t.reverse_value id
  | `Type -> get_set t.reverse_type id

let frontier t ~changed_files =
  let rec bfs visited queue =
    match queue with
    | [] -> visited
    | id :: rest ->
        if DeclIdSet.mem id visited then bfs visited rest
        else
          let visited = DeclIdSet.add id visited in
          let predecessors =
            DeclIdSet.union
              (reverse_successors t ~kind:`Value id)
              (reverse_successors t ~kind:`Type id)
          in
          let rest =
            DeclIdSet.fold
              (fun pred acc -> pred :: acc)
              predecessors rest
          in
          bfs visited rest
  in
  let seeds =
    changed_files
    |> List.fold_left
         (fun acc file ->
           file_decls t file |> List.fold_left (fun acc id -> id :: acc) acc)
         []
  in
  bfs DeclIdSet.empty seeds

let pop_min_file set_ref =
  if FileSet.is_empty !set_ref then None
  else
    let file = FileSet.min_elt !set_ref in
    set_ref := FileSet.remove file !set_ref;
    Some file

let recompute_file_order t =
  let incoming = Hashtbl.create 64 in
  let add_file file =
    if not (Hashtbl.mem incoming file) then Hashtbl.add incoming file 0
  in
  Hashtbl.iter (fun file _ -> add_file file) t.file_to_decls;
  Hashtbl.iter
    (fun from_file dests ->
      add_file from_file;
      StringSet.iter
        (fun to_file ->
          add_file to_file;
          let current = Hashtbl.find incoming to_file in
          Hashtbl.replace incoming to_file (current + 1))
        dests)
    t.file_edges;
  let ready =
    Hashtbl.fold
      (fun file count acc ->
        if count = 0 then FileSet.add file acc else acc)
      incoming FileSet.empty
  in
  let ready_ref = ref ready in
  let ranks = Hashtbl.create 64 in
  let current_rank = ref 0 in
  let rec process () =
    match pop_min_file ready_ref with
    | None -> ()
    | Some file ->
        incr current_rank;
        Hashtbl.replace ranks file !current_rank;
        let successors =
          match Hashtbl.find_opt t.file_edges file with
          | Some set -> set
          | None -> StringSet.empty
        in
        StringSet.iter
          (fun dest ->
            let count = Hashtbl.find incoming dest - 1 in
            Hashtbl.replace incoming dest count;
            if count = 0 then ready_ref := FileSet.add dest !ready_ref)
          successors;
        process ()
  in
  process ();
  Hashtbl.iter
    (fun file _ ->
      if not (Hashtbl.mem ranks file) then (
        incr current_rank;
        Hashtbl.replace ranks file !current_rank))
    incoming;
  t.file_order <- ranks;
  t.file_order_valid <- true

let ordered_files t =
  if not t.file_order_valid then recompute_file_order t;
  Hashtbl.copy t.file_order

let compare_decl_ids t ~ordered_files id1 id2 =
  match (find_node t id1, find_node t id2) with
  | Some node1, Some node2 ->
      Decl.compareUsingDependencies ~orderedFiles:ordered_files node1.decl
        node2.decl
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

