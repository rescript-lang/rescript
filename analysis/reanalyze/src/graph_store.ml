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

type node = {
  id: DeclId.t;
  file: string;
  decl: Common.decl;
  summary: Summary.decl;
}

type position_key = string * int

type pending_value_edge = DeclId.t * bool

type t = {
  nodes: (DeclId.t, node) Hashtbl.t;
  file_to_decls: (string, DeclId.t list) Hashtbl.t;
  position_index: (position_key, DeclId.t list) Hashtbl.t;
  forward_value: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  reverse_value: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  forward_type: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  reverse_type: (DeclId.t, DeclIdSet.t) Hashtbl.t;
  file_digests: (string, string) Hashtbl.t;
  dirty_files: (string, unit) Hashtbl.t;
  pending_value: (position_key, pending_value_edge list) Hashtbl.t;
  pending_type: (position_key, DeclId.t list) Hashtbl.t;
  file_edges: (string, StringSet.t) Hashtbl.t;
}

let create () =
  {
    nodes = Hashtbl.create 256;
    file_to_decls = Hashtbl.create 256;
    position_index = Hashtbl.create 512;
    forward_value = Hashtbl.create 256;
    reverse_value = Hashtbl.create 256;
    forward_type = Hashtbl.create 64;
    reverse_type = Hashtbl.create 64;
    file_digests = Hashtbl.create 64;
    dirty_files = Hashtbl.create 16;
    pending_value = Hashtbl.create 64;
    pending_type = Hashtbl.create 64;
    file_edges = Hashtbl.create 32;
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

let position_key (pos : Summary.position) = (pos.file, pos.cnum)

let lexing_position_key (pos : Lexing.position) = (pos.Lexing.pos_fname, pos.Lexing.pos_cnum)

let find_decl_by_position t key =
  match Hashtbl.find_opt t.position_index key with
  | Some (id :: _) -> Some id
  | _ -> None

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

let record_file_edge t ~from_file ~to_file =
  let existing = match Hashtbl.find_opt t.file_edges from_file with Some s -> s | None -> StringSet.empty in
  Hashtbl.replace t.file_edges from_file (StringSet.add to_file existing)

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
              remove_position_index t (position_key node.summary.loc.start_) id
          | None -> ());
          Hashtbl.remove t.nodes id)
        decls

let resolve_pending_value t key target_id =
  match Hashtbl.find_opt t.pending_value key with
  | None -> ()
  | Some pending ->
      List.iter
        (fun (src, add_file_edge) ->
          add_edge t.forward_value src target_id;
          add_edge t.reverse_value target_id src;
          if add_file_edge then (
            let from_node = Hashtbl.find t.nodes src in
            let to_node = Hashtbl.find t.nodes target_id in
            if not (String.equal from_node.file to_node.file) then
              record_file_edge t ~from_file:from_node.file ~to_file:to_node.file))
        pending;
      Hashtbl.remove t.pending_value key

let resolve_pending_type t key target_id =
  match Hashtbl.find_opt t.pending_type key with
  | None -> ()
  | Some pending ->
      List.iter
        (fun src ->
          add_edge t.forward_type src target_id;
          add_edge t.reverse_type target_id src)
        pending;
      Hashtbl.remove t.pending_type key

let add_decls t summary =
  let decl_ids =
    summary.Summary.decls
    |> List.map (fun decl ->
           let id = decl_id_for summary decl in
           let common_decl = Summary.to_common_decl decl in
           let node = {id; file = summary.source_file; decl = common_decl; summary = decl} in
           Hashtbl.replace t.nodes id node;
           add_position_index t (position_key decl.loc.start_) id;
           resolve_pending_value t (position_key decl.loc.start_) id;
           resolve_pending_type t (position_key decl.loc.start_) id;
           id)
  in
  Hashtbl.replace t.file_to_decls summary.source_file decl_ids;
  decl_ids

let position_in_range (range : Summary.range) (pos : Summary.position) =
  let start_cnum = range.start_.cnum
  and end_cnum = range.end_.cnum
  and cnum = pos.cnum in
  cnum >= start_cnum && cnum <= end_cnum

let find_local_decl t summary decl_ids pos =
  let key = position_key pos in
  match find_decl_by_position t key with
  | Some id -> Some id
  | None ->
      let rec loop decls ids =
        match (decls, ids) with
        | decl :: rest, id :: id_rest ->
            if position_in_range decl.Summary.loc pos then Some id
            else loop rest id_rest
        | _ -> None
      in
      loop summary.Summary.decls decl_ids

let add_value_reference t summary decl_ids ref =
  match find_local_decl t summary decl_ids ref.Summary.loc_from with
  | None -> ()
  | Some src ->
      let key = position_key ref.loc_to in
      (match Hashtbl.find_opt t.position_index key with
      | Some (target :: _) ->
          add_edge t.forward_value src target;
          add_edge t.reverse_value target src;
          if ref.add_file_edge then (
            let from_node = Hashtbl.find t.nodes src in
            let to_node = Hashtbl.find t.nodes target in
            if not (String.equal from_node.file to_node.file) then
              record_file_edge t ~from_file:from_node.file ~to_file:to_node.file)
      | _ ->
          let pending =
            match Hashtbl.find_opt t.pending_value key with
            | Some list -> (src, ref.add_file_edge) :: list
            | None -> [ (src, ref.add_file_edge) ]
          in
          Hashtbl.replace t.pending_value key pending)

let add_type_reference t summary decl_ids ref =
  match find_local_decl t summary decl_ids ref.Summary.pos_from with
  | None -> ()
  | Some src ->
      let key = position_key ref.pos_to in
      (match Hashtbl.find_opt t.position_index key with
      | Some (target :: _) ->
          add_edge t.forward_type src target;
          add_edge t.reverse_type target src
      | _ ->
          let pending =
            match Hashtbl.find_opt t.pending_type key with
            | Some list -> src :: list
            | None -> [src]
          in
          Hashtbl.replace t.pending_type key pending)

let add_summary t summary =
  let file = summary.Summary.source_file in
  let digest_changed =
    match Hashtbl.find_opt t.file_digests file with
    | Some existing when String.equal existing summary.digest -> false
    | _ -> true
  in
  if not digest_changed then ()
  else (
    remove_file t file;
    let decl_ids = add_decls t summary in
    List.iter (add_value_reference t summary decl_ids) summary.value_references;
    List.iter (add_type_reference t summary decl_ids) summary.type_references;
    Hashtbl.replace t.file_digests file summary.digest;
    mark_dirty t file)

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

