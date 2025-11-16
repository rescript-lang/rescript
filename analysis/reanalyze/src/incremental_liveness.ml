open Common
open Graph_store

module Decl = DeadCommon.Decl
module PosSet = DeadCommon.PosSet
module ProcessDeadAnnotations = DeadCommon.ProcessDeadAnnotations
module PosHash = DeadCommon.PosHash

let use_graph_solver =
  match Sys.getenv_opt "INCR_GRAPH_SOLVER" with
  | Some "1" -> true
  | _ -> false

let debug_target = Sys.getenv_opt "INCR_DEBUG_NAME"
let contains_substring text sub =
  let len_text = String.length text and len_sub = String.length sub in
  let rec aux i =
    i + len_sub <= len_text
    && (String.sub text i len_sub = sub || aux (i + 1))
  in
  len_sub = 0 || aux 0

let debug_decl decl message =
  match debug_target with
  | Some target ->
      let path_str = Path.toString decl.path in
      if String.equal target path_str || contains_substring path_str target then
        prerr_endline message
  | _ -> ()

type result = {
  visited: DeclIdSet.t;
  dead_declarations: Common.decl list;
}

type env = {
  graph: Graph_store.t;
  dead_acc: Common.decl list ref;
  annotations: ((string * int), Summary.annotation_snapshot) Hashtbl.t;
}

let annotation_none : Summary.annotation_snapshot =
  {
    annotated_dead = false;
    annotated_gen_type_or_live = false;
    annotated_gen_type_or_dead = false;
  }

let annotation_dead_snapshot : Summary.annotation_snapshot =
  {
    annotated_dead = true;
    annotated_gen_type_or_live = false;
    annotated_gen_type_or_dead = true;
  }

let position_key_of_lexing pos = (pos.Lexing.pos_fname, pos.Lexing.pos_cnum)

let set_annotation env pos snapshot =
  Hashtbl.replace env.annotations (position_key_of_lexing pos) snapshot

let get_annotation env pos =
  let key = position_key_of_lexing pos in
  match Hashtbl.find_opt env.annotations key with
  | Some snapshot -> snapshot
  | None ->
      let snapshot =
        match Graph_store.find_node_by_position env.graph pos with
        | Some node -> node.summary.annotations
        | None -> annotation_none
      in
      Hashtbl.replace env.annotations key snapshot;
      snapshot

let annotation_is_dead env pos = (get_annotation env pos).annotated_dead

let annotation_is_live env pos =
  (get_annotation env pos).annotated_gen_type_or_live

let annotation_is_dead_or_gen env pos =
  (get_annotation env pos).annotated_gen_type_or_dead

let decl_id_equal a b = DeclId.compare a b = 0

let incoming_refs graph id decl =
  let kind = if Decl.isValue decl then `Value else `Type in
  Graph_store.reverse_successors graph ~kind id

let successors_for_tarjan graph ~frontier ~compare_id id =
  let value_succ = Graph_store.successors graph ~kind:`Value id in
  let type_succ = Graph_store.successors graph ~kind:`Type id in
  let combined = DeclIdSet.union value_succ type_succ in
  let succs =
    DeclIdSet.fold
      (fun succ acc ->
        if DeclIdSet.mem succ frontier then succ :: acc else acc)
      combined []
  in
  List.sort compare_id succs

let rec resolve env id level refs_being_resolved =
  match Graph_store.find_node env.graph id with
  | None -> false
  | Some node ->
    let decl = node.decl in
    (match decl.resolvedDead with
    | Some is_dead -> is_dead
    | None ->
      if DeclIdSet.mem id !refs_being_resolved then true
      else (
        refs_being_resolved := DeclIdSet.add id !refs_being_resolved;
        let refs = incoming_refs env.graph id decl in
        let all_deps_resolved = ref true in
        let new_refs, unknown_live =
          DeclIdSet.fold
            (fun ref_id (acc, unknown_live) ->
              if decl_id_equal ref_id id then (acc, unknown_live)
              else
                match Graph_store.find_node env.graph ref_id with
                | None -> (acc, true)
                | Some ref_node ->
                  let ref_is_dead =
                    resolve env ref_id (level + 1) refs_being_resolved
                  in
                  if ref_node.decl.resolvedDead = None then
                    all_deps_resolved := false;
                  if ref_is_dead then (acc, unknown_live)
                  else (PosSet.add ref_node.decl.pos acc, unknown_live))
            refs (PosSet.empty, false)
        in
        let live_refs =
          new_refs |> PosSet.filter (fun pos -> not (annotation_is_dead env pos))
        in
        let is_dead =
          (not unknown_live)
          && PosSet.is_empty live_refs
          && not (annotation_is_live env decl.pos)
        in
        debug_decl decl
          (Printf.sprintf
             "decl=%s annotated_live=%b refs=%d unknown=%b"
             (Path.toString decl.path)
             (annotation_is_live env decl.pos)
             (PosSet.cardinal new_refs) unknown_live);
        let is_resolved = (not is_dead) || !all_deps_resolved || level = 0 in
        if is_resolved then (
          decl.resolvedDead <- Some is_dead;
          if is_dead then (
            if annotation_is_dead_or_gen env decl.pos then decl.report <- false;
            env.dead_acc := decl :: !(env.dead_acc);
            set_annotation env decl.pos annotation_dead_snapshot);
          if !Common.Cli.debug then
            Log_.item "%s %s %s: %d references [%d]@."
              (if is_dead then "Dead" else "Live")
              (DeclKind.toString decl.declKind)
              (Path.toString decl.path)
              (PosSet.cardinal new_refs) level);
        is_dead))

let legacy_dead_decls () =
  PosHash.fold
    (fun _ decl acc ->
      match decl.resolvedDead with
      | Some true when decl.report -> decl :: acc
      | _ -> acc)
    DeadCommon.decls []
  |> List.rev

let recompute ~graph ~changed_files =
  let frontier = Graph_store.frontier graph ~changed_files in
  if (not use_graph_solver) || DeclIdSet.is_empty frontier then
    {visited = frontier; dead_declarations = legacy_dead_decls ()}
  else
    let ordered_files = Graph_store.ordered_files graph in
    let compare_decl_ids = Graph_store.compare_decl_ids graph ~ordered_files in
    let nodes = DeclIdSet.elements frontier |> List.sort compare_decl_ids in
    let components =
      Tarjan.compute
        ~successors:(successors_for_tarjan graph ~frontier ~compare_id:compare_decl_ids)
        nodes
    in
    let components =
      components
      |> List.map (fun (component : _ Tarjan.component) ->
             {Tarjan.members = component.members |> List.sort compare_decl_ids})
      |> List.sort (fun a b ->
             match (a.Tarjan.members, b.Tarjan.members) with
             | id1 :: _, id2 :: _ -> compare_decl_ids id1 id2
             | _ -> 0)
    in
    let env = {graph; dead_acc = ref []; annotations = Hashtbl.create 1024} in
    components
    |> List.iter (fun component ->
           List.iter
             (fun id ->
               let refs_being_resolved = ref DeclIdSet.empty in
               ignore (resolve env id 0 refs_being_resolved))
             component.Tarjan.members);
    let dead_sorted =
      !(env.dead_acc) |> List.sort DeadCommon.Decl.compareForReporting
    in
    {visited = frontier; dead_declarations = dead_sorted}

