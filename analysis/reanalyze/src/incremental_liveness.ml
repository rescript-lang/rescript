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

type env = {graph: Graph_store.t; dead_acc: Common.decl list ref}

let decl_id_equal a b = DeclId.compare a b = 0

let incoming_refs graph id decl =
  let kind = if Decl.isValue decl then `Value else `Type in
  Graph_store.reverse_successors graph ~kind id

let successors_for_tarjan graph ~frontier id =
  let value_succ = Graph_store.successors graph ~kind:`Value id in
  let type_succ = Graph_store.successors graph ~kind:`Type id in
  let combined = DeclIdSet.union value_succ type_succ in
  DeclIdSet.fold
    (fun succ acc ->
      if DeclIdSet.mem succ frontier then succ :: acc else acc)
    combined []

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
        let is_dead =
          (not unknown_live) && DeadCommon.declIsDead ~refs:new_refs decl
        in
        debug_decl decl
          (Printf.sprintf
             "decl=%s annotated_live=%b refs=%d unknown=%b"
             (Path.toString decl.path)
             (ProcessDeadAnnotations.isAnnotatedGenTypeOrLive decl.pos)
             (PosSet.cardinal new_refs) unknown_live);
        let is_resolved = (not is_dead) || !all_deps_resolved || level = 0 in
        if is_resolved then (
          decl.resolvedDead <- Some is_dead;
          if is_dead then (
            if not (DeadCommon.doReportDead decl.pos) then decl.report <- false;
            env.dead_acc := decl :: !(env.dead_acc));
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
    let nodes = DeclIdSet.elements frontier in
    let components =
      Tarjan.compute
        ~successors:(successors_for_tarjan graph ~frontier)
        nodes
    in
    let env = {graph; dead_acc = ref []} in
    components
    |> List.iter (fun (component : _ Tarjan.component) ->
           List.iter
             (fun id ->
               let refs_being_resolved = ref DeclIdSet.empty in
               ignore (resolve env id 0 refs_being_resolved))
             component.Tarjan.members);
    let dead_sorted =
      !(env.dead_acc) |> List.sort DeadCommon.Decl.compareForReporting
    in
    {visited = frontier; dead_declarations = dead_sorted}

