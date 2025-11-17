open Common
open Cmt_format

module StringMap = Map.Make (String)

module Normalize = struct
  module DiffSet = Set.Make (String)

  type t = {
    decls: string list;
    value_refs: string list;
    type_refs: string list;
  }

  let pos_string (pos : Lexing.position) =
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Printf.sprintf "%s:%d:%d" pos.Lexing.pos_fname pos.Lexing.pos_lnum col

  let location_string (loc : Location.t) =
    Printf.sprintf "%s-%s"
      (pos_string loc.Location.loc_start)
      (pos_string loc.Location.loc_end)

  let string_of_names names =
    names |> List.map Name.toString |> String.concat "."

  let string_of_string_set set =
    set |> StringSet.elements |> List.sort String.compare |> String.concat ","

  let optional_summary (optional : OptionalArgs.t) =
    Printf.sprintf "count=%d;unused=[%s];always=[%s]" optional.count
      (string_of_string_set optional.unused)
      (string_of_string_set optional.alwaysUsed)

  let decl_kind_string = function
    | DeclKind.Exception -> "Exception"
    | RecordLabel -> "RecordLabel"
    | VariantCase -> "VariantCase"
    | Value {isToplevel; optionalArgs; sideEffects} ->
      Printf.sprintf "Value:isToplevel=%b:sideEffects=%b:%s" isToplevel
        sideEffects (optional_summary optionalArgs)

  let pos_adjustment_string = function
    | FirstVariant -> "FirstVariant"
    | OtherVariant -> "OtherVariant"
    | Nothing -> "Nothing"

  let decl_string (decl : Common.decl) =
    Printf.sprintf "path=%s|kind=%s|module=%s|pos=%s|start=%s|end=%s|adj=%s"
      (string_of_names decl.path)
      (decl_kind_string decl.declKind)
      (location_string decl.moduleLoc)
      (pos_string decl.pos)
      (pos_string decl.posStart)
      (pos_string decl.posEnd)
      (pos_adjustment_string decl.posAdjustment)

  let value_ref_string {Collected_types.loc_from; loc_to; _} =
    Printf.sprintf "%s->%s" (location_string loc_from) (location_string loc_to)

  let type_ref_string {Collected_types.pos_from; pos_to} =
    Printf.sprintf "%s->%s" (pos_string pos_from) (pos_string pos_to)

  let normalize collected =
    let decls =
      collected.Collected_types.decls |> List.map decl_string
      |> List.sort String.compare
    in
    let value_refs =
      collected.value_references |> List.map value_ref_string
      |> List.sort String.compare
    in
    let type_refs =
      collected.type_references |> List.map type_ref_string
      |> List.sort String.compare
    in
    {decls; value_refs; type_refs}

  let diff_lists expected actual =
    let module S = DiffSet in
    let to_set lst = List.fold_left (fun acc x -> S.add x acc) S.empty lst in
    let missing = S.diff (to_set expected) (to_set actual) |> S.elements in
    let extra = S.diff (to_set actual) (to_set expected) |> S.elements in
    (missing, extra)

  type diff = {
    decl_missing: string list;
    decl_extra: string list;
    value_missing: string list;
    value_extra: string list;
    type_missing: string list;
    type_extra: string list;
  }

  let compare a b =
    let decl_missing, decl_extra = diff_lists a.decls b.decls in
    let value_missing, value_extra = diff_lists a.value_refs b.value_refs in
    let type_missing, type_extra = diff_lists a.type_refs b.type_refs in
    if
      decl_missing = [] && decl_extra = [] && value_missing = []
      && value_extra = [] && type_missing = [] && type_extra = []
    then None
    else
      Some
        {
          decl_missing;
          decl_extra;
          value_missing;
          value_extra;
          type_missing;
          type_extra;
        }
end

type mismatch = {file: string; diff: Normalize.diff}

type error =
  | Load_error of string * string
  | Source_missing of string
  | Parity_diff of mismatch

type settings = {
  allow_missing_source: bool;
  max_examples: int;
  dump_filters: string list;
}

let rec gather_files acc path =
  if Sys.file_exists path then
    if Sys.is_directory path then
      Sys.readdir path
      |> Array.fold_left
           (fun acc entry ->
             gather_files acc (Filename.concat path entry))
           acc
    else if
      Filename.check_suffix path ".cmt"
      || Filename.check_suffix path ".cmti"
    then path :: acc
    else acc
  else acc

let with_module_context ~source_file ~is_interface f =
  let module_basename = Paths.getModuleName source_file in
  let module_name = Name.create ~isInterface:is_interface module_basename in
  Common.with_current_module ~src:source_file ~module_name ~module_basename f

let normalize_collected snapshot = Normalize.normalize snapshot

let resolve_source_file cmt_infos =
  match FindSourceFile.cmt cmt_infos.cmt_annots with
  | Some path -> Some path
  | None -> cmt_infos.cmt_sourcefile

let summary_pos_string (pos : Summary.position) =
  Printf.sprintf "%s:%d:%d" pos.file pos.line pos.column

let string_contains ~needle haystack =
  let len_h = String.length haystack and len_n = String.length needle in
  let rec loop i =
    if len_n = 0 then true
    else if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let should_dump dump_filters path =
  match dump_filters with
  | [] -> false
  | filters -> List.exists (fun needle -> string_contains ~needle path) filters

let dump_collected path collected =
  let normalized = Normalize.normalize collected in
  Printf.printf "=== Dump for %s ===\n" path;
  Printf.printf "Decls (%d):\n" (List.length normalized.decls);
  List.iter (fun decl -> Printf.printf "  %s\n" decl) normalized.decls;
  Printf.printf "Value refs (%d):\n" (List.length normalized.value_refs);
  List.iter (fun ref -> Printf.printf "  %s\n" ref) normalized.value_refs;
  Printf.printf "Type refs (%d):\n" (List.length normalized.type_refs);
  List.iter (fun ref -> Printf.printf "  %s\n" ref) normalized.type_refs;
  flush stdout

let string_of_path names =
  names |> List.map Name.toString |> String.concat "."

let dump_graph_stats graph source_file =
  let decl_ids = Graph_store.file_decls graph source_file in
  if decl_ids = [] then
    Printf.printf "Graph stats: no declarations recorded for %s\n%!" source_file
  else (
    let unknown_ids = Graph_store.unknown_value_ids graph in
    let unknown_for_file =
      unknown_ids
      |> List.filter_map (fun id ->
             match Graph_store.find_node graph id with
             | Some node when String.equal node.file source_file ->
                 Some (string_of_path node.decl.path)
             | _ -> None)
    in
    Printf.printf "=== Graph stats for %s (%d decls) ===\n" source_file
      (List.length decl_ids);
    Printf.printf "  total_unknown_values=%d\n"
      (List.length unknown_ids);
    List.iter
      (fun id ->
        match Graph_store.find_node graph id with
        | None ->
            Printf.printf "  [missing node for %s]\n"
              (Graph_store.DeclId.to_string id)
        | Some node ->
            let incoming_set =
              Graph_store.reverse_successors graph ~kind:`Value id
            in
            let incoming = Graph_store.DeclIdSet.cardinal incoming_set in
            let incoming_names =
              Graph_store.DeclIdSet.elements incoming_set
              |> List.filter_map (fun pred_id ->
                     match Graph_store.find_node graph pred_id with
                     | None -> None
                     | Some pred ->
                         Some (string_of_path pred.decl.path))
              |> List.sort String.compare
            in
            let outgoing =
              Graph_store.successors graph ~kind:`Value id
              |> Graph_store.DeclIdSet.cardinal
            in
            let unknown = Graph_store.has_unknown_value_ref graph id in
            Printf.printf
              "  %s | in=%d out=%d unknown=%b start=%s:%d:%d\n    preds=[%s]\n"
              (string_of_path node.decl.path)
              incoming outgoing unknown
              node.summary.loc.start_.file node.summary.loc.start_.line
              node.summary.loc.start_.column
              (String.concat ", " incoming_names))
      decl_ids;
    if unknown_for_file <> [] then
      Printf.printf "  unknown_value_refs=[%s]\n"
        (String.concat ", " unknown_for_file);
    flush stdout)

let dump_legacy_stats source_file =
  let decls =
    DeadCommon.PosHash.fold
      (fun _ decl acc ->
        if String.equal decl.pos.pos_fname source_file then decl :: acc else acc)
      DeadCommon.decls []
  in
  if decls = [] then
    Printf.printf "Legacy stats: no declarations recorded for %s\n%!" source_file
  else (
    Printf.printf "=== Legacy stats for %s (%d decls) ===\n" source_file
      (List.length decls);
    decls
    |> List.iter (fun decl ->
           let refs =
             if DeadCommon.Decl.isValue decl then
               DeadCommon.ValueReferences.find decl.pos
             else DeadCommon.TypeReferences.find decl.pos
           in
           let incoming = DeadCommon.PosSet.cardinal refs in
           let incoming_desc =
             refs |> DeadCommon.PosSet.elements
             |> List.filter_map (fun pos ->
                    match DeadCommon.PosHash.find_opt DeadCommon.decls pos with
                    | Some ref_decl ->
                        Some
                          (Printf.sprintf "%s(resolved=%s)"
                             (string_of_path ref_decl.path)
                             (match ref_decl.resolvedDead with
                             | Some true -> "dead"
                             | Some false -> "live"
                             | None -> "unset"))
                    | None ->
                        Some
                          (Printf.sprintf "%s(unresolved)" (posToString pos)))
             |> String.concat ", "
           in
           Printf.printf
             "  %s | kind=%s resolved=%s report=%b annotated(dead=%b live=%b \
              dead_or_gen=%b) in=%d\n    refs=[%s]\n"
             (string_of_path decl.path)
             (DeclKind.toString decl.declKind)
             (match decl.resolvedDead with
             | Some true -> "true"
             | Some false -> "false"
             | None -> "unset")
             decl.report
             (DeadCommon.ProcessDeadAnnotations.isAnnotatedDead decl.pos)
             (DeadCommon.ProcessDeadAnnotations.isAnnotatedGenTypeOrLive
                decl.pos)
             (DeadCommon.ProcessDeadAnnotations.isAnnotatedGenTypeOrDead
                decl.pos)
             incoming incoming_desc);
    flush stdout)

let compare_file settings path =
  match Typedtree_helpers.read_cmt path with
  | Result.Error msg -> Result.Error (Load_error (path, msg))
  | Result.Ok cmt_infos -> (
    match resolve_source_file cmt_infos with
    | None -> Result.Error (Source_missing path)
    | Some source_file ->
      let is_interface =
        Filename.check_suffix source_file "i"
        || Filename.check_suffix path ".cmti"
      in
      let pure =
        with_module_context ~source_file ~is_interface (fun () ->
            DeadCode.collect_cmt ~cmtFilePath:path cmt_infos)
      in
    if should_dump settings.dump_filters path then dump_collected path pure;
      DeadCommon.Test.clear ();
      with_module_context ~source_file ~is_interface (fun () ->
          let collector = Collector.dead_common_sink () in
          ModulePath.with_current (fun () ->
              DeadCode.processCmt ~collector ~cmtFilePath:path cmt_infos);
          ignore (Collector.finalize collector));
      let legacy = DeadCommon.Test.snapshot () in
      DeadCommon.Test.clear ();
      let pure_norm = normalize_collected pure in
      let legacy_norm = normalize_collected legacy in
      match Normalize.compare legacy_norm pure_norm with
      | None -> Result.Ok ()
      | Some diff -> Result.Error (Parity_diff {file = path; diff}))

let build_decl_key (decl : Common.decl) =
  let names = decl.path |> List.map Name.toString |> String.concat "." in
  Printf.sprintf "%s|%s|%s"
    decl.pos.pos_fname names
    (DeclKind.toString decl.declKind)

type decl_index = {
  set: StringSet.t;
  map: Common.decl StringMap.t;
}

let empty_decl_index = {set = StringSet.empty; map = StringMap.empty}

let add_decl_to_index index decl =
  if decl.report then
    let key = build_decl_key decl in
    {
      set = StringSet.add key index.set;
      map = StringMap.add key decl index.map;
    }
  else index

let decl_index_of_list decls =
  List.fold_left add_decl_to_index empty_decl_index decls

let legacy_dead_index () =
  DeadCommon.PosHash.fold
    (fun _ decl acc ->
      match decl.resolvedDead with
      | Some true -> add_decl_to_index acc decl
      | _ -> acc)
    DeadCommon.decls empty_decl_index

let print_entries ~label ~max_examples entries =
  if entries <> [] then (
    Printf.printf "    %s (%d):\n" label (List.length entries);
    entries
    |> List.iteri (fun idx entry ->
           if idx < max_examples then Printf.printf "      - %s\n" entry);
    if List.length entries > max_examples then
      Printf.printf "      ... (%d more)\n"
        (List.length entries - max_examples))

let describe_decl ~prefix decl =
  let resolved =
    match decl.resolvedDead with
    | Some true -> "true"
    | Some false -> "false"
    | None -> "unset"
  in
  let annotated_dead =
    DeadCommon.ProcessDeadAnnotations.isAnnotatedDead decl.pos
  in
  let annotated_live =
    DeadCommon.ProcessDeadAnnotations.isAnnotatedGenTypeOrLive decl.pos
  in
  let annotated_dead_or_gen =
    DeadCommon.ProcessDeadAnnotations.isAnnotatedGenTypeOrDead decl.pos
  in
  Printf.printf "%s%s\n" prefix (Normalize.decl_string decl);
  Printf.printf
    "%s  resolved=%s report=%b annotated(dead=%b live=%b dead_or_gen=%b)\n"
    prefix resolved decl.report annotated_dead annotated_live
    annotated_dead_or_gen

let describe_diff ~label ~keys ~primary ~secondary =
  keys
  |> List.iter (fun key ->
         Printf.printf "      %s: %s\n" label key;
         (match StringMap.find_opt key primary.map with
         | Some decl -> describe_decl ~prefix:"        primary: " decl
         | None -> Printf.printf "        primary: <missing>\n");
         (match StringMap.find_opt key secondary.map with
         | Some decl -> describe_decl ~prefix:"        secondary: " decl
         | None -> Printf.printf "        secondary: <missing>\n"))

let print_mismatch ~settings {file; diff} =
  Printf.printf "Parity mismatch: %s\n" file;
  print_entries ~label:"Missing decls" ~max_examples:settings.max_examples
    diff.decl_missing;
  print_entries ~label:"Extra decls" ~max_examples:settings.max_examples
    diff.decl_extra;
  print_entries ~label:"Missing value refs" ~max_examples:settings.max_examples
    diff.value_missing;
  print_entries ~label:"Extra value refs" ~max_examples:settings.max_examples
    diff.value_extra;
  print_entries ~label:"Missing type refs" ~max_examples:settings.max_examples
    diff.type_missing;
  print_entries ~label:"Extra type refs" ~max_examples:settings.max_examples
    diff.type_extra

let report_liveness_diff ~settings ~legacy ~incremental =
  if StringSet.equal legacy.set incremental.set then
    Printf.printf "Incremental liveness parity OK (%d declarations checked)\n"
      (StringSet.cardinal legacy.set)
  else (
    let missing =
      StringSet.diff legacy.set incremental.set |> StringSet.elements
    in
    let extra =
      StringSet.diff incremental.set legacy.set |> StringSet.elements
    in
    prerr_endline "Incremental liveness mismatch:";
    print_entries ~label:"Missing deaths" ~max_examples:settings.max_examples
      missing;
    print_entries ~label:"Unexpected deaths" ~max_examples:settings.max_examples
      extra;
    if missing <> [] then
      describe_diff ~label:"Legacy-only dead" ~keys:missing ~primary:legacy
        ~secondary:incremental;
    if extra <> [] then
      describe_diff ~label:"Incremental-only dead" ~keys:extra
        ~primary:incremental ~secondary:legacy;
    exit 1)

let run ~settings ~paths =
  let compare_cmt_paths a b =
    let base_cmp =
      String.compare (Filename.chop_extension a) (Filename.chop_extension b)
    in
    if base_cmp <> 0 then base_cmp
    else
      let priority filename =
        if Filename.check_suffix filename ".cmti" then 0 else 1
      in
      compare (priority a) (priority b)
  in
  let files = paths |> List.fold_left gather_files [] |> List.sort compare_cmt_paths in
  if files = [] then (
    prerr_endline "No .cmt/.cmti files found.";
    exit 1);
  let mismatches = ref [] in
  let failures = ref [] in
  let warnings = ref [] in
  let parity_failed = ref false in
  files
  |> List.iter (fun file ->
         match compare_file settings file with
         | Result.Ok () -> ()
         | Result.Error (Parity_diff diff) -> mismatches := diff :: !mismatches
         | Result.Error (Load_error (path, msg)) ->
           failures := (path, msg) :: !failures
         | Result.Error (Source_missing path) ->
           if settings.allow_missing_source then
             warnings := (path, "unable to resolve source file") :: !warnings
           else failures := (path, "unable to resolve source file") :: !failures);
  if !warnings <> [] then (
    prerr_endline "Warnings:";
    !warnings
    |> List.iter (fun (file, msg) ->
           Printf.eprintf "  %s: %s\n" file msg));
  if !failures <> [] then (
    prerr_endline "Errors while processing files:";
    !failures
    |> List.iter (fun (file, msg) ->
           Printf.eprintf "  %s: %s\n" file msg));
  if !mismatches <> [] then (
    List.rev !mismatches |> List.iter (print_mismatch ~settings);
    prerr_endline "Parity mismatches detected.";
    parity_failed := true)
  else Printf.printf "Collector parity OK (%d files checked)\n" (List.length files);
  if !failures <> [] then exit 1;
  (* Second pass: build full graph/liveness parity *)
  let graph = Graph_store.create () in
  DeadCommon.Test.clear ();
  let graph_failures = ref [] in
  let graph_warnings = ref [] in
  let dump_sources = ref [] in
  let process_file file =
    match Typedtree_helpers.read_cmt file with
    | Result.Error msg ->
        graph_failures := (file, msg) :: !graph_failures
    | Result.Ok cmt_infos -> (
        match resolve_source_file cmt_infos with
        | None ->
            if settings.allow_missing_source then
              graph_warnings :=
                (file, "unable to resolve source file") :: !graph_warnings
            else
              graph_failures :=
                (file, "unable to resolve source file") :: !graph_failures
        | Some source_file ->
            let is_interface =
              Filename.check_suffix source_file "i"
              || Filename.check_suffix file ".cmti"
            in
            with_module_context ~source_file ~is_interface (fun () ->
                let collected = DeadCode.collect_cmt ~cmtFilePath:file cmt_infos in
                let summary = Summary.of_collected ~source_file collected in
                (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
                | Some ("1" | "true" | "on") ->
                    let with_targets, without_targets =
                      List.fold_left
                        (fun (with_t, without_t) ref ->
                          match ref.Summary.target_path with
                          | Some _ -> (with_t + 1, without_t)
                          | None -> (with_t, without_t + 1))
                        (0, 0) summary.Summary.value_references
                    in
                    Printf.eprintf
                      "[collector_parity] %s summary refs target_path with=%d \
                       without=%d\n"
                      source_file with_targets without_targets;
                    if without_targets > 0 then
                      summary.Summary.value_references
                      |> List.filter (fun ref -> ref.Summary.target_path = None)
                      |> List.iteri (fun idx ref ->
                             if idx < 5 then
                               Printf.eprintf
                                 "  [collector_parity] missing target_path %s -> \
                                  %s\n"
                                 (summary_pos_string ref.Summary.loc_from)
                                 (summary_pos_string ref.Summary.loc_to))
                | _ -> ());
                Graph_store.add_summary graph summary;
                if should_dump settings.dump_filters file then (
                  dump_graph_stats graph source_file;
                  dump_sources := source_file :: !dump_sources);
                let collector = Collector.dead_common_sink () in
                ModulePath.with_current (fun () ->
                    DeadCode.processCmt ~collector ~cmtFilePath:file cmt_infos);
                ignore (Collector.finalize collector)))
  in
  List.iter process_file files;
  (match Sys.getenv_opt "GRAPH_TRACE_DEBUG_PATHS" with
  | Some paths when paths <> "" ->
      let names =
        paths |> String.split_on_char ','
        |> List.map String.trim |> List.filter (fun s -> s <> "")
      in
      if names <> [] then Graph_store.debug_log_nodes graph names
  | _ -> ());
  if !graph_failures <> [] then (
    prerr_endline "Graph build errors:";
    !graph_failures
    |> List.iter (fun (file, msg) -> Printf.eprintf "  %s: %s\n" file msg);
    exit 1);
  if !graph_warnings <> [] then (
    prerr_endline "Graph build warnings:";
    !graph_warnings
    |> List.iter (fun (file, msg) -> Printf.eprintf "  %s: %s\n" file msg));
  let dirty_files = Graph_store.get_dirty_files graph in
  let incremental_after_legacy =
    match Sys.getenv_opt "INCR_AFTER_LEGACY" with
    | Some "1" -> true
    | Some "0" -> false
    | _ -> (
      match Sys.getenv_opt "INCR_GRAPH_SOLVER" with
      | Some "1" -> false
      | _ -> true)
  in
  let run_incremental () =
    let result = Incremental_liveness.recompute ~graph ~changed_files:dirty_files in
    let visited = Graph_store.DeclIdSet.cardinal result.visited in
    Printf.printf
      "Incremental liveness frontier: %d decls across %d files\n"
      visited (List.length dirty_files);
    result
  in
  let incremental =
    if incremental_after_legacy then None else Some (run_incremental ())
  in
  DeadCommon.reportDead ~checkOptionalArg:DeadOptionalArgs.check;
  let incremental =
    match incremental with
    | Some res -> res
    | None -> run_incremental ()
  in
  if !dump_sources <> [] then
    !dump_sources
    |> List.rev
    |> List.iter
         (fun source_file ->
           Printf.printf "--- Legacy dump for %s ---\n" source_file;
           dump_legacy_stats source_file);
  let legacy_dead = legacy_dead_index () in
  let incremental_dead = decl_index_of_list incremental.dead_declarations in
  report_liveness_diff ~settings ~legacy:legacy_dead ~incremental:incremental_dead;
  DeadCommon.Test.clear ();
  if !parity_failed then exit 1

let cli () =
  let allow_missing_source = ref false in
  let max_examples = ref 5 in
  let dump_filters = ref [] in
  let paths = ref [] in
  let usage = "collector_parity.exe [options] <cmt-or-directory> [...]" in
  let speclist =
    [
      ( "--allow-missing-source",
        Arg.Set allow_missing_source,
        "Treat files whose source cannot be resolved as warnings instead of \
         errors" );
      ( "--max-examples",
        Arg.Int (fun n -> max_examples := max 0 n),
        "Maximum number of diff entries to display per category (default 5)" );
      ( "--dump",
        Arg.String (fun s -> dump_filters := s :: !dump_filters),
        "Dump normalized declarations and references for files whose paths \
         contain the given substring (can be provided multiple times)" );
    ]
  in
  let anon_fun path = paths := path :: !paths in
  Arg.parse speclist anon_fun usage;
  if !paths = [] then (
    prerr_endline usage;
    exit 1);
  let settings =
    {
      allow_missing_source = !allow_missing_source;
      max_examples = !max_examples;
      dump_filters = !dump_filters;
    }
  in
  run ~settings ~paths:(List.rev !paths)

