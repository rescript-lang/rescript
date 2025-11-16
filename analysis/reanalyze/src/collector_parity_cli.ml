open Common
open Cmt_format

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

let print_entries ~label ~max_examples entries =
  if entries <> [] then (
    Printf.printf "    %s (%d):\n" label (List.length entries);
    entries
    |> List.iteri (fun idx entry ->
           if idx < max_examples then Printf.printf "      - %s\n" entry);
    if List.length entries > max_examples then
      Printf.printf "      ... (%d more)\n"
        (List.length entries - max_examples))

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

let run ~settings ~paths =
  let files =
    paths |> List.fold_left gather_files [] |> List.sort String.compare
  in
  if files = [] then (
    prerr_endline "No .cmt/.cmti files found.";
    exit 1);
  let mismatches = ref [] in
  let failures = ref [] in
  let warnings = ref [] in
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
    exit 1);
  if !failures <> [] then exit 1;
  Printf.printf "Collector parity OK (%d files checked)\n" (List.length files)

let cli () =
  let allow_missing_source = ref false in
  let max_examples = ref 5 in
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
    ]
  in
  let anon_fun path = paths := path :: !paths in
  Arg.parse speclist anon_fun usage;
  if !paths = [] then (
    prerr_endline usage;
    exit 1);
  let settings =
    {allow_missing_source = !allow_missing_source; max_examples = !max_examples}
  in
  run ~settings ~paths:(List.rev !paths)

