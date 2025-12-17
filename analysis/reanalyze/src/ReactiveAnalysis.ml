(** Reactive analysis service using ReactiveFileCollection.

    This module provides incremental analysis that only re-processes
    files that have changed, using ReactiveFileCollection for efficient
    delta-based updates. *)

type cmt_file_result = {
  dce_data: DceFileProcessing.file_data option;
  exception_data: Exception.file_result option;
}
(** Result of processing a single CMT file *)

type all_files_result = {
  dce_data_list: DceFileProcessing.file_data list;
  exception_results: Exception.file_result list;
}
(** Result of processing all CMT files *)

type t = (Cmt_format.cmt_infos, cmt_file_result option) ReactiveFileCollection.t
(** The reactive collection type *)

(** Process cmt_infos into a file result *)
let process_cmt_infos ~config ~cmtFilePath cmt_infos : cmt_file_result option =
  let excludePath sourceFile =
    config.DceConfig.cli.exclude_paths
    |> List.exists (fun prefix_ ->
           let prefix =
             match Filename.is_relative sourceFile with
             | true -> prefix_
             | false -> Filename.concat (Sys.getcwd ()) prefix_
           in
           String.length prefix <= String.length sourceFile
           &&
           try String.sub sourceFile 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  match cmt_infos.Cmt_format.cmt_annots |> FindSourceFile.cmt with
  | Some sourceFile when not (excludePath sourceFile) ->
    let is_interface =
      match cmt_infos.cmt_annots with
      | Interface _ -> true
      | _ -> Filename.check_suffix sourceFile "i"
    in
    let module_name = sourceFile |> Paths.getModuleName in
    let dce_file_context : DceFileProcessing.file_context =
      {source_path = sourceFile; module_name; is_interface}
    in
    let file_context =
      DeadCommon.FileContext.
        {source_path = sourceFile; module_name; is_interface}
    in
    let dce_data =
      if config.DceConfig.run.dce then
        Some
          (cmt_infos
          |> DceFileProcessing.process_cmt_file ~config ~file:dce_file_context
               ~cmtFilePath)
      else None
    in
    let exception_data =
      if config.DceConfig.run.exception_ then
        cmt_infos |> Exception.processCmt ~file:file_context
      else None
    in
    if config.DceConfig.run.termination then
      cmt_infos |> Arnold.processCmt ~config ~file:file_context;
    Some {dce_data; exception_data}
  | _ -> None

(** Create a new reactive collection *)
let create ~config : t =
  ReactiveFileCollection.create ~read_file:Cmt_format.read_cmt
    ~process:(fun path cmt_infos ->
      process_cmt_infos ~config ~cmtFilePath:path cmt_infos)

(** Process all files incrementally using ReactiveFileCollection.
    First run processes all files. Subsequent runs only process changed files.
    Uses batch processing to emit all changes as a single Batch delta. *)
let process_files ~(collection : t) ~config:_ cmtFilePaths : all_files_result =
  Timing.time_phase `FileLoading (fun () ->
      let total_files = List.length cmtFilePaths in
      let cached_before =
        cmtFilePaths
        |> List.filter (fun p -> ReactiveFileCollection.mem collection p)
        |> List.length
      in

      (* Process all files as a batch - emits single Batch delta *)
      let processed =
        ReactiveFileCollection.process_files_batch collection cmtFilePaths
      in
      let from_cache = total_files - processed in

      if !Cli.timing then
        Printf.eprintf
          "Reactive: %d files processed, %d from cache (was cached: %d)\n%!"
          processed from_cache cached_before;

      (* Collect results from the collection *)
      let dce_data_list = ref [] in
      let exception_results = ref [] in

      ReactiveFileCollection.iter
        (fun _path result_opt ->
          match result_opt with
          | Some {dce_data; exception_data} -> (
            (match dce_data with
            | Some data -> dce_data_list := data :: !dce_data_list
            | None -> ());
            match exception_data with
            | Some data -> exception_results := data :: !exception_results
            | None -> ())
          | None -> ())
        collection;

      {
        dce_data_list = List.rev !dce_data_list;
        exception_results = List.rev !exception_results;
      })

(** Get collection length *)
let length (collection : t) = ReactiveFileCollection.length collection

(** Get the underlying reactive collection for composition.
    Returns (path, file_data option) suitable for ReactiveMerge. *)
let to_file_data_collection (collection : t) :
    (string, DceFileProcessing.file_data option) Reactive.t =
  Reactive.flatMap
    (ReactiveFileCollection.to_collection collection)
    ~f:(fun path result_opt ->
      match result_opt with
      | Some {dce_data = Some data; _} -> [(path, Some data)]
      | _ -> [(path, None)])
    ()

(** Iterate over all file_data in the collection *)
let iter_file_data (collection : t) (f : DceFileProcessing.file_data -> unit) :
    unit =
  ReactiveFileCollection.iter
    (fun _path result_opt ->
      match result_opt with
      | Some {dce_data = Some data; _} -> f data
      | _ -> ())
    collection

(** Collect all exception results from the collection *)
let collect_exception_results (collection : t) : Exception.file_result list =
  let results = ref [] in
  ReactiveFileCollection.iter
    (fun _path result_opt ->
      match result_opt with
      | Some {exception_data = Some data; _} -> results := data :: !results
      | _ -> ())
    collection;
  !results
