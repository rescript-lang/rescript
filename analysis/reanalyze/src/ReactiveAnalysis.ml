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

type processing_stats = {
  mutable total_files: int;
  mutable processed: int;
  mutable from_cache: int;
}
(** Stats from a process_files call *)

(** Process cmt_infos into a file result *)
let process_cmt_infos ~config ~cmt_file_path cmt_infos : cmt_file_result option
    =
  let exclude_path source_file =
    config.DceConfig.cli.exclude_paths
    |> List.exists (fun prefix_ ->
           let prefix =
             match Filename.is_relative source_file with
             | true -> prefix_
             | false -> Filename.concat (Sys.getcwd ()) prefix_
           in
           String.length prefix <= String.length source_file
           &&
           try String.sub source_file 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  match cmt_infos.Cmt_format.cmt_annots |> FindSourceFile.cmt with
  | Some source_file when not (exclude_path source_file) ->
    let is_interface_ =
      match cmt_infos.cmt_annots with
      | Interface _ -> true
      | _ -> Filename.check_suffix source_file "i"
    in
    let module_name = source_file |> Paths.get_module_name in
    let dce_file_context : DceFileProcessing.file_context =
      {source_path = source_file; module_name; is_interface = is_interface_}
    in
    let file_context =
      DeadCommon.FileContext.
        {source_path = source_file; module_name; is_interface = is_interface_}
    in
    let dce_data =
      if config.DceConfig.run.dce then
        Some
          (cmt_infos
          |> DceFileProcessing.process_cmt_file ~config ~file:dce_file_context
               ~cmt_file_path)
      else None
    in
    let exception_data =
      if config.DceConfig.run.exception_ then
        cmt_infos |> Exception.process_cmt ~file:file_context
      else None
    in
    if config.DceConfig.run.termination then
      cmt_infos |> Arnold.process_cmt ~config ~file:file_context;
    Some {dce_data; exception_data}
  | _ -> None

(** Create a new reactive collection *)
let create ~config : t =
  ReactiveFileCollection.create ~read_file:Cmt_format.read_cmt
    ~process:(fun path cmt_infos ->
      process_cmt_infos ~config ~cmt_file_path:path cmt_infos)

(** Process all files incrementally using ReactiveFileCollection.
    First run processes all files. Subsequent runs only process changed files.
    Uses batch processing to emit all changes as a single Batch delta.
    Returns (result, stats) where stats contains processing information. *)
let process_files ~(collection : t) ~config:_ cmt_file_paths :
    all_files_result * processing_stats =
  Timing.time_phase `FileLoading (fun () ->
      let total_files = List.length cmt_file_paths in
      let cached_before =
        cmt_file_paths
        |> List.filter (fun p -> ReactiveFileCollection.mem collection p)
        |> List.length
      in

      (* Process all files as a batch - emits single Batch delta *)
      let processed =
        ReactiveFileCollection.process_files_batch collection cmt_file_paths
      in
      let from_cache = total_files - processed in
      let stats = {total_files; processed; from_cache} in

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

      ( {
          dce_data_list = List.rev !dce_data_list;
          exception_results = List.rev !exception_results;
        },
        stats ))

(** Get collection length *)
let length (collection : t) = ReactiveFileCollection.length collection

(** Get the underlying reactive collection for composition.
    Returns (path, file_data option) suitable for ReactiveMerge. *)
let to_file_data_collection (collection : t) :
    (string, DceFileProcessing.file_data option) Reactive.t =
  Reactive.flat_map ~name:"file_data_collection"
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
