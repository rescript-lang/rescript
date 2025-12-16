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
let process_cmt_infos ~config cmt_infos : cmt_file_result option =
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
               ~cmtFilePath:"")
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
    ~process:(process_cmt_infos ~config)

(** Process all files incrementally using ReactiveFileCollection.
    First run processes all files. Subsequent runs only process changed files. *)
let process_files ~(collection : t) ~config:_ cmtFilePaths : all_files_result =
  Timing.time_phase `FileLoading (fun () ->
      let processed = ref 0 in
      let from_cache = ref 0 in

      (* Add/update all files in the collection *)
      cmtFilePaths
      |> List.iter (fun cmtFilePath ->
             let was_in_collection =
               ReactiveFileCollection.mem collection cmtFilePath
             in
             let changed =
               ReactiveFileCollection.process_if_changed collection cmtFilePath
             in
             if changed then incr processed
             else if was_in_collection then incr from_cache);

      if !Cli.timing then
        Printf.eprintf "Reactive: %d files processed, %d from cache\n%!"
          !processed !from_cache;

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
