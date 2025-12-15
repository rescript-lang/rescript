(** Reactive analysis service using cached file processing.
    
    This module provides incremental analysis that only re-processes
    files that have changed, caching the processed file_data for
    unchanged files. *)

[@@@alert "-unsafe"]

(** Result of processing a single CMT file *)
type cmt_file_result = {
  dce_data: DceFileProcessing.file_data option;
  exception_data: Exception.file_result option;
}

(** Result of processing all CMT files *)
type all_files_result = {
  dce_data_list: DceFileProcessing.file_data list;
  exception_results: Exception.file_result list;
}

(** Cached file_data for a single CMT file.
    We cache the processed result, not just the raw CMT data. *)
type cached_file = {
  path: string;
  file_data: DceFileProcessing.file_data option;
  exception_data: Exception.file_result option;
}

(** The file cache - maps CMT paths to processed results *)
let file_cache : (string, cached_file) Hashtbl.t = Hashtbl.create 1024

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

(** Process a CMT file, using cached result if file unchanged.
    Returns the cached result if the file hasn't changed since last access. *)
let process_cmt_cached ~config cmtFilePath : cmt_file_result option =
  match CmtCache.read_cmt_if_changed cmtFilePath with
  | None ->
    (* File unchanged - return cached result *)
    (match Hashtbl.find_opt file_cache cmtFilePath with
     | Some cached -> 
       Some { dce_data = cached.file_data; exception_data = cached.exception_data }
     | None ->
       (* First time seeing this file - shouldn't happen, but handle gracefully *)
       None)
  | Some cmt_infos ->
    (* File changed or new - process it *)
    let result = process_cmt_infos ~config ~cmtFilePath cmt_infos in
    (* Cache the result *)
    (match result with
     | Some r ->
       Hashtbl.replace file_cache cmtFilePath {
         path = cmtFilePath;
         file_data = r.dce_data;
         exception_data = r.exception_data;
       }
     | None -> ());
    result

(** Process all files incrementally.
    First run processes all files. Subsequent runs only process changed files. *)
let process_files_incremental ~config cmtFilePaths : all_files_result =
  Timing.time_phase `FileLoading (fun () ->
    let dce_data_list = ref [] in
    let exception_results = ref [] in
    let processed = ref 0 in
    let from_cache = ref 0 in
    
    cmtFilePaths |> List.iter (fun cmtFilePath ->
      (* Check if file was in cache *before* processing *)
      let was_cached = Hashtbl.mem file_cache cmtFilePath in
      match process_cmt_cached ~config cmtFilePath with
      | Some {dce_data; exception_data} ->
        (match dce_data with
         | Some data -> dce_data_list := data :: !dce_data_list
         | None -> ());
        (match exception_data with
         | Some data -> exception_results := data :: !exception_results
         | None -> ());
        (* Track whether it was from cache *)
        if was_cached then
          incr from_cache
        else
          incr processed
      | None -> ()
    );
    
    if !Cli.timing then
      Printf.eprintf "Reactive: %d files processed, %d from cache\n%!" !processed !from_cache;
    
    {dce_data_list = List.rev !dce_data_list; exception_results = List.rev !exception_results})

(** Clear all cached file data *)
let clear () =
  Hashtbl.clear file_cache;
  CmtCache.clear ()

(** Get cache statistics *)
let stats () =
  let file_count = Hashtbl.length file_cache in
  let cmt_stats = CmtCache.stats () in
  (file_count, cmt_stats)

