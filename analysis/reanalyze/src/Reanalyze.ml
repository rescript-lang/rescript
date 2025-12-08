open Common

(** Process a cmt file and return its file_data (if DCE enabled).
    Conceptually: map over files, then merge results. *)
let loadCmtFile ~config cmtFilePath : DceFileProcessing.file_data option =
  let cmt_infos = Cmt_format.read_cmt cmtFilePath in
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
  match cmt_infos.cmt_annots |> FindSourceFile.cmt with
  | Some sourceFile when not (excludePath sourceFile) ->
    let is_interface =
      match cmt_infos.cmt_annots with
      | Interface _ -> true
      | _ -> Filename.check_suffix sourceFile "i"
    in
    let module_name = sourceFile |> Paths.getModuleName in
    (* File context for DceFileProcessing (breaks cycle with DeadCommon) *)
    let dce_file_context : DceFileProcessing.file_context =
      {source_path = sourceFile; module_name; is_interface}
    in
    (* File context for Exception/Arnold (uses DeadCommon.FileContext) *)
    let file_context =
      DeadCommon.FileContext.
        {source_path = sourceFile; module_name; is_interface}
    in
    if config.cli.debug then
      Log_.item "Scanning %s Source:%s@."
        (match config.cli.ci && not (Filename.is_relative cmtFilePath) with
        | true -> Filename.basename cmtFilePath
        | false -> cmtFilePath)
        (match config.cli.ci && not (Filename.is_relative sourceFile) with
        | true -> sourceFile |> Filename.basename
        | false -> sourceFile);
    (* Process file for DCE - return file_data *)
    let file_data_opt =
      if config.DceConfig.run.dce then
        Some
          (cmt_infos
          |> DceFileProcessing.process_cmt_file ~config ~file:dce_file_context
               ~cmtFilePath)
      else None
    in
    if config.DceConfig.run.exception_ then
      cmt_infos |> Exception.processCmt ~file:file_context;
    if config.DceConfig.run.termination then
      cmt_infos |> Arnold.processCmt ~config ~file:file_context;
    file_data_opt
  | _ -> None

(** Process all cmt files and return list of file_data.
    Conceptually: map process_cmt_file over all files. *)
let processCmtFiles ~config ~cmtRoot : DceFileProcessing.file_data list =
  let ( +++ ) = Filename.concat in
  (* Local mutable state for collecting results - does not escape this function *)
  let file_data_list = ref [] in
  let processFile cmtFilePath =
    match loadCmtFile ~config cmtFilePath with
    | Some file_data -> file_data_list := file_data :: !file_data_list
    | None -> ()
  in
  (match cmtRoot with
  | Some root ->
    Cli.cmtCommand := true;
    let rec walkSubDirs dir =
      let absDir =
        match dir = "" with
        | true -> root
        | false -> root +++ dir
      in
      let skipDir =
        let base = Filename.basename dir in
        base = "node_modules" || base = "_esy"
      in
      if (not skipDir) && Sys.file_exists absDir then
        if Sys.is_directory absDir then
          absDir |> Sys.readdir |> Array.iter (fun d -> walkSubDirs (dir +++ d))
        else if
          Filename.check_suffix absDir ".cmt"
          || Filename.check_suffix absDir ".cmti"
        then processFile absDir
    in
    walkSubDirs ""
  | None ->
    Lazy.force Paths.setReScriptProjectRoot;
    let lib_bs = runConfig.projectRoot +++ ("lib" +++ "bs") in
    let sourceDirs =
      Paths.readSourceDirs ~configSources:None |> List.sort String.compare
    in
    sourceDirs
    |> List.iter (fun sourceDir ->
           let libBsSourceDir = Filename.concat lib_bs sourceDir in
           let files =
             match Sys.readdir libBsSourceDir |> Array.to_list with
             | files -> files
             | exception Sys_error _ -> []
           in
           let cmtFiles =
             files
             |> List.filter (fun x ->
                    Filename.check_suffix x ".cmt"
                    || Filename.check_suffix x ".cmti")
           in
           cmtFiles |> List.sort String.compare
           |> List.iter (fun cmtFile ->
                  let cmtFilePath = Filename.concat libBsSourceDir cmtFile in
                  processFile cmtFilePath)));
  !file_data_list

let runAnalysis ~dce_config ~cmtRoot =
  (* Map: process each file -> list of file_data *)
  let file_data_list = processCmtFiles ~config:dce_config ~cmtRoot in
  if dce_config.DceConfig.run.dce then (
    (* Merge: combine all builders -> immutable data *)
    let annotations =
      FileAnnotations.merge_all
        (file_data_list |> List.map (fun fd -> fd.DceFileProcessing.annotations))
    in
    let decls =
      Declarations.merge_all
        (file_data_list |> List.map (fun fd -> fd.DceFileProcessing.decls))
    in
    let cross_file =
      CrossFileItems.merge_all
        (file_data_list |> List.map (fun fd -> fd.DceFileProcessing.cross_file))
    in
    (* Merge refs and file_deps into builders for cross-file items processing *)
    let refs_builder = References.create_builder () in
    let file_deps_builder = FileDeps.create_builder () in
    file_data_list
    |> List.iter (fun fd ->
           References.merge_into_builder ~from:fd.DceFileProcessing.refs
             ~into:refs_builder;
           FileDeps.merge_into_builder ~from:fd.DceFileProcessing.file_deps
             ~into:file_deps_builder);
    (* Process cross-file exception refs - they write to refs_builder and file_deps_builder *)
    CrossFileItems.process_exception_refs cross_file ~refs:refs_builder
      ~file_deps:file_deps_builder ~find_exception:DeadException.find_exception
      ~config:dce_config;
    (* Process cross-file optional args - they read decls *)
    CrossFileItems.process_optional_args cross_file ~decls;
    (* Now freeze refs and file_deps for solver *)
    let refs = References.freeze_builder refs_builder in
    let file_deps = FileDeps.freeze_builder file_deps_builder in
    DeadCommon.reportDead ~annotations ~decls ~refs ~file_deps
      ~config:dce_config ~checkOptionalArg:DeadOptionalArgs.check);
  if dce_config.DceConfig.run.exception_ then
    Exception.Checks.doChecks ~config:dce_config;
  if dce_config.DceConfig.run.termination && dce_config.DceConfig.cli.debug then
    Arnold.reportStats ~config:dce_config

let runAnalysisAndReport ~cmtRoot =
  Log_.Color.setup ();
  if !Common.Cli.json then EmitJson.start ();
  let dce_config = DceConfig.current () in
  runAnalysis ~dce_config ~cmtRoot;
  Log_.Stats.report ~config:dce_config;
  Log_.Stats.clear ();
  if !Common.Cli.json then EmitJson.finish ()

let cli () =
  let analysisKindSet = ref false in
  let cmtRootRef = ref None in
  let usage = "reanalyze version " ^ Version.version in
  let versionAndExit () =
    print_endline usage;
    exit 0
      [@@raises exit]
  in
  let rec setAll cmtRoot =
    RunConfig.all ();
    cmtRootRef := cmtRoot;
    analysisKindSet := true
  and setConfig () =
    Paths.Config.processBsconfig ();
    analysisKindSet := true
  and setDCE cmtRoot =
    RunConfig.dce ();
    cmtRootRef := cmtRoot;
    analysisKindSet := true
  and setException cmtRoot =
    RunConfig.exception_ ();
    cmtRootRef := cmtRoot;
    analysisKindSet := true
  and setTermination cmtRoot =
    RunConfig.termination ();
    cmtRootRef := cmtRoot;
    analysisKindSet := true
  and speclist =
    [
      ("-all", Arg.Unit (fun () -> setAll None), "Run all the analyses.");
      ( "-all-cmt",
        String (fun s -> setAll (Some s)),
        "root_path Run all the analyses for all the .cmt files under the root \
         path" );
      ("-ci", Unit (fun () -> Cli.ci := true), "Internal flag for use in CI");
      ("-config", Unit setConfig, "Read the analysis mode from rescript.json");
      ("-dce", Unit (fun () -> setDCE None), "Eperimental DCE");
      ("-debug", Unit (fun () -> Cli.debug := true), "Print debug information");
      ( "-dce-cmt",
        String (fun s -> setDCE (Some s)),
        "root_path Experimental DCE for all the .cmt files under the root path"
      );
      ( "-exception",
        Unit (fun () -> setException None),
        "Experimental exception analysis" );
      ( "-exception-cmt",
        String (fun s -> setException (Some s)),
        "root_path Experimental exception analysis for all the .cmt files \
         under the root path" );
      ( "-exclude-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Common.Cli.excludePaths := paths @ Common.Cli.excludePaths.contents),
        "comma-separated-path-prefixes Exclude from analysis files whose path \
         has a prefix in the list" );
      ( "-experimental",
        Set Common.Cli.experimental,
        "Turn on experimental analyses (this option is currently unused)" );
      ( "-externals",
        Set DeadCommon.Config.analyzeExternals,
        "Report on externals in dead code analysis" );
      ("-json", Set Common.Cli.json, "Print reports in json format");
      ( "-live-names",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            Common.Cli.liveNames := names @ Common.Cli.liveNames.contents),
        "comma-separated-names Consider all values with the given names as live"
      );
      ( "-live-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Common.Cli.livePaths := paths @ Common.Cli.livePaths.contents),
        "comma-separated-path-prefixes Consider all values whose path has a \
         prefix in the list as live" );
      ( "-suppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            runConfig.suppress <- names @ runConfig.suppress),
        "comma-separated-path-prefixes Don't report on files whose path has a \
         prefix in the list" );
      ( "-termination",
        Unit (fun () -> setTermination None),
        "Experimental termination analysis" );
      ( "-termination-cmt",
        String (fun s -> setTermination (Some s)),
        "root_path Experimental termination analysis for all the .cmt files \
         under the root path" );
      ( "-unsuppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            runConfig.unsuppress <- names @ runConfig.unsuppress),
        "comma-separated-path-prefixes Report on files whose path has a prefix \
         in the list, overriding -suppress (no-op if -suppress is not \
         specified)" );
      ("-version", Unit versionAndExit, "Show version information and exit");
      ("--version", Unit versionAndExit, "Show version information and exit");
    ]
  in
  Arg.parse speclist print_endline usage;
  if !analysisKindSet = false then setConfig ();
  let cmtRoot = !cmtRootRef in
  runAnalysisAndReport ~cmtRoot
[@@raises exit]

module RunConfig = RunConfig
module DceConfig = DceConfig
module Log_ = Log_
