let runConfig = RunConfig.runConfig

type cmt_file_result = {
  dce_data: DceFileProcessing.file_data option;
  exception_data: Exception.file_result option;
}
(** Result of processing a single cmt file *)

(** Process a cmt file and return its results.
    Conceptually: map over files, then merge results. *)
let loadCmtFile ~config cmtFilePath : cmt_file_result option =
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
    let dce_data =
      if config.DceConfig.run.dce then
        Some
          (cmt_infos
          |> DceFileProcessing.process_cmt_file ~config ~file:dce_file_context
               ~cmtFilePath)
      else None
    in
    (* Process file for Exception analysis *)
    let exception_data =
      if config.DceConfig.run.exception_ then
        cmt_infos |> Exception.processCmt ~file:file_context
      else None
    in
    if config.DceConfig.run.termination then
      cmt_infos |> Arnold.processCmt ~config ~file:file_context;
    Some {dce_data; exception_data}
  | _ -> None

type all_files_result = {
  dce_data_list: DceFileProcessing.file_data list;
  exception_results: Exception.file_result list;
}
(** Result of processing all cmt files *)

(** Collect all cmt file paths to process *)
let collectCmtFilePaths ~cmtRoot : string list =
  let ( +++ ) = Filename.concat in
  let paths = ref [] in
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
        then paths := absDir :: !paths
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
                  paths := cmtFilePath :: !paths)));
  !paths |> List.rev

(** Process files sequentially *)
let processFilesSequential ~config (cmtFilePaths : string list) :
    all_files_result =
  Timing.time_phase `FileLoading (fun () ->
      let dce_data_list = ref [] in
      let exception_results = ref [] in
      cmtFilePaths
      |> List.iter (fun cmtFilePath ->
             match loadCmtFile ~config cmtFilePath with
             | Some {dce_data; exception_data} -> (
               (match dce_data with
               | Some data -> dce_data_list := data :: !dce_data_list
               | None -> ());
               match exception_data with
               | Some data -> exception_results := data :: !exception_results
               | None -> ())
             | None -> ());
      {dce_data_list = !dce_data_list; exception_results = !exception_results})

(** Process files in parallel using OCaml 5 Domains *)
let processFilesParallel ~config ~numDomains (cmtFilePaths : string list) :
    all_files_result =
  let numFiles = List.length cmtFilePaths in
  if numFiles = 0 then {dce_data_list = []; exception_results = []}
  else
    let filesArray = Array.of_list cmtFilePaths in
    let numDomains = min numDomains numFiles in
    (* Divide files among domains *)
    let chunkSize = (numFiles + numDomains - 1) / numDomains in
    (* Thread-safe results accumulator using Mutex *)
    let resultsMutex = Mutex.create () in
    let allDceData = ref [] in
    let allExceptionData = ref [] in
    let processChunk startIdx endIdx =
      let localDce = ref [] in
      let localExn = ref [] in
      for i = startIdx to endIdx - 1 do
        match loadCmtFile ~config filesArray.(i) with
        | Some {dce_data; exception_data} -> (
          (match dce_data with
          | Some data -> localDce := data :: !localDce
          | None -> ());
          match exception_data with
          | Some data -> localExn := data :: !localExn
          | None -> ())
        | None -> ()
      done;
      (* Merge local results into global results under mutex.
         Timed separately to measure time spent in (and waiting on) the
         mutex-protected merge. Note: this is an aggregate across domains and
         may exceed wall-clock time in parallel runs. *)
      Timing.time_phase `ResultCollection (fun () ->
          Mutex.lock resultsMutex;
          allDceData := !localDce @ !allDceData;
          allExceptionData := !localExn @ !allExceptionData;
          Mutex.unlock resultsMutex)
    in
    (* Time the overall parallel processing *)
    Timing.time_phase `FileLoading (fun () ->
        (* Spawn domains for parallel processing *)
        let domains =
          Array.init numDomains (fun i ->
              let startIdx = i * chunkSize in
              let endIdx = min ((i + 1) * chunkSize) numFiles in
              if startIdx < numFiles then
                Some (Domain.spawn (fun () -> processChunk startIdx endIdx))
              else None)
        in
        (* Wait for all domains to complete *)
        Array.iter
          (function
            | Some d -> Domain.join d
            | None -> ())
          domains);
    {dce_data_list = !allDceData; exception_results = !allExceptionData}

(** Process all cmt files and return results for DCE and Exception analysis.
    Conceptually: map process_cmt_file over all files. *)
let processCmtFiles ~config ~cmtRoot ~reactive_collection : all_files_result =
  let cmtFilePaths = collectCmtFilePaths ~cmtRoot in
  (* Reactive mode: use incremental processing that skips unchanged files *)
  match reactive_collection with
  | Some collection ->
    let result =
      ReactiveAnalysis.process_files ~collection ~config cmtFilePaths
    in
    {
      dce_data_list = result.dce_data_list;
      exception_results = result.exception_results;
    }
  | None ->
    let numDomains =
      match !Cli.parallel with
      | n when n > 0 -> n
      | n when n < 0 ->
        (* Auto-detect: use recommended domain count (number of cores) *)
        Domain.recommended_domain_count ()
      | _ -> 0
    in
    if numDomains > 0 then (
      if !Cli.timing then
        Printf.eprintf "Using %d parallel domains for %d files\n%!" numDomains
          (List.length cmtFilePaths);
      processFilesParallel ~config ~numDomains cmtFilePaths)
    else processFilesSequential ~config cmtFilePaths

(* Shuffle a list using Fisher-Yates algorithm *)
let shuffle_list lst =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

let runAnalysis ~dce_config ~cmtRoot ~reactive_collection ~reactive_merge
    ~reactive_liveness =
  (* Map: process each file -> list of file_data *)
  let {dce_data_list; exception_results} =
    processCmtFiles ~config:dce_config ~cmtRoot ~reactive_collection
  in
  (* Get exception results from reactive collection if available *)
  let exception_results =
    match reactive_collection with
    | Some collection -> ReactiveAnalysis.collect_exception_results collection
    | None -> exception_results
  in
  (* Optionally shuffle for order-independence testing *)
  let dce_data_list =
    if !Cli.testShuffle then (
      Random.self_init ();
      if dce_config.DceConfig.cli.debug then
        Log_.item "Shuffling file order for order-independence test@.";
      shuffle_list dce_data_list)
    else dce_data_list
  in
  (* Analysis phase: merge data and solve *)
  let analysis_result =
    if dce_config.DceConfig.run.dce then
      (* Merging phase: combine all builders -> immutable data *)
      let ann_store, decl_store, cross_file_store, ref_store =
        Timing.time_phase `Merging (fun () ->
            (* Use reactive merge if available, otherwise list-based merge *)
            let ann_store, decl_store, cross_file_store =
              match reactive_merge with
              | Some merged ->
                (* Reactive mode: use stores directly, skip freeze! *)
                ( AnnotationStore.of_reactive merged.ReactiveMerge.annotations,
                  DeclarationStore.of_reactive merged.ReactiveMerge.decls,
                  CrossFileItemsStore.of_reactive
                    merged.ReactiveMerge.cross_file_items )
              | None ->
                (* Non-reactive mode: freeze into data, wrap in store *)
                let decls =
                  Declarations.merge_all
                    (dce_data_list
                    |> List.map (fun fd -> fd.DceFileProcessing.decls))
                in
                ( AnnotationStore.of_frozen
                    (FileAnnotations.merge_all
                       (dce_data_list
                       |> List.map (fun fd -> fd.DceFileProcessing.annotations)
                       )),
                  DeclarationStore.of_frozen decls,
                  CrossFileItemsStore.of_frozen
                    (CrossFileItems.merge_all
                       (dce_data_list
                       |> List.map (fun fd -> fd.DceFileProcessing.cross_file)))
                )
            in
            (* Compute refs.
               In reactive mode, use stores directly (skip freeze!).
               In non-reactive mode, use the imperative processing. *)
            let ref_store =
              match reactive_merge with
              | Some merged ->
                (* Reactive mode: use stores directly *)
                ReferenceStore.of_reactive
                  ~value_refs_from:merged.value_refs_from
                  ~type_refs_from:merged.type_refs_from
                  ~type_deps:merged.type_deps
                  ~exception_refs:merged.exception_refs
              | None ->
                (* Non-reactive mode: build refs imperatively *)
                (* Need Declarations.t for type deps processing *)
                let decls =
                  match decl_store with
                  | DeclarationStore.Frozen d -> d
                  | DeclarationStore.Reactive _ ->
                    failwith
                      "unreachable: non-reactive path with reactive store"
                in
                (* Need CrossFileItems.t for exception refs processing *)
                let cross_file =
                  match cross_file_store with
                  | CrossFileItemsStore.Frozen cfi -> cfi
                  | CrossFileItemsStore.Reactive _ ->
                    failwith
                      "unreachable: non-reactive path with reactive store"
                in
                let refs_builder = References.create_builder () in
                let file_deps_builder = FileDeps.create_builder () in
                (match reactive_collection with
                | Some collection ->
                  ReactiveAnalysis.iter_file_data collection (fun fd ->
                      References.merge_into_builder
                        ~from:fd.DceFileProcessing.refs ~into:refs_builder;
                      FileDeps.merge_into_builder
                        ~from:fd.DceFileProcessing.file_deps
                        ~into:file_deps_builder)
                | None ->
                  dce_data_list
                  |> List.iter (fun fd ->
                         References.merge_into_builder
                           ~from:fd.DceFileProcessing.refs ~into:refs_builder;
                         FileDeps.merge_into_builder
                           ~from:fd.DceFileProcessing.file_deps
                           ~into:file_deps_builder));
                (* Compute type-label dependencies after merge *)
                DeadType.process_type_label_dependencies ~config:dce_config
                  ~decls ~refs:refs_builder;
                let find_exception =
                  DeadException.find_exception_from_decls decls
                in
                (* Process cross-file exception refs *)
                CrossFileItems.process_exception_refs cross_file
                  ~refs:refs_builder ~file_deps:file_deps_builder
                  ~find_exception ~config:dce_config;
                (* Freeze refs for solver *)
                let refs = References.freeze_builder refs_builder in
                ReferenceStore.of_frozen refs
            in
            (ann_store, decl_store, cross_file_store, ref_store))
      in
      (* Solving phase: run the solver and collect issues *)
      Timing.time_phase `Solving (fun () ->
          let empty_optional_args_state = OptionalArgsState.create () in
          let analysis_result_core =
            match (reactive_merge, reactive_liveness) with
            | Some merged, Some liveness_result ->
              let live = liveness_result.ReactiveLiveness.live in
              (* Freeze refs for debug/transitive support in solver *)
              let refs = ReactiveMerge.freeze_refs merged in
              DeadCommon.solveDeadReactive ~ann_store ~decl_store ~refs ~live
                ~optional_args_state:empty_optional_args_state
                ~config:dce_config
                ~checkOptionalArg:(fun
                    ~optional_args_state:_ ~ann_store:_ ~config:_ _ -> [])
            | _ ->
              DeadCommon.solveDead ~ann_store ~decl_store ~ref_store
                ~optional_args_state:empty_optional_args_state
                ~config:dce_config
                ~checkOptionalArg:(fun
                    ~optional_args_state:_ ~ann_store:_ ~config:_ _ -> [])
          in
          (* Compute liveness-aware optional args state *)
          let is_live pos =
            match DeclarationStore.find_opt decl_store pos with
            | Some decl -> Decl.isLive decl
            | None -> true
          in
          let optional_args_state =
            CrossFileItemsStore.compute_optional_args_state cross_file_store
              ~find_decl:(DeclarationStore.find_opt decl_store)
              ~is_live
          in
          (* Collect optional args issues only for live declarations *)
          let optional_args_issues =
            DeclarationStore.fold
              (fun _pos decl acc ->
                if Decl.isLive decl then
                  let issues =
                    DeadOptionalArgs.check ~optional_args_state ~ann_store
                      ~config:dce_config decl
                  in
                  List.rev_append issues acc
                else acc)
              decl_store []
            |> List.rev
          in
          Some
            (AnalysisResult.add_issues analysis_result_core optional_args_issues))
    else None
  in
  (* Reporting phase *)
  Timing.time_phase `Reporting (fun () ->
      (match analysis_result with
      | Some result ->
        AnalysisResult.get_issues result
        |> List.iter (fun (issue : Issue.t) ->
               Log_.warning ~loc:issue.loc issue.description)
      | None -> ());
      if dce_config.DceConfig.run.exception_ then
        Exception.runChecks ~config:dce_config exception_results;
      if dce_config.DceConfig.run.termination && dce_config.DceConfig.cli.debug
      then Arnold.reportStats ~config:dce_config)

let runAnalysisAndReport ~cmtRoot =
  Log_.Color.setup ();
  Timing.enabled := !Cli.timing;
  if !Cli.json then EmitJson.start ();
  let dce_config = DceConfig.current () in
  let numRuns = max 1 !Cli.runs in
  (* Create reactive collection once, reuse across runs *)
  let reactive_collection =
    if !Cli.reactive then Some (ReactiveAnalysis.create ~config:dce_config)
    else None
  in
  (* Create reactive merge once if reactive mode is enabled.
     This automatically updates when reactive_collection changes. *)
  let reactive_merge =
    match reactive_collection with
    | Some collection ->
      let file_data_collection =
        ReactiveAnalysis.to_file_data_collection collection
      in
      Some (ReactiveMerge.create file_data_collection)
    | None -> None
  in
  (* Create reactive liveness. This is created before files are processed,
     so it receives deltas as files are processed incrementally. *)
  let reactive_liveness =
    match reactive_merge with
    | Some merged -> Some (ReactiveLiveness.create ~merged)
    | None -> None
  in
  for run = 1 to numRuns do
    Timing.reset ();
    (* Clear stats at start of each run to avoid accumulation *)
    if run > 1 then Log_.Stats.clear ();
    if numRuns > 1 && !Cli.timing then
      Printf.eprintf "\n=== Run %d/%d ===\n%!" run numRuns;
    runAnalysis ~dce_config ~cmtRoot ~reactive_collection ~reactive_merge
      ~reactive_liveness;
    if run = numRuns then (
      (* Only report on last run *)
      Log_.Stats.report ~config:dce_config;
      Log_.Stats.clear ());
    Timing.report ()
  done;
  if !Cli.json then EmitJson.finish ()

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
            Cli.excludePaths := paths @ Cli.excludePaths.contents),
        "comma-separated-path-prefixes Exclude from analysis files whose path \
         has a prefix in the list" );
      ( "-experimental",
        Set Cli.experimental,
        "Turn on experimental analyses (this option is currently unused)" );
      ( "-externals",
        Set DeadCommon.Config.analyzeExternals,
        "Report on externals in dead code analysis" );
      ("-json", Set Cli.json, "Print reports in json format");
      ( "-live-names",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            Cli.liveNames := names @ Cli.liveNames.contents),
        "comma-separated-names Consider all values with the given names as live"
      );
      ( "-live-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Cli.livePaths := paths @ Cli.livePaths.contents),
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
      ( "-test-shuffle",
        Set Cli.testShuffle,
        "Test flag: shuffle file processing order to verify order-independence"
      );
      ( "-parallel",
        Int (fun n -> Cli.parallel := n),
        "n Process files in parallel using n domains (0 = sequential, default; \
         -1 = auto-detect cores)" );
      ("-timing", Set Cli.timing, "Report internal timing of analysis phases");
      ( "-reactive",
        Set Cli.reactive,
        "Use reactive analysis (caches processed file_data, skips unchanged \
         files)" );
      ( "-runs",
        Int (fun n -> Cli.runs := n),
        "n Run analysis n times (for benchmarking cache effectiveness)" );
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
