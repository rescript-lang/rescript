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

(** Process all cmt files and return results for DCE and Exception analysis.
    Conceptually: map process_cmt_file over all files. *)
let processCmtFiles ~config ~cmtRoot ~reactive_collection ~skip_file :
    all_files_result =
  let cmtFilePaths =
    let all = collectCmtFilePaths ~cmtRoot in
    match skip_file with
    | Some should_skip -> List.filter (fun p -> not (should_skip p)) all
    | None -> all
  in
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
  | None -> processFilesSequential ~config cmtFilePaths

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
    ~reactive_liveness ~reactive_solver ~skip_file =
  (* Map: process each file -> list of file_data *)
  let {dce_data_list; exception_results} =
    processCmtFiles ~config:dce_config ~cmtRoot ~reactive_collection ~skip_file
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
          match reactive_solver with
          | Some solver ->
            (* Reactive solver: iterate dead_decls + live_decls *)
            let t0 = Unix.gettimeofday () in
            let dead_code_issues =
              ReactiveSolver.collect_issues ~t:solver ~config:dce_config
                ~ann_store
            in
            let t1 = Unix.gettimeofday () in
            (* Collect optional args issues from live declarations *)
            let optional_args_issues =
              match reactive_merge with
              | Some merged ->
                (* Create CrossFileItemsStore from reactive collection *)
                let cross_file_store =
                  CrossFileItemsStore.of_reactive
                    merged.ReactiveMerge.cross_file_items
                in
                (* Compute optional args state using reactive liveness check.
                   Uses ReactiveSolver.is_pos_live which checks the reactive live collection
                   instead of mutable resolvedDead field. *)
                let is_live pos = ReactiveSolver.is_pos_live ~t:solver pos in
                let find_decl pos =
                  Reactive.get merged.ReactiveMerge.decls pos
                in
                let optional_args_state =
                  CrossFileItemsStore.compute_optional_args_state
                    cross_file_store ~find_decl ~is_live
                in
                (* Iterate live declarations and check for optional args issues *)
                let issues = ref [] in
                ReactiveSolver.iter_live_decls ~t:solver (fun decl ->
                    let decl_issues =
                      DeadOptionalArgs.check ~optional_args_state ~ann_store
                        ~config:dce_config decl
                    in
                    issues := List.rev_append decl_issues !issues);
                List.rev !issues
              | None -> []
            in
            let t2 = Unix.gettimeofday () in
            let all_issues = dead_code_issues @ optional_args_issues in
            let num_dead, num_live = ReactiveSolver.stats ~t:solver in
            if !Cli.timing then (
              Printf.eprintf
                "  ReactiveSolver: dead_code=%.3fms opt_args=%.3fms (dead=%d, \
                 live=%d, issues=%d)\n"
                ((t1 -. t0) *. 1000.0)
                ((t2 -. t1) *. 1000.0)
                num_dead num_live (List.length all_issues);
              (match reactive_liveness with
              | Some liveness -> ReactiveLiveness.print_stats ~t:liveness
              | None -> ());
              ReactiveSolver.print_stats ~t:solver;
              (* Print full reactive node stats, including Top-N by time. *)
              Reactive.print_stats ());
            if !Cli.mermaid then
              Printf.eprintf "\n%s\n" (Reactive.to_mermaid ());
            Some (AnalysisResult.add_issues AnalysisResult.empty all_issues)
          | None ->
            (* Non-reactive path: use old solver with optional args *)
            let empty_optional_args_state = OptionalArgsState.create () in
            let analysis_result_core =
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
              (AnalysisResult.add_issues analysis_result_core
                 optional_args_issues))
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
  (* Reactive scheduler debug output: keep surface area minimal by reusing -timing.
     (-debug is already very verbose for DCE per-decl logging.) *)
  Reactive.set_debug !Cli.timing;
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
  (* Create reactive solver once - sets up the reactive pipeline:
     decls + live → dead_decls → issues
     All downstream collections update automatically when inputs change. *)
  let reactive_solver =
    match (reactive_merge, reactive_liveness) with
    | Some merged, Some liveness_result ->
      (* Pass value_refs_from for hasRefBelow (needed when transitive=false) *)
      let value_refs_from =
        if dce_config.DceConfig.run.transitive then None
        else Some merged.ReactiveMerge.value_refs_from
      in
      Some
        (ReactiveSolver.create ~decls:merged.ReactiveMerge.decls
           ~live:liveness_result.ReactiveLiveness.live
           ~annotations:merged.ReactiveMerge.annotations ~value_refs_from
           ~config:dce_config)
    | _ -> None
  in
  (* Collect CMT file paths once for churning *)
  let cmtFilePaths =
    if !Cli.churn > 0 then Some (collectCmtFilePaths ~cmtRoot) else None
  in
  (* Track previous issue count for diff reporting *)
  let prev_issue_count = ref 0 in
  (* Track currently removed files (to add them back on next run) *)
  let removed_files = ref [] in
  (* Set of removed files for filtering in processCmtFiles *)
  let removed_set = Hashtbl.create 64 in
  (* Aggregate stats for churn mode *)
  let churn_times = ref [] in
  let issues_added_list = ref [] in
  let issues_removed_list = ref [] in
  for run = 1 to numRuns do
    Timing.reset ();
    (* Clear stats at start of each run to avoid accumulation *)
    if run > 1 then Log_.Stats.clear ();
    (* Print run header first *)
    if numRuns > 1 && !Cli.timing then
      Printf.eprintf "\n=== Run %d/%d ===\n%!" run numRuns;
    (* Churn: alternate between remove and add phases *)
    (if !Cli.churn > 0 then
       match (reactive_collection, cmtFilePaths) with
       | Some collection, Some paths ->
         Reactive.reset_stats ();
         if run > 1 && !removed_files <> [] then (
           (* Add back previously removed files *)
           let to_add = !removed_files in
           removed_files := [];
           (* Clear removed set so these files get processed again *)
           List.iter (fun p -> Hashtbl.remove removed_set p) to_add;
           let t0 = Unix.gettimeofday () in
           let processed =
             ReactiveFileCollection.process_files_batch
               (collection
                 : ReactiveAnalysis.t
                 :> (_, _) ReactiveFileCollection.t)
               to_add
           in
           let elapsed = Unix.gettimeofday () -. t0 in
           Timing.add_churn_time elapsed;
           churn_times := elapsed :: !churn_times;
           if !Cli.timing then (
             Printf.eprintf "  Added back %d files (%.3fs)\n%!" processed
               elapsed;
             (match reactive_liveness with
             | Some liveness -> ReactiveLiveness.print_stats ~t:liveness
             | None -> ());
             match reactive_solver with
             | Some solver -> ReactiveSolver.print_stats ~t:solver
             | None -> ()))
         else if run > 1 then (
           (* Remove new random files *)
           let numChurn = min !Cli.churn (List.length paths) in
           let shuffled = shuffle_list paths in
           let to_remove = List.filteri (fun i _ -> i < numChurn) shuffled in
           removed_files := to_remove;
           (* Mark as removed so processCmtFiles skips them *)
           List.iter (fun p -> Hashtbl.replace removed_set p ()) to_remove;
           let t0 = Unix.gettimeofday () in
           let removed =
             ReactiveFileCollection.remove_batch
               (collection
                 : ReactiveAnalysis.t
                 :> (_, _) ReactiveFileCollection.t)
               to_remove
           in
           let elapsed = Unix.gettimeofday () -. t0 in
           Timing.add_churn_time elapsed;
           churn_times := elapsed :: !churn_times;
           if !Cli.timing then (
             Printf.eprintf "  Removed %d files (%.3fs)\n%!" removed elapsed;
             (match reactive_liveness with
             | Some liveness -> ReactiveLiveness.print_stats ~t:liveness
             | None -> ());
             match reactive_solver with
             | Some solver -> ReactiveSolver.print_stats ~t:solver
             | None -> ()))
       | _ -> ());
    (* Skip removed files in reactive mode *)
    let skip_file =
      if Hashtbl.length removed_set > 0 then
        Some (fun path -> Hashtbl.mem removed_set path)
      else None
    in
    runAnalysis ~dce_config ~cmtRoot ~reactive_collection ~reactive_merge
      ~reactive_liveness ~reactive_solver ~skip_file;
    (* Report issue count with diff *)
    let current_count = Log_.Stats.get_issue_count () in
    if !Cli.churn > 0 then (
      let diff = current_count - !prev_issue_count in
      (* Track added/removed separately *)
      if run > 1 then
        if diff > 0 then
          issues_added_list := float_of_int diff :: !issues_added_list
        else if diff < 0 then
          issues_removed_list := float_of_int (-diff) :: !issues_removed_list;
      let diff_str =
        if run = 1 then ""
        else if diff >= 0 then Printf.sprintf " (+%d)" diff
        else Printf.sprintf " (%d)" diff
      in
      Log_.Stats.report ~config:dce_config;
      if !Cli.timing then
        Printf.eprintf "  Total issues: %d%s\n%!" current_count diff_str;
      prev_issue_count := current_count)
    else if run = numRuns then
      (* Only report on last run for non-churn mode *)
      Log_.Stats.report ~config:dce_config;
    Log_.Stats.clear ();
    Timing.report ()
  done;
  (* Print aggregate churn stats *)
  if !Cli.churn > 0 && !Cli.timing && List.length !churn_times > 0 then (
    let calc_stats lst =
      if lst = [] then (0.0, 0.0)
      else
        let n = float_of_int (List.length lst) in
        let sum = List.fold_left ( +. ) 0.0 lst in
        let mean = sum /. n in
        let variance =
          List.fold_left (fun acc x -> acc +. ((x -. mean) ** 2.0)) 0.0 lst /. n
        in
        (mean, sqrt variance)
    in
    let time_mean, time_std = calc_stats !churn_times in
    let added_mean, added_std = calc_stats !issues_added_list in
    let removed_mean, removed_std = calc_stats !issues_removed_list in
    Printf.eprintf "\n=== Churn Summary ===\n";
    Printf.eprintf "  Churn operations: %d\n" (List.length !churn_times);
    Printf.eprintf "  Churn time: mean=%.3fs std=%.3fs\n" time_mean time_std;
    Printf.eprintf "  Issues added: mean=%.0f std=%.0f\n" added_mean added_std;
    Printf.eprintf "  Issues removed: mean=%.0f std=%.0f\n" removed_mean
      removed_std);
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
    Paths.Config.processConfig ();
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
      ("-timing", Set Cli.timing, "Report internal timing of analysis phases");
      ( "-mermaid",
        Set Cli.mermaid,
        "Output Mermaid diagram of reactive pipeline" );
      ( "-reactive",
        Set Cli.reactive,
        "Use reactive analysis (caches processed file_data, skips unchanged \
         files)" );
      ( "-runs",
        Int (fun n -> Cli.runs := n),
        "n Run analysis n times (for benchmarking cache effectiveness)" );
      ( "-churn",
        Int (fun n -> Cli.churn := n),
        "n Remove and re-add n random files between runs (tests incremental \
         correctness)" );
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
