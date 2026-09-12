let run_config = Run_config.run_config

let collect_cmt_file_paths ~cmt_root : string list =
  let ( +++ ) = Filename.concat in
  let paths = ref [] in
  (match cmt_root with
  | Some root ->
    Cli.cmt_command := true;
    let rec walk_sub_dirs dir =
      let abs_dir =
        match dir = "" with
        | true -> root
        | false -> root +++ dir
      in
      let skip_dir =
        let base = Filename.basename dir in
        base = "node_modules" || base = "_esy"
      in
      if (not skip_dir) && Sys.file_exists abs_dir then
        if Sys.is_directory abs_dir then
          abs_dir |> Sys.readdir
          |> Array.iter (fun d -> walk_sub_dirs (dir +++ d))
        else if
          Filename.check_suffix abs_dir ".cmt"
          || Filename.check_suffix abs_dir ".cmti"
        then paths := abs_dir :: !paths
    in
    walk_sub_dirs ""
  | None ->
    Lazy.force Paths.set_rescript_project_root;
    (* Prefer explicit scan plan emitted by rewatch (v2 `.sourcedirs.json`).
       This supports monorepos without reanalyze-side package resolution. *)
    let scan_plan = Paths.read_cmt_scan () in
    let seen = Hashtbl.create 256 in
    let add_dir (abs_dir : string) =
      let files =
        match Sys.readdir abs_dir |> Array.to_list with
        | files -> files
        | exception Sys_error _ -> []
      in
      files
      |> List.filter (fun x ->
          Filename.check_suffix x ".cmt" || Filename.check_suffix x ".cmti")
      |> List.sort String.compare
      |> List.iter (fun f ->
          let p = Filename.concat abs_dir f in
          if not (Hashtbl.mem seen p) then (
            Hashtbl.add seen p ();
            paths := p :: !paths))
    in
    scan_plan
    |> List.iter (fun (entry : Paths.cmt_scan_entry) ->
        let build_root_abs =
          Filename.concat run_config.project_root entry.build_root
        in
        (* Scan configured subdirs. *)
        entry.scan_dirs
        |> List.iter (fun d -> add_dir (Filename.concat build_root_abs d));
        (* Optionally scan build root itself for namespace/mlmap `.cmt`s. *)
        if entry.also_scan_build_root then add_dir build_root_abs));
  !paths |> List.rev

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

(** Process all cmt files and return results for DCE and Exception analysis.
    Conceptually: map process_cmt_file over all files.
    If file_stats is provided, it will be updated with processing statistics. *)
let process_cmt_files ~config ~cmt_root ~collection ~skip_file
    ?(file_stats : Reactive_analysis.processing_stats option) () :
    Reactive_analysis.all_files_result =
  let cmt_file_paths =
    let all = collect_cmt_file_paths ~cmt_root in
    let all =
      match skip_file with
      | Some should_skip -> List.filter (fun p -> not (should_skip p)) all
      | None -> all
    in
    (* Order-independence testing: results must not depend on the order files
       are processed in. Shuffle the paths, which is the order that reaches the
       reactive collection. *)
    if !Cli.test_shuffle then (
      Random.self_init ();
      if config.Dce_config.cli.debug then
        Log_.item "Shuffling file order for order-independence test@.";
      shuffle_list all)
    else all
  in
  let result, stats =
    Reactive_analysis.process_files ~collection ~config cmt_file_paths
  in
  (match file_stats with
  | Some fs ->
    fs.total_files <- stats.total_files;
    fs.processed <- stats.processed;
    fs.from_cache <- stats.from_cache
  | None -> ());
  {
    dce_data_list = result.dce_data_list;
    exception_results = result.exception_results;
  }

let run_analysis ~dce_config ~cmt_root ~(pipeline : Dce_pipeline.t) ~skip_file
    ?file_stats () =
  let {Dce_pipeline.collection; merged; liveness; solver} = pipeline in
  (* Map: process each file -> the reactive collection *)
  ignore
    (process_cmt_files ~config:dce_config ~cmt_root ~collection ~skip_file
       ?file_stats ());
  let exception_results =
    Reactive_analysis.collect_exception_results collection
  in
  (* Analysis phase: solve over the reactive collections *)
  let analysis_result =
    if dce_config.Dce_config.run.dce then
      let ann_store =
        Timing.time_phase `Merging (fun () ->
            Annotation_store.of_reactive merged.Reactive_merge.annotations)
      in
      Timing.time_phase `Solving (fun () ->
          let t0 = Unix.gettimeofday () in
          let dead_code_issues =
            Reactive_solver.collect_issues ~t:solver ~config:dce_config
              ~ann_store
          in
          let t1 = Unix.gettimeofday () in
          (* Optional args issues, for live declarations only *)
          let optional_args_issues =
            let cross_file_store =
              Cross_file_items_store.of_reactive
                merged.Reactive_merge.cross_file_items
            in
            let is_live pos = Reactive_solver.is_pos_live ~t:solver pos in
            let find_decl pos = Reactive.get merged.Reactive_merge.decls pos in
            let optional_args_state =
              Cross_file_items_store.compute_optional_args_state
                cross_file_store ~find_decl ~is_live
            in
            let optional_arg_value_escapes =
              Cross_file_items_store.compute_live_optional_arg_value_escapes
                cross_file_store ~is_live
            in
            let issues = ref [] in
            Reactive_solver.iter_live_decls ~t:solver (fun decl ->
                let decl_issues =
                  Dead_optional_args.check ~optional_args_state
                    ~optional_arg_value_escapes ~ann_store ~config:dce_config
                    decl
                in
                issues := List.rev_append decl_issues !issues);
            List.rev !issues
          in
          let t2 = Unix.gettimeofday () in
          let all_issues = dead_code_issues @ optional_args_issues in
          let num_dead, num_live = Reactive_solver.stats ~t:solver in
          if !Cli.timing then (
            Printf.eprintf
              "  ReactiveSolver: dead_code=%.3fms opt_args=%.3fms (dead=%d, \
               live=%d, issues=%d)\n"
              ((t1 -. t0) *. 1000.0)
              ((t2 -. t1) *. 1000.0)
              num_dead num_live (List.length all_issues);
            Reactive_liveness.print_stats ~t:liveness;
            Reactive_solver.print_stats ~t:solver;
            (* Print full reactive node stats, including Top-N by time. *)
            Reactive.print_stats ());
          if !Cli.mermaid then Printf.eprintf "\n%s\n" (Reactive.to_mermaid ());
          Some (Analysis_result.add_issues Analysis_result.empty all_issues))
    else None
  in
  (* Reporting phase *)
  Timing.time_phase `Reporting (fun () ->
      (match analysis_result with
      | Some result ->
        Analysis_result.get_issues result
        |> List.iter (fun (issue : Issue.t) ->
            Log_.warning ~loc:issue.loc issue.description)
      | None -> ());
      if dce_config.Dce_config.run.exception_ then
        Exception.run_checks ~config:dce_config exception_results;
      if
        dce_config.Dce_config.run.termination && dce_config.Dce_config.cli.debug
      then Arnold.report_stats ~config:dce_config)

let run_analysis_and_report ~cmt_root =
  Log_.Color.setup ();
  Timing.enabled := !Cli.timing;
  (* Reactive scheduler debug output: keep surface area minimal by reusing -timing.
     (-debug is already very verbose for DCE per-decl logging.) *)
  Reactive.set_debug !Cli.timing;
  if !Cli.json then Emit_json.start ();
  let dce_config = Dce_config.current () in
  let num_runs = max 1 !Cli.runs in
  (* One reactive pipeline, created once and reused across runs. Downstream
     collections update automatically as files are processed. *)
  let pipeline = Dce_pipeline.create ~config:dce_config in
  let {Dce_pipeline.collection; liveness; solver} = pipeline in
  (* Collect CMT file paths once for churning *)
  let cmt_file_paths =
    if !Cli.churn > 0 then Some (collect_cmt_file_paths ~cmt_root) else None
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
  for run = 1 to num_runs do
    Timing.reset ();
    (* Clear stats at start of each run to avoid accumulation *)
    if run > 1 then Log_.Stats.clear ();
    (* Print run header first *)
    if num_runs > 1 && !Cli.timing then
      Printf.eprintf "\n=== Run %d/%d ===\n%!" run num_runs;
    (* Churn: alternate between remove and add phases *)
    (if !Cli.churn > 0 then
       match cmt_file_paths with
       | Some paths ->
         Reactive.reset_stats ();
         if run > 1 && !removed_files <> [] then (
           (* Add back previously removed files *)
           let to_add = !removed_files in
           removed_files := [];
           (* Clear removed set so these files get processed again *)
           List.iter (fun p -> Hashtbl.remove removed_set p) to_add;
           let t0 = Unix.gettimeofday () in
           let processed =
             Reactive_file_collection.process_files_batch
               (collection
                 : Reactive_analysis.t
                 :> (_, _) Reactive_file_collection.t)
               to_add
           in
           let elapsed = Unix.gettimeofday () -. t0 in
           Timing.add_churn_time elapsed;
           churn_times := elapsed :: !churn_times;
           if !Cli.timing then (
             Printf.eprintf "  Added back %d files (%.3fs)\n%!" processed
               elapsed;
             Reactive_liveness.print_stats ~t:liveness;
             Reactive_solver.print_stats ~t:solver))
         else if run > 1 then (
           (* Remove new random files *)
           let num_churn = min !Cli.churn (List.length paths) in
           let shuffled = shuffle_list paths in
           let to_remove = List.filteri (fun i _ -> i < num_churn) shuffled in
           removed_files := to_remove;
           (* Mark as removed so processCmtFiles skips them *)
           List.iter (fun p -> Hashtbl.replace removed_set p ()) to_remove;
           let t0 = Unix.gettimeofday () in
           let removed =
             Reactive_file_collection.remove_batch
               (collection
                 : Reactive_analysis.t
                 :> (_, _) Reactive_file_collection.t)
               to_remove
           in
           let elapsed = Unix.gettimeofday () -. t0 in
           Timing.add_churn_time elapsed;
           churn_times := elapsed :: !churn_times;
           if !Cli.timing then (
             Printf.eprintf "  Removed %d files (%.3fs)\n%!" removed elapsed;
             Reactive_liveness.print_stats ~t:liveness;
             Reactive_solver.print_stats ~t:solver))
       | _ -> ());
    (* Skip removed files in reactive mode *)
    let skip_file =
      if Hashtbl.length removed_set > 0 then
        Some (fun path -> Hashtbl.mem removed_set path)
      else None
    in
    run_analysis ~dce_config ~cmt_root ~pipeline ~skip_file ();
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
    else if run = num_runs then
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
  if !Cli.json then Emit_json.finish ()

let parse_argv (argv : string array) : string option =
  let analysis_kind_set = ref false in
  let cmt_root_ref = ref None in
  (* CLI override for transitive mode (overrides rescript.json if provided). *)
  let transitive_override : bool option ref = ref None in
  let usage = "reanalyze version " ^ Version.version in
  let version_and_exit () =
    print_endline usage;
    exit 0
      [@@raises exit]
  in
  let rec set_all cmt_root =
    Run_config.all ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_config () =
    Paths.Config.process_config ();
    analysis_kind_set := true
  and set_dce cmt_root =
    Run_config.dce ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_exception cmt_root =
    Run_config.exception_ ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_termination cmt_root =
    Run_config.termination ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and speclist =
    [
      ("-all", Arg.Unit (fun () -> set_all None), "Run all the analyses.");
      ( "-all-cmt",
        String (fun s -> set_all (Some s)),
        "root_path Run all the analyses for all the .cmt files under the root \
         path" );
      ("-ci", Unit (fun () -> Cli.ci := true), "Internal flag for use in CI");
      ("-config", Unit set_config, "Read the analysis mode from rescript.json");
      ( "-transitive",
        Unit (fun () -> transitive_override := Some true),
        "Force transitive reporting (overrides rescript.json \
         reanalyze.transitive)" );
      ( "-no-transitive",
        Unit (fun () -> transitive_override := Some false),
        "Disable transitive reporting (overrides rescript.json \
         reanalyze.transitive)" );
      ("-dce", Unit (fun () -> set_dce None), "Eperimental DCE");
      ("-debug", Unit (fun () -> Cli.debug := true), "Print debug information");
      ( "-dce-cmt",
        String (fun s -> set_dce (Some s)),
        "root_path Experimental DCE for all the .cmt files under the root path"
      );
      ( "-exception",
        Unit (fun () -> set_exception None),
        "Experimental exception analysis" );
      ( "-exception-cmt",
        String (fun s -> set_exception (Some s)),
        "root_path Experimental exception analysis for all the .cmt files \
         under the root path" );
      ( "-exclude-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Cli.exclude_paths := paths @ Cli.exclude_paths.contents),
        "comma-separated-path-prefixes Exclude from analysis files whose path \
         has a prefix in the list" );
      ( "-experimental",
        Set Cli.experimental,
        "Turn on experimental analyses (this option is currently unused)" );
      ( "-externals",
        Set Dead_common.Config.analyze_externals,
        "Report on externals in dead code analysis" );
      ("-json", Set Cli.json, "Print reports in json format");
      ( "-live-names",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            Cli.live_names := names @ Cli.live_names.contents),
        "comma-separated-names Consider all values with the given names as live"
      );
      ( "-live-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Cli.live_paths := paths @ Cli.live_paths.contents),
        "comma-separated-path-prefixes Consider all values whose path has a \
         prefix in the list as live" );
      ( "-suppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            run_config.suppress <- names @ run_config.suppress),
        "comma-separated-path-prefixes Don't report on files whose path has a \
         prefix in the list" );
      ( "-termination",
        Unit (fun () -> set_termination None),
        "Experimental termination analysis" );
      ( "-termination-cmt",
        String (fun s -> set_termination (Some s)),
        "root_path Experimental termination analysis for all the .cmt files \
         under the root path" );
      ( "-unsuppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            run_config.unsuppress <- names @ run_config.unsuppress),
        "comma-separated-path-prefixes Report on files whose path has a prefix \
         in the list, overriding -suppress (no-op if -suppress is not \
         specified)" );
      ( "-test-shuffle",
        Set Cli.test_shuffle,
        "Test flag: shuffle file processing order to verify order-independence"
      );
      ("-timing", Set Cli.timing, "Report internal timing of analysis phases");
      ( "-mermaid",
        Set Cli.mermaid,
        "Output Mermaid diagram of reactive pipeline" );
      ( "-runs",
        Int (fun n -> Cli.runs := n),
        "n Run analysis n times (for benchmarking cache effectiveness)" );
      ( "-churn",
        Int (fun n -> Cli.churn := n),
        "n Remove and re-add n random files between runs (tests incremental \
         correctness)" );
      ("-version", Unit version_and_exit, "Show version information and exit");
      ("--version", Unit version_and_exit, "Show version information and exit");
    ]
  in
  let current = ref 0 in
  Arg.parse_argv ~current argv speclist print_endline usage;
  if !analysis_kind_set = false then set_config ();
  (match !transitive_override with
  | None -> ()
  | Some b -> Run_config.transitive b);
  !cmt_root_ref

(** Default socket location invariant:
    - the socket lives in the project root
    - reanalyze can be called from anywhere within the project

    Project root detection reuses the same logic as reanalyze config discovery:
    walk up from a directory until we find rescript.json. *)
let cli () =
  let cmt_root = parse_argv Sys.argv in
  run_analysis_and_report ~cmt_root
[@@raises exit]

(* Re-export server module for external callers (e.g. tools/bin/main.ml).
   This keeps the wrapped-library layering intact: Reanalyze depends on internal
   modules, not the other way around. *)
module Reanalyze_server = Reanalyze_server

module Run_config = Run_config
module Dce_pipeline = Dce_pipeline
module Dce_config = Dce_config
module Log_ = Log_
module Yojson_helpers = Yojson_helpers
