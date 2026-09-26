exception Build_failure of string
exception Module_failed
type cmi_change = Build_state.cmi_change =
  | Cmi_changed
  | Cmi_unchanged
  | Cmi_change_unknown
exception Publication_failure of exn * cmi_change

type publish_result = {
  stderr: string;
  cmi_change: cmi_change;
  optimization_changed: bool;
  deferred_export: (unit -> unit) option;
  cancel_export: (unit -> unit) option;
  staged_cmi_path: string option;
}
type namespace_task = {
  task: Process.task;
  publish: Process.result -> publish_result;
}
type post_build_task = {output: string; task: Process.task}

type phase =
  | Start
  | Interface of string
  | Implementation of string
  | Post_build of {output: string; remaining: post_build_task list}
  | Done

type publication =
  | Published of publish_result
  | Failed_after_cmi_publication of {error: exn; cmi_change: cmi_change}

type recorded_publication =
  | No_publication
  | Publication_succeeded of string
  | Publication_failed of string

let capture_publication publish =
  try Published (publish ()) with
  | Publication_failure (error, cmi_change) ->
    Failed_after_cmi_publication {error; cmi_change}
  | error ->
    Failed_after_cmi_publication {error; cmi_change = Cmi_change_unknown}

type scheduled_module = {
  key: string;
  dependencies: string list;
  source: Source.module_;
  state: Build_state.module_;
  cmi_path: string;
  publication: publication option Atomic.t;
  prepare: unit -> unit;
  compile: source_kind:Source.source_kind -> string -> Process.task;
  publish:
    source_kind:Source.source_kind -> string -> Process.result -> publish_result;
  record_published_outputs: source_kind:Source.source_kind -> string -> unit;
  post_build: string -> post_build_task list;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  mutable messages: string list;
  mutable phase: phase;
  mutable staged_cmi_path: string option;
}

type candidate = {
  key: string;
  state: Build_state.module_;
  warning_paths: string list;
  make: unit -> scheduled_module;
}

let candidate_key candidate = candidate.key
let candidate_dependencies candidate = candidate.state.dependencies

type scheduled_item = Module of scheduled_module | Namespace_barrier

let create ~key ~dependencies ~source ~state ~cmi_path ~prepare ~compile
    ~publish ~record_published_outputs ~post_build ~package_root ~is_local
    ~mark_warning =
  {
    key;
    dependencies;
    source;
    state;
    cmi_path;
    publication = Atomic.make None;
    prepare;
    compile;
    publish;
    record_published_outputs;
    post_build;
    package_root;
    is_local;
    mark_warning;
    messages = [];
    phase = Start;
    staged_cmi_path = None;
  }

let candidate ~key ~state ~warning_paths ~make =
  {key; state; warning_paths; make}

let candidate_requires_compile candidate = candidate.state.compile_dirty

let run ~on_ast_invalidation ~poll ~warning_state ~compile_assets ~build_state
    ~candidates ~mark_compiled ~mark_had_warnings ~progress ~compile_step
    ~namespace_count ~verbosity =
  let dirty_propagation = Hashtbl.create 16 in
  let deferred_exports = ref [] in
  let async_exports = Queue.create () in
  let async_lock = Mutex.create () in
  let async_ready = Condition.create () in
  let async_closed = ref false in
  let async_aborted = ref false in
  let async_results = ref [] in
  let async_worker = ref None in
  (* Export ordinary implementation artifacts while other compiler jobs run.
     The scheduler still owns build-state commits after the worker is joined. *)
  let rec export_loop () =
    let next =
      Mutex.lock async_lock;
      Fun.protect
        (fun () ->
          while Queue.is_empty async_exports && not !async_closed do
            Condition.wait async_ready async_lock
          done;
          if !async_aborted || Queue.is_empty async_exports then None
          else Some (Queue.take async_exports))
        ~finally:(fun () -> Mutex.unlock async_lock)
    in
    match next with
    | None -> ()
    | Some (key, export) ->
      let result = try Ok (export ()) with error -> Error error in
      async_results := (key, result) :: !async_results;
      export_loop ()
  in
  let enqueue_async_export key export =
    if Option.is_none !async_worker then
      async_worker := Some (Domain.spawn export_loop);
    Mutex.lock async_lock;
    Fun.protect
      (fun () ->
        Queue.add (key, export) async_exports;
        Condition.signal async_ready)
      ~finally:(fun () -> Mutex.unlock async_lock)
  in
  let finish_async_exports ~abort =
    Option.iter
      (fun worker ->
        Mutex.lock async_lock;
        Fun.protect
          (fun () ->
            async_closed := true;
            async_aborted := abort;
            Condition.broadcast async_ready)
          ~finally:(fun () -> Mutex.unlock async_lock);
        Domain.join worker)
      !async_worker;
    let results = Hashtbl.create (List.length !async_results) in
    List.iter
      (fun (key, result) -> Hashtbl.replace results key result)
      !async_results;
    results
  in
  let is_async_export (scheduled : scheduled_module) source_kind =
    (* An explicit interface and a JS post-build hook have their own ordered
       publication phases, so only independent implementations use this path. *)
    source_kind = Source.Implementation
    && Option.is_none scheduled.source.Source.interface
    && scheduled.post_build scheduled.source.Source.implementation = []
  in
  let refresh_published_cmi (scheduled : scheduled_module) ~path cmi_change =
    Build_state.record_published_cmi ~dirty_propagation build_state
      ~compile_assets scheduled.state ~path cmi_change
  in
  let refresh_published_optimization (scheduled : scheduled_module) changed =
    Build_state.record_published_optimization ~dirty_propagation build_state
      scheduled.state ~changed
  in
  let finish_successful_compile (scheduled : scheduled_module) =
    let cmi_path =
      Option.value scheduled.staged_cmi_path ~default:scheduled.cmi_path
    in
    let cmt_path = Filename.remove_extension cmi_path ^ ".cmt" in
    Build_state.record_successful_compile ~compile_assets scheduled.state
      ~cmt_path
  in
  let warning_paths =
    candidates |> List.concat_map (fun candidate -> candidate.warning_paths)
  in
  Warning_state.retain_paths warning_state warning_paths;
  if Output.trace_enabled verbosity then
    candidates
    |> List.filter candidate_requires_compile
    |> List.sort (fun first second -> String.compare first.key second.key)
    |> List.iter (fun candidate ->
        Printf.printf "compile dirty: %s\n%!" candidate.key);
  (* The scheduler only needs dirty modules and their transitive dependents.
     Dependencies outside that universe already have usable artifacts, while
     keeping every module in the subprocess graph makes small edits scale with
     the whole project. *)
  let candidate_by_key = Hashtbl.create (List.length candidates) in
  List.iter
    (fun candidate -> Hashtbl.replace candidate_by_key candidate.key candidate)
    candidates;
  let universe = Hashtbl.create (List.length candidates) in
  let reached = Hashtbl.create (List.length candidates) in
  let pending = Queue.create () in
  let add_to_closure key =
    if not (Hashtbl.mem reached key) then (
      Hashtbl.add reached key ();
      if Hashtbl.mem candidate_by_key key then Hashtbl.add universe key ();
      Queue.add key pending)
  in
  candidates
  |> List.iter (fun candidate ->
      if candidate.state.compile_dirty then add_to_closure candidate.key);
  while not (Queue.is_empty pending) do
    let key = Queue.take pending in
    let state = Build_state.find_exn build_state key in
    Build_state.String_set.iter add_to_closure state.dependents
  done;
  let scheduled_modules =
    candidates
    |> List.filter (fun candidate -> Hashtbl.mem universe candidate.key)
    |> List.map (fun candidate -> candidate.make ())
  in
  Output.Progress.start progress ~step:compile_step
    ~symbol:Platform.build_symbol ~label:"Compiling"
    ~total:(namespace_count + List.length scheduled_modules);
  for _ = 1 to namespace_count do
    Output.Progress.advance progress
  done;
  let completed_modules = ref 0 in
  let scheduled_keys = Hashtbl.copy universe in
  Hashtbl.iter
    (fun key () ->
      match Build_state.find build_state key with
      | Some state when state.kind = Build_state.Namespace_map ->
        Hashtbl.replace scheduled_keys key ()
      | Some _ | None -> ())
    reached;
  let scheduled_dependencies dependencies =
    List.filter (Hashtbl.mem scheduled_keys) dependencies
  in
  let module_works =
    scheduled_modules
    |> List.map (fun (scheduled : scheduled_module) ->
        Process.
          {
            key = scheduled.key;
            dependencies = scheduled_dependencies scheduled.dependencies;
            value = Module scheduled;
          })
  in
  let namespace_works =
    Hashtbl.to_seq_keys scheduled_keys
    |> Seq.filter_map (fun key ->
        match Build_state.find build_state key with
        | Some state when state.kind = Build_state.Namespace_map ->
          Some
            Process.
              {
                key;
                dependencies = scheduled_dependencies state.dependencies;
                value = Namespace_barrier;
              }
        | Some _ | None -> None)
    |> List.of_seq
  in
  let works = module_works @ namespace_works in
  let record_publication (scheduled : scheduled_module) ~source_kind path =
    let publication = Atomic.exchange scheduled.publication None in
    match publication with
    | Some
        (Published
           {
             stderr;
             cmi_change;
             optimization_changed;
             deferred_export;
             cancel_export;
             staged_cmi_path;
           }) ->
      let cmi_path = Option.value staged_cmi_path ~default:scheduled.cmi_path in
      scheduled.staged_cmi_path <- staged_cmi_path;
      refresh_published_cmi scheduled ~path:cmi_path cmi_change;
      refresh_published_optimization scheduled optimization_changed;
      (match (deferred_export, cancel_export) with
      | Some export, Some cancel ->
        deferred_exports :=
          (scheduled, source_kind, export, cancel) :: !deferred_exports;
        if is_async_export scheduled source_kind then
          enqueue_async_export scheduled.key export
      | None, None -> ()
      | Some _, None | None, Some _ ->
        raise (Project_context.Error "incomplete deferred export"));
      scheduled.record_published_outputs ~source_kind path;
      Publication_succeeded stderr
    | Some (Failed_after_cmi_publication {error; cmi_change}) ->
      refresh_published_cmi scheduled ~path:scheduled.cmi_path cmi_change;
      scheduled.record_published_outputs ~source_kind path;
      Publication_failed (Printexc.to_string error)
    | None -> No_publication
  in
  let publish_compilation (scheduled : scheduled_module) ~source_kind path
      result =
    (if Process.succeeded result then
       let publication =
         capture_publication (fun () ->
             scheduled.publish ~source_kind path result)
       in
       Atomic.set scheduled.publication (Some publication));
    result
  in
  let record_result (scheduled : scheduled_module) ~source_kind path result =
    (* Domain tasks only compile. Publication and build-state updates remain on
       this scheduler domain after their result has been collected. *)
    let result = publish_compilation scheduled ~source_kind path result in
    let result, publication_error =
      match record_publication scheduled ~source_kind path with
      | Publication_succeeded stderr -> ({result with Process.stderr}, None)
      | Publication_failed message -> (result, Some message)
      | No_publication -> (result, None)
    in
    let message =
      match publication_error with
      | Some message ->
        Warning_state.remove warning_state ~package_root:scheduled.package_root
          ~path;
        Some message
      | None ->
        if Process.succeeded result then (
          match result.Process.stderr with
          | "" ->
            Warning_state.remove warning_state
              ~package_root:scheduled.package_root ~path;
            None
          | warning ->
            mark_had_warnings ();
            Warning_state.set warning_state ~module_name:scheduled.key
              ~package_root:scheduled.package_root ~path ~output:warning;
            if scheduled.is_local then scheduled.mark_warning path;
            None)
        else (
          Warning_state.remove warning_state
            ~package_root:scheduled.package_root ~path;
          Some (result.Process.stderr ^ result.Process.stdout))
    in
    Option.iter
      (fun message -> scheduled.messages <- message :: scheduled.messages)
      message;
    Option.is_none message
  in
  let compilation_task (scheduled : scheduled_module) ~source_kind path =
    let task = scheduled.compile ~source_kind path in
    Atomic.set scheduled.publication None;
    task
  in
  let record_post_build_result (scheduled : scheduled_module) output result =
    if Process.succeeded result then (
      if result.Process.stdout <> "" then print_string result.stdout;
      if result.stderr <> "" then prerr_string result.stderr;
      true)
    else
      let captured = result.stderr ^ result.stdout in
      let message =
        Printf.sprintf "js-post-build command failed for %s%s" output
          (if captured = "" then "" else "\n" ^ captured)
      in
      scheduled.messages <- message :: scheduled.messages;
      false
  in
  let invalidate_persistent_freshness (scheduled : scheduled_module) =
    let ocaml_dir = Filename.dirname scheduled.cmi_path in
    scheduled.source.Source.implementation
    :: Option.to_list scheduled.source.Source.interface
    |> List.iter (fun source ->
        let path = Build_artifacts.published_ast_path ~ocaml_dir source in
        on_ast_invalidation path;
        File_util.remove_file path;
        Compile_assets.refresh_ast compile_assets
          ~source:(Filename.concat scheduled.package_root source)
          ~path)
  in
  let complete_module (scheduled : scheduled_module) =
    scheduled.phase <- Done;
    Output.Progress.advance progress;
    if scheduled.messages <> [] then (
      invalidate_persistent_freshness scheduled;
      raise Module_failed)
    else (
      finish_successful_compile scheduled;
      incr completed_modules)
  in
  let continue_post_build (scheduled : scheduled_module) tasks =
    match tasks with
    | [] ->
      complete_module scheduled;
      None
    | {output; task} :: remaining ->
      scheduled.phase <- Post_build {output; remaining};
      Some task
  in
  let reconcile_unconsumed_publications () =
    List.iter
      (fun (scheduled : scheduled_module) ->
        let attempt_is_incomplete =
          match scheduled.phase with
          | Interface _ | Implementation _ | Post_build _ -> true
          | Start | Done -> false
        in
        let source =
          match scheduled.phase with
          | Interface path -> Some (Source.Interface, path)
          | Implementation path -> Some (Source.Implementation, path)
          | Start | Post_build _ | Done -> None
        in
        Option.iter
          (fun (source_kind, path) ->
            match record_publication scheduled ~source_kind path with
            | Publication_failed message ->
              scheduled.messages <- message :: scheduled.messages
            | No_publication | Publication_succeeded _ -> ())
          source;
        if attempt_is_incomplete then invalidate_persistent_freshness scheduled)
      scheduled_modules
  in
  let scheduler_failed =
    try
      let max_jobs = Compiler_execution_mode.configured_count () in
      Process.run_dependency_graph ~max_jobs ?poll
        works
        (* Finish ready independent work after a module failure; dependents of
           the failed module remain blocked. Interruptions still abort. *)
        ~on_failure:(function
          | Module_failed -> Process.Continue_independent_work
          | _ -> Process.Abort_immediately)
        ~next:(fun item result ->
          match item with
          | Namespace_barrier -> (
            match result with
            | None -> None
            | Some _ ->
              raise
                (Project_context.Error
                   "namespace scheduler barrier produced a process result"))
          | Module scheduled -> (
            match (result, scheduled.phase) with
            | None, Start ->
              if scheduled.state.compile_dirty then (
                mark_compiled ();
                scheduled.prepare ();
                match scheduled.source.Source.interface with
                | Some path ->
                  Output.Progress.debug progress ~verbosity
                    ("Compiling interface file: " ^ scheduled.key);
                  scheduled.phase <- Interface path;
                  Some
                    (compilation_task scheduled ~source_kind:Source.Interface
                       path)
                | None ->
                  let path = scheduled.source.Source.implementation in
                  Output.Progress.debug progress ~verbosity
                    ("Compiling file: " ^ scheduled.key);
                  scheduled.phase <- Implementation path;
                  Some
                    (compilation_task scheduled
                       ~source_kind:Source.Implementation path))
              else (
                scheduled.phase <- Done;
                incr completed_modules;
                Output.Progress.advance progress;
                None)
            | Some result, Interface path ->
              ignore
                (record_result scheduled ~source_kind:Source.Interface path
                   result);
              let path = scheduled.source.Source.implementation in
              Output.Progress.debug progress ~verbosity
                ("Compiling file: " ^ scheduled.key);
              scheduled.phase <- Implementation path;
              Some
                (compilation_task scheduled ~source_kind:Source.Implementation
                   path)
            | Some result, Implementation path ->
              if
                record_result scheduled ~source_kind:Source.Implementation path
                  result
              then continue_post_build scheduled (scheduled.post_build path)
              else (
                complete_module scheduled;
                None)
            | Some result, Post_build {output; remaining} ->
              if record_post_build_result scheduled output result then
                continue_post_build scheduled remaining
              else (
                complete_module scheduled;
                None)
            | None, (Interface _ | Implementation _ | Post_build _ | Done)
            | Some _, (Start | Done) ->
              raise (Project_context.Error "invalid compiler scheduler state")));
      false
    with exn -> (
      reconcile_unconsumed_publications ();
      match exn with
      | Module_failed -> true
      | _ ->
        ignore (finish_async_exports ~abort:true);
        List.iter
          (fun ((scheduled : scheduled_module), _, _, cancel) ->
            cancel ();
            scheduled.state.compile_dirty <- true;
            invalidate_persistent_freshness scheduled)
          !deferred_exports;
        raise exn)
  in
  let async_export_results = finish_async_exports ~abort:false in
  List.rev !deferred_exports
  |> List.iter (fun (scheduled, source_kind, export, cancel) ->
      try
        if is_async_export scheduled source_kind then
          match Hashtbl.find_opt async_export_results scheduled.key with
          | Some (Ok ()) -> ()
          | Some (Error error) -> raise error
          | None ->
            raise
              (Project_context.Error
                 ("missing artifact export for " ^ scheduled.key))
        else export ();
        refresh_published_cmi scheduled ~path:scheduled.cmi_path Cmi_unchanged;
        if
          source_kind = Source.Implementation
          && scheduled.phase = Done && scheduled.messages = []
        then
          Build_state.record_successful_compile ~compile_assets scheduled.state
            ~cmt_path:(Filename.remove_extension scheduled.cmi_path ^ ".cmt")
      with error ->
        cancel ();
        scheduled.state.compile_dirty <- true;
        invalidate_persistent_freshness scheduled;
        scheduled.messages <- Printexc.to_string error :: scheduled.messages);
  Output.Progress.finish progress;
  Output.trace ~verbosity
    (Printf.sprintf "Compiled %d out of %d in the universe" !completed_modules
       (List.length scheduled_modules));
  let failures =
    scheduled_modules
    |> List.sort (fun (first : scheduled_module) second ->
        String.compare first.key second.key)
    |> List.concat_map (fun scheduled ->
        scheduled.messages |> List.rev
        |> List.map (fun output -> (scheduled, output)))
  in
  Warning_state.entries warning_state
  |> List.iter (fun entry ->
      Compiler_log.append entry.Warning_state.package_root entry.output);
  List.iter
    (fun ((scheduled : scheduled_module), output) ->
      Compiler_log.append scheduled.package_root output)
    failures;
  match (failures, scheduler_failed) with
  | [], false -> ()
  | [], true ->
    raise
      (Project_context.Error "compiler scheduler stopped without a diagnostic")
  | failures, _ ->
    failures |> List.map snd |> String.concat "" |> fun output ->
    raise (Build_failure output)
