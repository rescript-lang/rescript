exception Build_failure of string
exception Module_failed

type scheduled_module = {
  key: string;
  dependencies: string list;
  source: Source.module_;
  state: Build_state.module_;
  cmi_path: string;
  mutable cmi_digest_before: Digest.t option;
  prepare: unit -> unit;
  compile: is_interface:bool -> string -> Process.job;
  publish: is_interface:bool -> string -> Process.result -> string;
  package_root: string;
  is_local: bool;
  mark_warning: string -> unit;
  messages: string list ref;
  phase:
    [ `Start | `Interface of string | `Implementation of string | `Done ] ref;
}

let create ~key ~dependencies ~source ~state ~cmi_path ~prepare ~compile ~publish
    ~package_root ~is_local ~mark_warning =
  {
    key;
    dependencies;
    source;
    state;
    cmi_path;
    cmi_digest_before = None;
    prepare;
    compile;
    publish;
    package_root;
    is_local;
    mark_warning;
    messages = ref [];
    phase = ref `Start;
  }

let requires_compile scheduled = scheduled.state.compile_dirty

let file_digest path =
  try Some (Digest.file path) with Sys_error _ | Unix.Unix_error _ -> None

let run ~poll ~warning_state ~blocked_modules ~compile_assets ~build_state
    ~scheduled_modules ~compile_cleanup ~mark_compiled ~mark_had_warnings =
  let finish_successful_compile (scheduled : scheduled_module) =
    (* Only a changed interface invalidates reverse dependents. Comparing bytes
       avoids timestamp races and skips unnecessary downstream compilation. *)
    let cmi_digest_after = file_digest scheduled.cmi_path in
    let cmi_changed =
      match scheduled.cmi_digest_before, cmi_digest_after with
      | Some before, Some after -> before <> after
      | _ -> true
    in
    let cmt_path = Filename.remove_extension scheduled.cmi_path ^ ".cmt" in
    Compile_assets.refresh_cmi compile_assets ~key:scheduled.key
      ~path:scheduled.cmi_path;
    Compile_assets.refresh_cmt compile_assets ~key:scheduled.key ~path:cmt_path;
    scheduled.state.last_compiled_cmi <-
      (Compile_assets.cmi compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified));
    scheduled.state.last_compiled_cmt <-
      (Compile_assets.cmt compile_assets scheduled.key
      |> Option.map (fun entry -> entry.Compile_assets.modified));
    scheduled.state.compile_dirty <- false;
    if cmi_changed then
      Build_state.mark_dependents_compile_dirty build_state scheduled.state
        ~is_blocked:(Hashtbl.mem blocked_modules)
  in
  let warning_paths =
    scheduled_modules
    |> List.concat_map (fun (scheduled : scheduled_module) ->
         (scheduled.source.Source.implementation
         :: Option.to_list scheduled.source.Source.interface)
         |> List.map (fun path -> Filename.concat scheduled.package_root path))
  in
  Warning_state.retain_paths warning_state warning_paths;
  let works =
    scheduled_modules
    |> List.map (fun (scheduled : scheduled_module) ->
         Process.
           {
             key = scheduled.key;
             dependencies = scheduled.dependencies;
             value = scheduled;
           })
  in
  Fun.protect
    ~finally:(fun () -> List.iter (fun cleanup -> cleanup ()) compile_cleanup)
    (fun () ->
      let record_result (scheduled : scheduled_module) ~is_interface path result =
        let message =
          if Process.succeeded result then
            try
              match scheduled.publish ~is_interface path result with
              | "" ->
                Warning_state.remove warning_state
                  ~package_root:scheduled.package_root ~path;
                None
              | warning ->
                mark_had_warnings ();
                Warning_state.set warning_state ~module_name:scheduled.key
                  ~package_root:scheduled.package_root ~path ~output:warning;
                if scheduled.is_local then scheduled.mark_warning path;
                None
            with Build_failure output ->
              Warning_state.remove warning_state
                ~package_root:scheduled.package_root ~path;
              Some output
          else (
            Warning_state.remove warning_state
              ~package_root:scheduled.package_root ~path;
            Some (result.Process.stderr ^ result.Process.stdout))
        in
        Option.iter
          (fun message -> scheduled.messages := message :: !(scheduled.messages))
          message
      in
      let scheduler_failed =
        try
          Process.run_dependency_graph ~poll works
            (* A module failure blocks its dependents, while unrelated ready
               work is drained so all independent diagnostics are retained. *)
            ~is_fatal:(function Module_failed -> false | _ -> true)
            ~next:(fun scheduled result ->
              match result, !(scheduled.phase) with
              | None, `Start ->
                if scheduled.state.compile_dirty then (
                  mark_compiled ();
                  scheduled.prepare ();
                  scheduled.cmi_digest_before <- file_digest scheduled.cmi_path;
                  match scheduled.source.Source.interface with
                  | Some path ->
                    scheduled.phase := `Interface path;
                    Some (scheduled.compile ~is_interface:true path)
                  | None ->
                    let path = scheduled.source.Source.implementation in
                    scheduled.phase := `Implementation path;
                    Some (scheduled.compile ~is_interface:false path))
                else (
                  scheduled.phase := `Done;
                  None)
              | Some result, `Interface path ->
                record_result scheduled ~is_interface:true path result;
                let path = scheduled.source.Source.implementation in
                scheduled.phase := `Implementation path;
                Some (scheduled.compile ~is_interface:false path)
              | Some result, `Implementation path ->
                record_result scheduled ~is_interface:false path result;
                scheduled.phase := `Done;
                if !(scheduled.messages) <> [] then
                  raise Module_failed
                else (
                  finish_successful_compile scheduled;
                  None)
              | None, (`Interface _ | `Implementation _ | `Done)
              | Some _, (`Start | `Done) ->
                raise
                  (Project_context.Error "invalid compiler scheduler state"));
          false
        with Module_failed -> true
      in
      let failures = ref [] in
      scheduled_modules
      |> List.sort (fun (first : scheduled_module) second ->
           String.compare first.key second.key)
      |> List.iter (fun (scheduled : scheduled_module) ->
           !(scheduled.messages) |> List.rev
           |> List.iter (fun output ->
                failures := (scheduled, output) :: !failures));
      Warning_state.entries warning_state
      |> List.iter (fun entry ->
           Compiler_log.append entry.Warning_state.package_root entry.output);
      let failures = List.rev !failures in
      List.iter
        (fun ((scheduled : scheduled_module), output) ->
          Compiler_log.append scheduled.package_root output)
        failures;
      match failures, scheduler_failed with
      | [], false -> ()
      | [], true ->
        raise
          (Project_context.Error
             "compiler scheduler stopped without a diagnostic")
      | failures, _ ->
        failures |> List.map snd |> String.concat "" |> fun output ->
        raise (Build_failure output))
