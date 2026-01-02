(* Reanalyze server implementation.
   Kept in a separate module so Reanalyze.ml stays focused on analysis logic. *)

(** Default socket location invariant:
    - the socket lives in the project root
    - reanalyze can be called from anywhere within the project

    Project root detection reuses the same logic as reanalyze config discovery:
    walk up from a directory until we find rescript.json or bsconfig.json. *)
let default_socket_filename = ".rescript-reanalyze.sock"

let project_root_from_dir (dir : string) : string option =
  try Some (Paths.findProjectRoot ~dir) with _ -> None

let with_cwd_dir (cwd : string) (f : unit -> 'a) : 'a =
  let old = Sys.getcwd () in
  Sys.chdir cwd;
  Fun.protect ~finally:(fun () -> Sys.chdir old) f

let default_socket_for_dir_exn (dir : string) : string * string =
  match project_root_from_dir dir with
  | Some root ->
    (* IMPORTANT: use a relative socket path (name only) to avoid Unix domain
       socket path-length limits (common on macOS). The socket file still lives
       in the project root directory. *)
    (root, default_socket_filename)
  | None ->
    (* Match reanalyze behavior: it cannot run outside a project root. *)
    Printf.eprintf "Error: cannot find project root containing %s.\n%!"
      Paths.rescriptJson;
    exit 2

let default_socket_for_current_project_exn () : string * string =
  default_socket_for_dir_exn (Sys.getcwd ())

type request = unit

type response = {exit_code: int; stdout: string; stderr: string}

(** Try to send a request to a running server. Returns None if no server is running. *)
let try_request ~socket_dir ~socket_path : response option =
  let try_ () =
    if not (Sys.file_exists socket_path) then None
    else
      let sockaddr = Unix.ADDR_UNIX socket_path in
      let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      try
        Unix.connect sock sockaddr;
        let ic = Unix.in_channel_of_descr sock in
        let oc = Unix.out_channel_of_descr sock in
        Fun.protect
          ~finally:(fun () ->
            close_out_noerr oc;
            close_in_noerr ic)
          (fun () ->
            let req : request = () in
            Marshal.to_channel oc req [Marshal.No_sharing];
            flush oc;
            let (resp : response) = Marshal.from_channel ic in
            Some resp)
      with _ ->
        (try Unix.close sock with _ -> ());
        None
  in
  match socket_dir with
  | None -> try_ ()
  | Some dir -> with_cwd_dir dir try_

let try_request_default () : response option =
  let socket_dir, socket_path = default_socket_for_current_project_exn () in
  try_request ~socket_dir:(Some socket_dir) ~socket_path

module Server = struct
  let ( let* ) x f =
    match x with
    | Ok v -> f v
    | Error _ as e -> e

  let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

  type server_config = {socket_path: string; cwd: string option}

  type server_stats = {mutable request_count: int}

  let skip_compact =
    Sys.getenv_opt "RESCRIPT_REANALYZE_SERVER_SKIP_COMPACT" = Some "1"

  let bytes_per_word = Sys.word_size / 8

  let mb_of_words (words : int) : float =
    float_of_int words *. float_of_int bytes_per_word /. (1024.0 *. 1024.0)

  let pp_mb (mb : float) : string = Printf.sprintf "%.1fMB" mb

  let gc_live_mb () : float =
    let s = Gc.quick_stat () in
    mb_of_words s.live_words

  type server_state = {
    parse_argv: string array -> string option;
    run_analysis:
      dce_config:DceConfig.t ->
      cmtRoot:string option ->
      reactive_collection:ReactiveAnalysis.t option ->
      reactive_merge:ReactiveMerge.t option ->
      reactive_liveness:ReactiveLiveness.t option ->
      reactive_solver:ReactiveSolver.t option ->
      skip_file:(string -> bool) option ->
      ?file_stats:ReactiveAnalysis.processing_stats ->
      unit ->
      unit;
    config: server_config;
    cmtRoot: string option;
    dce_config: DceConfig.t;
    reactive_collection: ReactiveAnalysis.t;
    reactive_merge: ReactiveMerge.t;
    reactive_liveness: ReactiveLiveness.t;
    reactive_solver: ReactiveSolver.t;
    stats: server_stats;
  }

  type request_info = {
    req_num: int;
    elapsed_ms: float;
    issue_count: int;
    dead_count: int;
    live_count: int;
    processed_files: int;
    cached_files: int;
  }

  let usage () =
    Printf.eprintf
      {|Usage:
  rescript-tools reanalyze-server [--socket <path>]

Examples:
  rescript-tools reanalyze-server
|}

  let parse_cli_args () : (server_config, string) result =
    let args =
      Array.to_list Sys.argv |> List.tl |> List.filter (fun s -> s <> "")
    in
    let rec loop socket_path rest =
      match rest with
      | "--socket" :: path :: tl -> loop (Some path) tl
      | [] ->
        let start_dir =
          try Unix.realpath (Sys.getcwd ())
          with Unix.Unix_error _ -> Sys.getcwd ()
        in
        let project_root, _ = default_socket_for_dir_exn start_dir in
        let cwd = Some project_root in
        let socket_path =
          match socket_path with
          | Some p -> p
          | None -> default_socket_filename
        in
        Ok {socket_path; cwd}
      | x :: _ when String.length x > 0 && x.[0] = '-' ->
        errorf "Unknown server option: %s" x
      | x :: _ -> errorf "Unexpected argument before --: %s" x
    in
    loop None args

  let unlink_if_exists path =
    match Sys.file_exists path with
    | true -> ( try Sys.remove path with Sys_error _ -> ())
    | false -> ()

  let setup_socket_cleanup ~cwd_opt ~socket_path =
    let cleanup () =
      match cwd_opt with
      | None -> unlink_if_exists socket_path
      | Some dir -> with_cwd_dir dir (fun () -> unlink_if_exists socket_path)
    in
    at_exit cleanup;
    let install sig_ =
      try
        Sys.set_signal sig_
          (Sys.Signal_handle
             (fun _ ->
               cleanup ();
               exit 130))
      with _ -> ()
    in
    install Sys.sigint;
    install Sys.sigterm;
    install Sys.sighup;
    install Sys.sigquit

  let with_cwd (cwd_opt : string option) f =
    match cwd_opt with
    | None -> f ()
    | Some cwd ->
      let old = Sys.getcwd () in
      Sys.chdir cwd;
      Fun.protect ~finally:(fun () -> Sys.chdir old) f

  let capture_stdout_stderr (f : unit -> unit) :
      (string * string, string) result =
    let tmp_dir =
      match Sys.getenv_opt "TMPDIR" with
      | Some d -> d
      | None -> Filename.get_temp_dir_name ()
    in
    let stdout_path = Filename.temp_file ~temp_dir:tmp_dir "reanalyze" ".stdout"
    and stderr_path =
      Filename.temp_file ~temp_dir:tmp_dir "reanalyze" ".stderr"
    in
    let orig_out = Unix.dup Unix.stdout and orig_err = Unix.dup Unix.stderr in
    let out_fd =
      Unix.openfile stdout_path
        [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
        0o644
    in
    let err_fd =
      Unix.openfile stderr_path
        [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
        0o644
    in
    let restore () =
      (try Unix.dup2 orig_out Unix.stdout with _ -> ());
      (try Unix.dup2 orig_err Unix.stderr with _ -> ());
      (try Unix.close orig_out with _ -> ());
      (try Unix.close orig_err with _ -> ());
      (try Unix.close out_fd with _ -> ());
      try Unix.close err_fd with _ -> ()
    in
    let read_all path =
      try
        let ic = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let len = in_channel_length ic in
            really_input_string ic len)
      with _ -> ""
    in
    let run () =
      Unix.dup2 out_fd Unix.stdout;
      Unix.dup2 err_fd Unix.stderr;
      try
        f ();
        flush_all ();
        Ok (read_all stdout_path, read_all stderr_path)
      with exn ->
        flush_all ();
        let bt = Printexc.get_backtrace () in
        let msg =
          if bt = "" then Printexc.to_string exn
          else Printf.sprintf "%s\n%s" (Printexc.to_string exn) bt
        in
        Error msg
    in
    Fun.protect
      ~finally:(fun () ->
        restore ();
        unlink_if_exists stdout_path;
        unlink_if_exists stderr_path)
      run

  let init_state ~(parse_argv : string array -> string option)
      ~(run_analysis :
         dce_config:DceConfig.t ->
         cmtRoot:string option ->
         reactive_collection:ReactiveAnalysis.t option ->
         reactive_merge:ReactiveMerge.t option ->
         reactive_liveness:ReactiveLiveness.t option ->
         reactive_solver:ReactiveSolver.t option ->
         skip_file:(string -> bool) option ->
         ?file_stats:ReactiveAnalysis.processing_stats ->
         unit ->
         unit) (config : server_config) : (server_state, string) result =
    Printexc.record_backtrace true;
    with_cwd config.cwd (fun () ->
        (* Editor mode only: the server always behaves like `reanalyze -json`. *)
        let cmtRoot = parse_argv [|"reanalyze"; "-json"|] in
        (* Force reactive mode in server. *)
        Cli.reactive := true;
        (* Keep server requests single-run and deterministic. *)
        if !Cli.runs <> 1 then
          errorf
            "reanalyze-server does not support -runs (got %d). Start the \
             server with editor-like args only."
            !Cli.runs
        else if !Cli.churn <> 0 then
          errorf
            "reanalyze-server does not support -churn (got %d). Start the \
             server with editor-like args only."
            !Cli.churn
        else
          let dce_config = DceConfig.current () in
          let reactive_collection =
            ReactiveAnalysis.create ~config:dce_config
          in
          let file_data_collection =
            ReactiveAnalysis.to_file_data_collection reactive_collection
          in
          let reactive_merge = ReactiveMerge.create file_data_collection in
          let reactive_liveness =
            ReactiveLiveness.create ~merged:reactive_merge
          in
          let value_refs_from =
            if dce_config.DceConfig.run.transitive then None
            else Some reactive_merge.ReactiveMerge.value_refs_from
          in
          let reactive_solver =
            ReactiveSolver.create ~decls:reactive_merge.ReactiveMerge.decls
              ~live:reactive_liveness.ReactiveLiveness.live
              ~annotations:reactive_merge.ReactiveMerge.annotations
              ~value_refs_from ~config:dce_config
          in
          Ok
            {
              parse_argv;
              run_analysis;
              config;
              cmtRoot;
              dce_config;
              reactive_collection;
              reactive_merge;
              reactive_liveness;
              reactive_solver;
              stats = {request_count = 0};
            })

  let run_one_request (state : server_state) (_req : request) :
      request_info * response =
    state.stats.request_count <- state.stats.request_count + 1;
    let req_num = state.stats.request_count in
    let t_start = Unix.gettimeofday () in
    let response_of_result res =
      match res with
      | Ok (stdout, stderr) -> {exit_code = 0; stdout; stderr}
      | Error err -> {exit_code = 1; stdout = ""; stderr = err ^ "\n"}
    in
    let issue_count = ref 0 in
    let dead_count = ref 0 in
    let live_count = ref 0 in
    let file_stats : ReactiveAnalysis.processing_stats =
      {total_files = 0; processed = 0; from_cache = 0}
    in
    let resp =
      with_cwd
        (* Always run from the server's project root; client cwd is not stable in VS Code. *)
        state.config.cwd (fun () ->
          capture_stdout_stderr (fun () ->
              Log_.Color.setup ();
              Timing.enabled := !Cli.timing;
              Reactive.set_debug !Cli.timing;
              Timing.reset ();
              Log_.Stats.clear ();
              (* Editor mode only: always JSON output. *)
              Cli.json := true;
              (* Match direct CLI output (a leading newline before the JSON array). *)
              Printf.printf "\n";
              EmitJson.start ();
              state.run_analysis ~dce_config:state.dce_config
                ~cmtRoot:state.cmtRoot
                ~reactive_collection:(Some state.reactive_collection)
                ~reactive_merge:(Some state.reactive_merge)
                ~reactive_liveness:(Some state.reactive_liveness)
                ~reactive_solver:(Some state.reactive_solver) ~skip_file:None
                ~file_stats ();
              issue_count := Log_.Stats.get_issue_count ();
              let d, l = ReactiveSolver.stats ~t:state.reactive_solver in
              dead_count := d;
              live_count := l;
              Log_.Stats.report ~config:state.dce_config;
              Log_.Stats.clear ();
              EmitJson.finish ())
          |> response_of_result)
    in
    let t_end = Unix.gettimeofday () in
    let elapsed_ms = (t_end -. t_start) *. 1000.0 in
    ( {
        req_num;
        elapsed_ms;
        issue_count = !issue_count;
        dead_count = !dead_count;
        live_count = !live_count;
        processed_files = file_stats.processed;
        cached_files = file_stats.from_cache;
      },
      resp )

  let serve (state : server_state) : unit =
    with_cwd state.config.cwd (fun () ->
        unlink_if_exists state.config.socket_path;
        setup_socket_cleanup ~cwd_opt:state.config.cwd
          ~socket_path:state.config.socket_path;
        let sockaddr = Unix.ADDR_UNIX state.config.socket_path in
        let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.bind sock sockaddr;
        Unix.listen sock 10;
        Printf.eprintf "reanalyze-server listening on %s/%s\n%!" (Sys.getcwd ())
          state.config.socket_path;
        Fun.protect
          ~finally:(fun () -> unlink_if_exists state.config.socket_path)
          (fun () ->
            let rec loop () =
              let client, _ = Unix.accept sock in
              let ic = Unix.in_channel_of_descr client in
              let oc = Unix.out_channel_of_descr client in
              let info_ref : request_info option ref = ref None in
              Fun.protect
                ~finally:(fun () ->
                  close_out_noerr oc;
                  close_in_noerr ic)
                (fun () ->
                  let (req : request) = Marshal.from_channel ic in
                  let info, resp = run_one_request state req in
                  Marshal.to_channel oc resp [Marshal.No_sharing];
                  flush oc;
                  info_ref := Some info);
              (match !info_ref with
              | None -> ()
              | Some info ->
                if not skip_compact then Gc.compact ();
                let live_mb = gc_live_mb () in
                Printf.eprintf
                  "[request #%d] %.1fms | issues: %d | dead: %d | live: %d | \
                   files: %d processed, %d cached | mem: %s\n\
                   %!"
                  info.req_num info.elapsed_ms info.issue_count info.dead_count
                  info.live_count info.processed_files info.cached_files
                  (pp_mb live_mb));
              loop ()
            in
            loop ()))

  let cli ~(parse_argv : string array -> string option)
      ~(run_analysis :
         dce_config:DceConfig.t ->
         cmtRoot:string option ->
         reactive_collection:ReactiveAnalysis.t option ->
         reactive_merge:ReactiveMerge.t option ->
         reactive_liveness:ReactiveLiveness.t option ->
         reactive_solver:ReactiveSolver.t option ->
         skip_file:(string -> bool) option ->
         ?file_stats:ReactiveAnalysis.processing_stats ->
         unit ->
         unit) () =
    match parse_cli_args () with
    | Ok config -> (
      match init_state ~parse_argv ~run_analysis config with
      | Ok state -> serve state
      | Error msg ->
        Printf.eprintf "reanalyze-server: %s\n%!" msg;
        usage ();
        exit 2)
    | Error msg ->
      Printf.eprintf "reanalyze-server: %s\n%!" msg;
      usage ();
      exit 2
end

let server_cli ~(parse_argv : string array -> string option)
    ~(run_analysis :
       dce_config:DceConfig.t ->
       cmtRoot:string option ->
       reactive_collection:ReactiveAnalysis.t option ->
       reactive_merge:ReactiveMerge.t option ->
       reactive_liveness:ReactiveLiveness.t option ->
       reactive_solver:ReactiveSolver.t option ->
       skip_file:(string -> bool) option ->
       ?file_stats:ReactiveAnalysis.processing_stats ->
       unit ->
       unit) () =
  Server.cli ~parse_argv ~run_analysis ()

(* NOTE: intentionally no reanalyze-server-request CLI.
   We expose only two commands: reanalyze and reanalyze-server. *)
