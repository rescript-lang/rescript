module Process = struct
  type result = {status: Unix.process_status; stdout: string}

  let rec waitpid pid =
    match Unix.waitpid [] pid with
    | _, status -> status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid

  let with_temp_file f =
    let path, channel =
      Filename.open_temp_file ~mode:[Open_binary] "rescript-lsp-process-" ".tmp"
    in
    Fun.protect
      ~finally:(fun () ->
        close_out_noerr channel;
        try Sys.remove path with Sys_error _ -> ())
      (fun () -> f path channel)

  let run_unix ~stdin program args =
    (* Windows cannot select() on anonymous pipes. Files avoid pipe-buffer
       deadlocks even if a child writes both streams before reading its stdin. *)
    with_temp_file (fun input_path input ->
        output_string input stdin;
        flush input;
        with_temp_file (fun output_path output ->
            with_temp_file (fun _ error ->
                In_channel.with_open_bin input_path (fun input ->
                    let pid =
                      Unix.create_process program
                        (Array.of_list (program :: args))
                        (Unix.descr_of_in_channel input)
                        (Unix.descr_of_out_channel output)
                        (Unix.descr_of_out_channel error)
                    in
                    let reaped = ref false in
                    Fun.protect
                      ~finally:(fun () ->
                        if not !reaped then (
                          (try Unix.kill pid Sys.sigkill
                           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
                          ignore (waitpid pid)))
                      (fun () ->
                        let status = waitpid pid in
                        reaped := true;
                        let stdout =
                          In_channel.with_open_bin output_path
                            In_channel.input_all
                        in
                        {status; stdout})))))

  let run_eio ~stdin program args =
    (* The worker needs its own effect handler: Eio operations cannot run in a
       plain systhread using the calling fiber's scheduler. *)
    Eio_main.run (fun env ->
        let stdout = Buffer.create 4096 in
        let status =
          Eio.Switch.run (fun sw ->
              let manager = Eio.Stdenv.process_mgr env in
              let input, writer = Eio.Process.pipe ~sw manager in
              let process =
                Eio.Process.spawn ~sw manager ~executable:program ~stdin:input
                  ~stdout:(Eio.Flow.buffer_sink stdout)
                  ~stderr:Eio.Flow.null (program :: args)
              in
              Eio.Flow.close input;
              Eio.Fiber.fork ~sw (fun () ->
                  Fun.protect
                    ~finally:(fun () -> Eio.Flow.close writer)
                    (fun () ->
                      try Eio.Flow.copy (Eio.Flow.string_source stdin) writer
                      with Eio.Io (Eio.Net.E (Connection_reset _), _) ->
                        (* A child may exit or close stdin without consuming all
                           input. Its exit status and diagnostics still matter. *)
                        ()));
              match Eio.Process.await process with
              | `Exited code -> Unix.WEXITED code
              | `Signaled signal -> Unix.WSIGNALED signal)
        in
        (* Switch completion also waits for the output copying fibers. *)
        {status; stdout = Buffer.contents stdout})

  let run ~stdin program args =
    Eio_unix.run_in_systhread ~label:"lsp process" (fun () ->
        if Sys.win32 then run_unix ~stdin program args
        else run_eio ~stdin program args)
end

open Lsp

let run ~uri ~(state : State.t) =
  let doc = Document_store.get ~uri state.store in
  let source = Document.text doc in
  let kind_file = Document.kind doc in

  let executable =
    let executable_name = "rescript" in
    let root_path = State.workspace_root state |> Uri.to_path in
    let ( /+ ) = Filename.concat in
    root_path /+ "node_modules" /+ ".bin" /+ executable_name
  in

  match Fs.exists ~follow:true ~fs:state.fs executable with
  | false ->
    Error
      (Printf.sprintf "Unable to find rescript binary at %s"
         (Filename.dirname executable))
  | true -> (
    let extension_name = Document.extension_of_kind kind_file in
    try
      let result =
        Process.run ~stdin:source executable
          ["format"; "--stdin"; "." ^ extension_name]
      in
      match result.status with
      | Unix.WEXITED 0 -> Ok (Diff.edit ~from:source ~to_:result.stdout)
      | _ ->
        Error
          (Printf.sprintf "Failed to format %s"
             (uri |> Uri.to_path |> Filename.basename))
    with exn ->
      Error
        (Printf.sprintf "Unexpected result to format %s. %s" (Uri.to_path uri)
           (Printexc.to_string exn)))
