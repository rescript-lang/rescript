(* See the interface. The variable is read once, while the module
   initializes, and the events stay in memory until the process exits. *)
let path =
  match Sys.getenv_opt "RESCRIPT_TRACE" with
  | None | Some "" -> None
  | Some path -> Some path

let enabled = Option.is_some path

let now = Unix.gettimeofday

let origin = now ()

let pid = Unix.getpid ()

(* Whole microseconds since the process started, the unit of the format. A
   span's duration is the difference of its rounded end points, so spans that
   share an end point nest exactly. *)
let microseconds time = int_of_float (Float.round ((time -. origin) *. 1e6))

(* The main domain's track is 0 and worker slot [n]'s is [n + 1]. Worker pools
   are created per dependency graph, so slots rather than domain ids keep the
   number of tracks at the pool size. *)
let slot_key : int option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let set_worker_slot slot = Domain.DLS.set slot_key (Some slot)

(* Far above any worker slot *)
let writer_track = 1000

let set_writer_track () = Domain.DLS.set slot_key (Some (writer_track - 1))

let track () =
  match Domain.DLS.get slot_key with
  | None -> 0
  | Some slot -> slot + 1

let mutex = Mutex.create ()

(* Newest first *)
let events : Yojson.Basic.t list ref = ref []

let named_tracks = Hashtbl.create 16

let track_names tid =
  let name =
    if tid = 0 then "main"
    else if tid = writer_track then "writer"
    else Printf.sprintf "worker %d" tid
  in
  let metadata name args =
    `Assoc
      [
        ("ph", `String "M");
        ("name", `String name);
        ("pid", `Int pid);
        ("tid", `Int tid);
        ("args", `Assoc args);
      ]
  in
  [
    metadata "thread_name" [("name", `String name)];
    metadata "thread_sort_index" [("sort_index", `Int tid)];
  ]

let add event =
  let tid = track () in
  Process_child.with_lock mutex (fun () ->
      if not (Hashtbl.mem named_tracks tid) then (
        Hashtbl.add named_tracks tid ();
        events := List.rev_append (track_names tid) !events);
      events := event tid :: !events)

let complete ~name ?cat ~args ~start ~finish () =
  add (fun tid ->
      `Assoc
        ([
           ("ph", `String "X");
           ("name", `String name);
           ("pid", `Int pid);
           ("tid", `Int tid);
           ("ts", `Int (microseconds start));
           ("dur", `Int (max 0 (microseconds finish - microseconds start)));
         ]
        @ (match cat with
          | Some cat -> [("cat", `String cat)]
          | None -> [])
        @ if args = [] then [] else [("args", `Assoc args)]))

let span ~name ?(args = []) ~start () =
  if enabled then complete ~name ~args ~start ~finish:(now ()) ()

let counters ~name values =
  if enabled then
    let time = now () in
    add (fun tid ->
        `Assoc
          [
            ("ph", `String "C");
            ("name", `String name);
            ("pid", `Int pid);
            ("tid", `Int tid);
            ("ts", `Int (microseconds time));
            ( "args",
              `Assoc (List.map (fun (key, value) -> (key, `Int value)) values)
            );
          ])

(* Allocated words as the runtime counts them: minor plus major, without the
   promoted ones, which both count *)
let allocated_words () =
  let minor, promoted, major = Gc.counters () in
  (minor +. major -. promoted, promoted)

let compiler_request ~phase ~input f =
  if not enabled then f ()
  else
    let start = now () in
    let typed = ref None in
    Compile_phase_hook.set_typing_finished (fun () -> typed := Some (now ()));
    let allocated, promoted = allocated_words () in
    let stat = Gc.quick_stat () in
    Fun.protect f ~finally:(fun () ->
        let finish = now () in
        Compile_phase_hook.set_typing_finished ignore;
        let allocated', promoted' = allocated_words () in
        let stat' = Gc.quick_stat () in
        complete ~name:(Filename.basename input) ~cat:phase
          ~args:
            [
              ("input", `String input);
              ("allocated_words", `Float (allocated' -. allocated));
              ("promoted_words", `Float (promoted' -. promoted));
              ( "minor_collections",
                `Int (stat'.Gc.minor_collections - stat.Gc.minor_collections) );
              ( "major_collections",
                `Int (stat'.Gc.major_collections - stat.Gc.major_collections) );
            ]
          ~start ~finish ();
        match !typed with
        | None -> ()
        | Some typed ->
          complete ~name:"type checking" ~args:[] ~start ~finish:typed ();
          complete ~name:"code generation" ~args:[] ~start:typed ~finish ())

(* Commands that record nothing, such as a clean after a build, leave the
   file alone *)
let write path =
  match !events with
  | [] -> ()
  | recorded -> (
    let process_name =
      `Assoc
        [
          ("ph", `String "M");
          ("name", `String "process_name");
          ("pid", `Int pid);
          ("args", `Assoc [("name", `String "rescript")]);
        ]
    in
    let trace =
      `Assoc
        [
          ("traceEvents", `List (process_name :: List.rev recorded));
          ("displayTimeUnit", `String "ms");
        ]
    in
    try Yojson.Basic.to_file path trace
    with Sys_error message ->
      Printf.eprintf "Could not write the build trace: %s\n%!" message)

let () =
  match path with
  | None -> ()
  | Some path ->
    at_exit (fun () -> Process_child.with_lock mutex (fun () -> write path))
