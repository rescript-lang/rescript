module ResParser = Res_core
module Doc = Res_doc
module CommentTable = Res_comments_table
module Parser = Res_parser
module Printer = Res_printer

module IO : sig
  val read_file : string -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunk_size = 32768

  let read_file filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunk_size in
    let chunk = (Bytes.create [@doesNotRaise]) chunk_size in
    let rec loop () =
      let len =
        try input chan chunk 0 chunk_size with Invalid_argument _ -> 0
      in
      if len == 0 then (
        close_in_noerr chan;
        Buffer.contents buffer)
      else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ())
    in
    loop ()
end

module Time : sig
  type t

  val now : unit -> t

  val to_uint64 : t -> int64 [@@live]

  (* let of_uint64_ns ns = ns *)

  val nanosecond : t [@@live]
  val microsecond : t [@@live]
  val millisecond : t [@@live]
  val second : t [@@live]
  val minute : t [@@live]
  val hour : t [@@live]

  val zero : t

  val diff : t -> t -> t
  val add : t -> t -> t
  val print : t -> float
end = struct
  (* nanoseconds *)
  type t = int64

  let zero = 0L

  let to_uint64 s = s

  let nanosecond = 1L
  let microsecond = Int64.mul 1000L nanosecond
  let millisecond = Int64.mul 1000L microsecond
  let second = Int64.mul 1000L millisecond
  let minute = Int64.mul 60L second
  let hour = Int64.mul 60L minute

  (* TODO: we could do this inside caml_absolute_time *)
  external init : unit -> unit = "caml_mach_initialize"
  let () = init ()
  external now : unit -> t = "caml_mach_absolute_time"

  let diff t1 t2 = Int64.sub t2 t1
  let add t1 t2 = Int64.add t1 t2
  let print t = Int64.to_float t *. 1e-6
end

module Benchmark : sig
  type t

  val make : name:string -> f:(t -> unit) -> unit -> t
  val launch : t -> unit
  val report : t -> Yojson.t list
end = struct
  type t = {
    name: string;
    mutable start: Time.t;
    mutable n: int; (* current iterations count *)
    mutable duration: Time.t;
    bench_func: t -> unit;
    mutable timer_on: bool;
    (* mutable result: benchmarkResult; *)
    (* The initial states *)
    mutable start_allocs: float;
    mutable start_bytes: float;
    (* The net total of this test after being run. *)
    mutable net_allocs: float;
    mutable net_bytes: float;
  }

  let report b =
    [
      `Assoc
        [
          ( "name",
            `String (Format.sprintf "%s - time (%d iterations)" b.name b.n) );
          ("unit", `String "ms");
          ("value", `Float (Time.print b.duration));
        ];
      `Assoc
        [
          ("name", `String (Format.sprintf "%s - allocations" b.name));
          ("unit", `String "");
          ("value", `Int (int_of_float (b.net_allocs /. float_of_int b.n)));
        ];
      `Assoc
        [
          ("name", `String (Format.sprintf "%s - bytes allocated" b.name));
          ("unit", `String "");
          ("value", `Int (int_of_float (b.net_bytes /. float_of_int b.n)));
        ];
    ]

  let make ~name ~f () =
    {
      name;
      start = Time.zero;
      n = 0;
      bench_func = f;
      duration = Time.zero;
      timer_on = false;
      start_allocs = 0.;
      start_bytes = 0.;
      net_allocs = 0.;
      net_bytes = 0.;
    }

  (* total amount of memory allocated by the program since it started in words *)
  let mallocs () =
    let stats = Gc.quick_stat () in
    stats.minor_words +. stats.major_words -. stats.promoted_words

  let start_timer b =
    if not b.timer_on then (
      let allocated_words = mallocs () in
      b.start_allocs <- allocated_words;
      b.start_bytes <- allocated_words *. 8.;
      b.start <- Time.now ();
      b.timer_on <- true)

  let stop_timer b =
    if b.timer_on then (
      let allocated_words = mallocs () in
      let diff = Time.diff b.start (Time.now ()) in
      b.duration <- Time.add b.duration diff;
      b.net_allocs <- b.net_allocs +. (allocated_words -. b.start_allocs);
      b.net_bytes <- b.net_bytes +. ((allocated_words *. 8.) -. b.start_bytes);
      b.timer_on <- false)

  let reset_timer b =
    if b.timer_on then (
      let allocated_words = mallocs () in
      b.start_allocs <- allocated_words;
      b.net_allocs <- allocated_words *. 8.;
      b.start <- Time.now ());
    b.net_allocs <- 0.;
    b.net_bytes <- 0.

  let run_iteration b n =
    Gc.full_major ();
    b.n <- n;
    reset_timer b;
    start_timer b;
    b.bench_func b;
    stop_timer b

  let launch b =
    (* 150 runs * all the benchmarks means around 1m of benchmark time *)
    for n = 1 to 150 do
      run_iteration b n
    done
end

module Benchmarks : sig
  val run : unit -> unit
end = struct
  type action = Parse | Print

  let string_of_action action =
    match action with
    | Parse -> "Parse"
    | Print -> "Print"

  let parse_rescript src filename =
    let p = Parser.make src filename in
    let structure = ResParser.parse_implementation p in
    assert (p.diagnostics == []);
    structure

  let data_dir = "tests/syntax_benchmarks/data"

  let benchmark (filename, action) =
    let path = Filename.concat data_dir filename in
    let src = IO.read_file path in
    let name = string_of_action action ^ " " ^ filename in
    let benchmark_fn =
      match action with
      | Parse ->
        fun _ ->
          let _ = Sys.opaque_identity (parse_rescript src path) in
          ()
      | Print ->
        let p = Parser.make src path in
        let ast = ResParser.parse_implementation p in
        fun _ ->
          let _ =
            Sys.opaque_identity
              (let cmt_tbl = CommentTable.make () in
               let comments = List.rev p.Parser.comments in
               let () = CommentTable.walk_structure ast cmt_tbl comments in
               Doc.to_string ~width:80 (Printer.print_structure ast cmt_tbl))
          in
          ()
    in
    let b = Benchmark.make ~name ~f:benchmark_fn () in
    Benchmark.launch b;
    Benchmark.report b

  let specs =
    [
      ("RedBlackTree.res", Parse);
      ("RedBlackTree.res", Print);
      ("RedBlackTreeNoComments.res", Print);
      ("Napkinscript.res", Parse);
      ("Napkinscript.res", Print);
      ("HeroGraphic.res", Parse);
      ("HeroGraphic.res", Print);
    ]

  let run () =
    List.to_seq specs
    |> Seq.flat_map (fun spec -> benchmark spec |> List.to_seq)
    |> Seq.iteri (fun i json ->
           print_endline (if i == 0 then "[" else ",");
           print_string (Yojson.to_string json));
    print_newline ();
    print_endline "]"
end

let () = Benchmarks.run ()
