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

  val [@warning "-32"] to_uint64 : t -> int64

  (* let of_uint64_ns ns = ns *)

  val [@warning "-32"] nanosecond : t
  val [@warning "-32"] microsecond : t
  val [@warning "-32"] millisecond : t
  val [@warning "-32"] second : t
  val [@warning "-32"] minute : t
  val [@warning "-32"] hour : t

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
  type test_result = {ms_per_run: float; allocs_per_run: int}

  val run : (unit -> unit) -> num_iterations:int -> test_result
end = struct
  type t = {
    mutable start: Time.t;
    mutable n: int; (* current iteration count *)
    mutable total_duration: Time.t;
    bench_func: unit -> unit;
    mutable timer_on: bool;
    mutable start_allocs: float;
    mutable total_allocs: float;
  }

  type test_result = {ms_per_run: float; allocs_per_run: int}

  let make f =
    {
      start = Time.zero;
      n = 0;
      bench_func = f;
      total_duration = Time.zero;
      timer_on = false;
      start_allocs = 0.;
      total_allocs = 0.;
    }

  (* total amount of memory allocated by the program since it started in words *)
  let mallocs () =
    let stats = Gc.quick_stat () in
    stats.minor_words +. stats.major_words -. stats.promoted_words

  let start_timer b =
    if not b.timer_on then (
      let allocated_words = mallocs () in
      b.start_allocs <- allocated_words;
      b.start <- Time.now ();
      b.timer_on <- true)

  let stop_timer b =
    if b.timer_on then (
      let allocated_words = mallocs () in
      let diff = Time.diff b.start (Time.now ()) in
      b.total_duration <- Time.add b.total_duration diff;
      b.total_allocs <- b.total_allocs +. (allocated_words -. b.start_allocs);
      b.timer_on <- false)

  let reset_timer b =
    if b.timer_on then (
      let allocated_words = mallocs () in
      b.start_allocs <- allocated_words;
      b.start <- Time.now ())

  let run_iteration b n =
    Gc.full_major ();
    b.n <- n;
    reset_timer b;
    start_timer b;
    b.bench_func ();
    stop_timer b

  let run f ~num_iterations =
    let b = make f in
    for n = 1 to num_iterations do
      run_iteration b n
    done;
    {
      ms_per_run = Time.print b.total_duration /. float_of_int b.n;
      allocs_per_run = int_of_float (b.total_allocs /. float_of_int b.n);
    }
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
    if p.diagnostics != [] then (
      Res_diagnostics.print_report p.diagnostics src;
      assert false);
    structure

  let data_dir = "tests/syntax_benchmarks/data"
  let num_iterations = 150

  let benchmark (filename, action) =
    let path = Filename.concat data_dir filename in
    let src = IO.read_file path in
    let benchmark_fn =
      match action with
      | Parse ->
        fun () ->
          let _ = Sys.opaque_identity (parse_rescript src path) in
          ()
      | Print ->
        let p = Parser.make src path in
        let ast = ResParser.parse_implementation p in
        fun () ->
          let _ =
            Sys.opaque_identity
              (let cmt_tbl = CommentTable.make () in
               let comments = List.rev p.Parser.comments in
               let () = CommentTable.walk_structure ast cmt_tbl comments in
               Doc.to_string ~width:80 (Printer.print_structure ast cmt_tbl))
          in
          ()
    in
    Benchmark.run benchmark_fn ~num_iterations

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
    |> Seq.flat_map (fun spec ->
           let filename, action = spec in
           let test_name = string_of_action action ^ " " ^ filename in
           let {Benchmark.ms_per_run; allocs_per_run} = benchmark spec in
           [
             `Assoc
               [
                 ("name", `String (Format.sprintf "%s - time/run" test_name));
                 ("unit", `String "ms");
                 ("value", `Float ms_per_run);
               ];
             `Assoc
               [
                 ("name", `String (Format.sprintf "%s - allocs/run" test_name));
                 ("unit", `String "words");
                 ("value", `Int allocs_per_run);
               ];
           ]
           |> List.to_seq)
    |> Seq.iteri (fun i json ->
           print_endline (if i == 0 then "[" else ",");
           print_string (Yojson.to_string json));
    print_newline ();
    print_endline "]"
end

let () = Benchmarks.run ()
