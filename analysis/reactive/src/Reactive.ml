(** Reactive V2: Accumulate-then-propagate scheduler for glitch-free semantics.

    Key design:
    1. Nodes accumulate batch deltas (don't process immediately)
    2. Scheduler visits nodes in dependency order
    3. Each node processes accumulated deltas exactly once per wave

    This eliminates glitches from multi-level dependencies. *)

(** {1 Waves} *)

type ('k, 'v) wave = ('k, 'v Maybe.t) StableWave.t

let create_wave () = StableWave.create ()

(** {1 Statistics} *)

type stats = {
  (* Input tracking *)
  mutable deltas_received: int;  (** Number of delta messages (Batch) *)
  mutable entries_received: int;  (** Total entries after expanding batches *)
  mutable adds_received: int;  (** Set operations received from upstream *)
  mutable removes_received: int;
      (** Remove operations received from upstream *)
  (* Processing tracking *)
  mutable process_count: int;  (** Times process() was called *)
  mutable process_time_ns: int64;  (** Total time in process() *)
  (* Output tracking *)
  mutable deltas_emitted: int;  (** Number of delta messages emitted *)
  mutable entries_emitted: int;  (** Total entries in emitted deltas *)
  mutable adds_emitted: int;  (** Set operations emitted downstream *)
  mutable removes_emitted: int;  (** Remove operations emitted downstream *)
}

let create_stats () =
  {
    deltas_received = 0;
    entries_received = 0;
    adds_received = 0;
    removes_received = 0;
    process_count = 0;
    process_time_ns = 0L;
    deltas_emitted = 0;
    entries_emitted = 0;
    adds_emitted = 0;
    removes_emitted = 0;
  }

(** {1 Debug} *)

let debug_enabled = ref false
let set_debug b = debug_enabled := b

(** {1 Node Registry} *)

module Registry = struct
  type node_info = {
    name: string;
    level: int;
    mutable upstream: string list;
    mutable downstream: string list;
    mutable dirty: bool;
    mutable outbound_inflight: int;
    process: unit -> unit; (* Process accumulated deltas *)
    destroy: unit -> unit;
    stats: stats;
  }

  let nodes : (string, node_info) Hashtbl.t = Hashtbl.create 64
  let edges : (string * string, string) Hashtbl.t = Hashtbl.create 128

  (* Combinator nodes: (combinator_id, (shape, inputs, output)) *)
  let combinators : (string, string * string list * string) Hashtbl.t =
    Hashtbl.create 32

  (* Dirty-node tracking: count + per-node flag (no list, no Hashtbl) *)
  let dirty_count = ref 0

  (* Pre-sorted node array for zero-alloc propagation.
     Built lazily on first propagate; invalidated by register_node. *)
  let sorted_nodes : node_info array ref = ref [||]
  let sorted_valid = ref true

  let register_node ~name ~level ~process ~destroy ~stats =
    let info =
      {
        name;
        level;
        upstream = [];
        downstream = [];
        dirty = false;
        outbound_inflight = 0;
        process;
        destroy;
        stats;
      }
    in
    Hashtbl.replace nodes name info;
    sorted_valid := false;
    info

  let add_edge ~from_name ~to_name ~label =
    Hashtbl.replace edges (from_name, to_name) label;
    (match Hashtbl.find_opt nodes from_name with
    | Some info -> info.downstream <- to_name :: info.downstream
    | None -> ());
    match Hashtbl.find_opt nodes to_name with
    | Some info -> info.upstream <- from_name :: info.upstream
    | None -> ()

  (** Register a multi-input combinator (rendered as diamond in Mermaid) *)
  let add_combinator ~name ~shape ~inputs ~output =
    Hashtbl.replace combinators name (shape, inputs, output)

  (** Zero-alloc mark_dirty: sets flag + increments counter.
      Takes node_info directly — no Hashtbl lookup, no cons cell. *)
  let mark_dirty_node (info : node_info) =
    if not info.dirty then (
      info.dirty <- true;
      incr dirty_count)

  (** Zero-alloc inflight tracking on node_info directly. *)
  let inc_inflight_node (info : node_info) =
    info.outbound_inflight <- info.outbound_inflight + 1

  let dec_inflight_node (info : node_info) count =
    if count > 0 then (
      let n = info.outbound_inflight in
      if count > n then
        failwith
          (Printf.sprintf
             "Reactive inflight underflow on node %s (count=%d, inflight=%d)"
             info.name count n);
      info.outbound_inflight <- n - count)

  let ensure_sorted () =
    if not !sorted_valid then (
      let all = Hashtbl.fold (fun _ info acc -> info :: acc) nodes [] in
      let sorted = List.sort (fun a b -> compare a.level b.level) all in
      sorted_nodes := Array.of_list sorted;
      sorted_valid := true)

  let clear () =
    Hashtbl.clear nodes;
    Hashtbl.clear edges;
    Hashtbl.clear combinators;
    dirty_count := 0;
    sorted_nodes := [||];
    sorted_valid := true

  let destroy_graph () =
    Hashtbl.iter (fun _ info -> info.destroy ()) nodes;
    clear ()

  let reset_stats () =
    Hashtbl.iter
      (fun _ info ->
        info.stats.deltas_received <- 0;
        info.stats.entries_received <- 0;
        info.stats.adds_received <- 0;
        info.stats.removes_received <- 0;
        info.stats.process_count <- 0;
        info.stats.process_time_ns <- 0L;
        info.stats.deltas_emitted <- 0;
        info.stats.entries_emitted <- 0;
        info.stats.adds_emitted <- 0;
        info.stats.removes_emitted <- 0)
      nodes

  (** Generate Mermaid diagram of the pipeline *)
  let to_mermaid () =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "graph TD\n";
    (* Collect edges that are part of combinators *)
    let combinator_edges = Hashtbl.create 64 in
    Hashtbl.iter
      (fun comb_name (_, inputs, output) ->
        List.iter
          (fun input ->
            Hashtbl.replace combinator_edges (input, output) comb_name)
          inputs)
      combinators;
    (* Output regular nodes *)
    Hashtbl.iter
      (fun name _info ->
        Buffer.add_string buf (Printf.sprintf "    %s[%s]\n" name name))
      nodes;
    (* Output combinator nodes (diamond shape) with classes *)
    let join_nodes = ref [] in
    let union_nodes = ref [] in
    let fixpoint_nodes = ref [] in
    Hashtbl.iter
      (fun comb_name (shape, _inputs, _output) ->
        Buffer.add_string buf (Printf.sprintf "    %s{%s}\n" comb_name shape);
        match shape with
        | "join" -> join_nodes := comb_name :: !join_nodes
        | "union" -> union_nodes := comb_name :: !union_nodes
        | "fixpoint" -> fixpoint_nodes := comb_name :: !fixpoint_nodes
        | _ -> ())
      combinators;
    (* Output edges *)
    Hashtbl.iter
      (fun name info ->
        List.iter
          (fun downstream ->
            (* Check if this edge is part of a combinator *)
            match Hashtbl.find_opt combinator_edges (name, downstream) with
            | Some comb_name ->
              (* Edge goes to combinator node instead *)
              Buffer.add_string buf
                (Printf.sprintf "    %s --> %s\n" name comb_name)
            | None ->
              let label =
                match Hashtbl.find_opt edges (name, downstream) with
                | Some l -> l
                | None -> ""
              in
              if label = "" then
                Buffer.add_string buf
                  (Printf.sprintf "    %s --> %s\n" name downstream)
              else
                Buffer.add_string buf
                  (Printf.sprintf "    %s -->|%s| %s\n" name label downstream))
          info.downstream)
      nodes;
    (* Output edges from combinators to their outputs *)
    Hashtbl.iter
      (fun comb_name (_shape, _inputs, output) ->
        Buffer.add_string buf
          (Printf.sprintf "    %s --> %s\n" comb_name output))
      combinators;
    (* Style definitions for combinator types *)
    Buffer.add_string buf
      "\n    classDef joinClass fill:#e6f3ff,stroke:#0066cc\n";
    Buffer.add_string buf
      "    classDef unionClass fill:#fff0e6,stroke:#cc6600\n";
    Buffer.add_string buf
      "    classDef fixpointClass fill:#e6ffe6,stroke:#006600\n";
    (* Assign classes to combinator nodes *)
    if !join_nodes <> [] then
      Buffer.add_string buf
        (Printf.sprintf "    class %s joinClass\n"
           (String.concat "," !join_nodes));
    if !union_nodes <> [] then
      Buffer.add_string buf
        (Printf.sprintf "    class %s unionClass\n"
           (String.concat "," !union_nodes));
    if !fixpoint_nodes <> [] then
      Buffer.add_string buf
        (Printf.sprintf "    class %s fixpointClass\n"
           (String.concat "," !fixpoint_nodes));
    Buffer.contents buf

  (** Print timing stats for all nodes *)
  let print_stats () =
    let all = Hashtbl.fold (fun _ info acc -> info :: acc) nodes [] in
    let sorted = List.sort (fun a b -> compare a.level b.level) all in
    let by_time =
      List.sort
        (fun a b ->
          Int64.compare b.stats.process_time_ns a.stats.process_time_ns)
        all
    in
    let top =
      by_time
      |> List.filter (fun info -> info.stats.process_time_ns <> 0L)
      |> List.filteri (fun i _ -> i < 5)
    in
    if top <> [] then (
      Printf.eprintf "Top nodes by process time:\n";
      List.iter
        (fun info ->
          let time_ms = Int64.to_float info.stats.process_time_ns /. 1e6 in
          Printf.eprintf "  - %s (L%d): %.2fms (runs=%d)\n" info.name info.level
            time_ms info.stats.process_count)
        top;
      Printf.eprintf "\n");
    Printf.eprintf "Node statistics:\n";
    Printf.eprintf "  %-30s | %8s %8s %5s %5s | %8s %8s %5s %5s | %5s %8s\n"
      "name" "d_recv" "e_recv" "+in" "-in" "d_emit" "e_emit" "+out" "-out"
      "runs" "time_ms";
    Printf.eprintf "  %s\n" (String.make 115 '-');
    List.iter
      (fun info ->
        let s = info.stats in
        let time_ms = Int64.to_float s.process_time_ns /. 1e6 in
        Printf.eprintf
          "  %-30s | %8d %8d %5d %5d | %8d %8d %5d %5d | %5d %8.2f\n"
          (Printf.sprintf "%s (L%d)" info.name info.level)
          s.deltas_received s.entries_received s.adds_received
          s.removes_received s.deltas_emitted s.entries_emitted s.adds_emitted
          s.removes_emitted s.process_count time_ms)
      sorted
end

(** {1 Scheduler} *)

module Scheduler = struct
  let propagating = ref false
  let wave_counter = ref 0

  let is_propagating () = !propagating

  type stats_snapshot = {
    deltas_received: int;
    entries_received: int;
    adds_received: int;
    removes_received: int;
    deltas_emitted: int;
    entries_emitted: int;
    adds_emitted: int;
    removes_emitted: int;
    process_count: int;
    process_time_ns: int64;
  }

  let snapshot_stats (s : stats) : stats_snapshot =
    {
      deltas_received = s.deltas_received;
      entries_received = s.entries_received;
      adds_received = s.adds_received;
      removes_received = s.removes_received;
      deltas_emitted = s.deltas_emitted;
      entries_emitted = s.entries_emitted;
      adds_emitted = s.adds_emitted;
      removes_emitted = s.removes_emitted;
      process_count = s.process_count;
      process_time_ns = s.process_time_ns;
    }

  let diff_stats (before : stats_snapshot) (after_ : stats) =
    let d_int x y = x - y in
    let d_time x y = Int64.sub x y in
    ( d_int after_.deltas_received before.deltas_received,
      d_int after_.entries_received before.entries_received,
      d_int after_.adds_received before.adds_received,
      d_int after_.removes_received before.removes_received,
      d_int after_.deltas_emitted before.deltas_emitted,
      d_int after_.entries_emitted before.entries_emitted,
      d_int after_.adds_emitted before.adds_emitted,
      d_int after_.removes_emitted before.removes_emitted,
      d_int after_.process_count before.process_count,
      d_time after_.process_time_ns before.process_time_ns )

  (** Process all dirty nodes in level order.
      Uses pre-sorted node array — zero allocation at steady state. *)
  let propagate () =
    if !propagating then
      failwith "Scheduler.propagate: already propagating (nested call)"
    else (
      propagating := true;
      incr wave_counter;
      Registry.ensure_sorted ();
      let nodes = !Registry.sorted_nodes in
      let len = Array.length nodes in

      if !debug_enabled then (
        let wave_id = !wave_counter in
        let wave_start = Unix.gettimeofday () in
        let processed_nodes = ref 0 in
        Printf.eprintf "\n=== Reactive wave %d ===\n%!" wave_id;

        while !Registry.dirty_count > 0 do
          let made_progress = ref false in
          for i = 0 to len - 1 do
            let info = nodes.(i) in
            if info.Registry.dirty && info.Registry.outbound_inflight = 0 then (
              info.Registry.dirty <- false;
              decr Registry.dirty_count;
              made_progress := true;
              incr processed_nodes;
              let before = snapshot_stats info.Registry.stats in
              let start = Sys.time () in
              info.Registry.process ();
              let elapsed = Sys.time () -. start in
              info.Registry.stats.process_time_ns <-
                Int64.add info.Registry.stats.process_time_ns
                  (Int64.of_float (elapsed *. 1e9));
              info.Registry.stats.process_count <-
                info.Registry.stats.process_count + 1;
              let ( d_recv,
                    e_recv,
                    add_in,
                    rem_in,
                    d_emit,
                    e_emit,
                    add_out,
                    rem_out,
                    runs,
                    dt_ns ) =
                diff_stats before info.Registry.stats
              in
              if runs <> 0 then
                Printf.eprintf
                  "  %-30s (L%d): recv d/e/+/-=%d/%d/%d/%d emit \
                   d/e/+/-=%d/%d/%d/%d time=%.2fms\n\
                   %!"
                  info.Registry.name info.Registry.level d_recv e_recv add_in
                  rem_in d_emit e_emit add_out rem_out
                  (Int64.to_float dt_ns /. 1e6))
          done;
          if (not !made_progress) && !Registry.dirty_count > 0 then
            failwith
              "Scheduler invariant violation: no runnable dirty node under \
               inflight gate"
        done;

        let wave_elapsed_ms = (Unix.gettimeofday () -. wave_start) *. 1000.0 in
        Printf.eprintf "Wave %d: processed_nodes=%d wall=%.2fms\n%!" wave_id
          !processed_nodes wave_elapsed_ms)
      else
        (* Hot path: no debug output, no gettimeofday, no timing *)
        while !Registry.dirty_count > 0 do
          let made_progress = ref false in
          for i = 0 to len - 1 do
            let info = nodes.(i) in
            if info.Registry.dirty && info.Registry.outbound_inflight = 0 then (
              info.Registry.dirty <- false;
              decr Registry.dirty_count;
              made_progress := true;
              info.Registry.process ();
              info.Registry.stats.process_count <-
                info.Registry.stats.process_count + 1)
          done;
          if (not !made_progress) && !Registry.dirty_count > 0 then
            failwith
              "Scheduler invariant violation: no runnable dirty node under \
               inflight gate"
        done;

      propagating := false)

  let wave_count () = !wave_counter
  let reset_wave_count () = wave_counter := 0
end

(** {1 Subscriber notification — zero-alloc} *)

(** Notify subscribers without allocating a closure.
    [List.iter (fun h -> h wave) subs] allocates 4 words for the
    closure capturing [wave]. This hand-unrolled version avoids that. *)
let rec notify_subscribers wave = function
  | [] -> ()
  | [h] -> h wave
  | [h1; h2] ->
    h1 wave;
    h2 wave
  | h :: rest ->
    h wave;
    notify_subscribers wave rest

(** {1 Collection Interface} *)

type ('k, 'v) t = {
  name: string;
  subscribe: (('k, 'v) wave -> unit) -> unit;
  iter: ('k Stable.t -> 'v Stable.t -> unit) -> unit;
  get: 'k Stable.t -> 'v Stable.t Maybe.t;
  length: unit -> int;
  destroy: unit -> unit;
  stats: stats;
  level: int;
  node: Registry.node_info;
}

let iter f t = t.iter f
let get t k = t.get k
let length t = t.length ()
let destroy t = t.destroy ()
let stats t = t.stats
let level t = t.level
let name t = t.name

(** {1 Source Collection} *)

module Source = struct
  type ('k, 'v) tables = {
    tbl: ('k, 'v) StableMap.t;
    pending: ('k, 'v Maybe.t) StableMap.t;
  }

  let apply_emit (tables : ('k, 'v) tables) k mv =
    let mv = Maybe.of_stable mv in
    if Maybe.is_some mv then (
      StableMap.replace tables.tbl k (Maybe.unsafe_get mv);
      StableMap.replace tables.pending k (Maybe.to_stable mv))
    else (
      StableMap.remove tables.tbl k;
      StableMap.replace tables.pending k (Maybe.to_stable mv))

  let create ~name () =
    let tbl : ('k, 'v) StableMap.t = StableMap.create () in
    let subscribers = ref [] in
    let my_stats = create_stats () in
    let output_wave = create_wave () in
    (* Pending deltas: accumulated by emit, flushed by process.
     Uses StableMap for zero-alloc deduplication (last-write-wins). *)
    let pending : ('k, 'v Maybe.t) StableMap.t = StableMap.create () in
    let tables = {tbl; pending} in
    let pending_count = ref 0 in

    let process () =
      let count = StableMap.cardinal pending in
      if count > 0 then (
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <- my_stats.entries_emitted + count;
        StableWave.clear output_wave;
        StableMap.iter_with
          (fun wave k v -> StableWave.push wave k v)
          output_wave pending;
        StableMap.clear pending;
        notify_subscribers output_wave !subscribers)
      else StableMap.clear pending
    in

    let destroy () =
      StableMap.destroy tbl;
      StableMap.destroy pending;
      StableWave.destroy output_wave
    in
    let my_info =
      Registry.register_node ~name ~level:0 ~process ~destroy ~stats:my_stats
    in

    let collection =
      {
        name;
        subscribe = (fun h -> subscribers := h :: !subscribers);
        iter = (fun f -> StableMap.iter f tbl);
        get = (fun k -> StableMap.find_maybe tbl k);
        length = (fun () -> StableMap.cardinal tbl);
        destroy;
        stats = my_stats;
        level = 0;
        node = my_info;
      }
    in

    let emit (input_wave : ('k, 'v Maybe.t) StableWave.t) =
      let count = StableWave.count input_wave in
      my_stats.deltas_received <- my_stats.deltas_received + 1;
      my_stats.entries_received <- my_stats.entries_received + count;
      (* Apply to internal state and accumulate into pending map *)
      StableWave.iter_with input_wave apply_emit tables;
      pending_count := !pending_count + 1;
      Registry.mark_dirty_node my_info;
      if not (Scheduler.is_propagating ()) then Scheduler.propagate ()
    in

    (collection, emit)
end

(** {1 FlatMap} *)

module FlatMap = struct
  let create ~name (src : ('k1, 'v1) t) ~f ?merge () : ('k2, 'v2) t =
    let my_level = src.level + 1 in
    let merge_fn =
      match merge with
      | Some m -> m
      | None -> fun _ v -> v
    in

    let subscribers = ref [] in
    let my_stats = create_stats () in
    let state = ReactiveFlatMap.create ~f ~merge:merge_fn in
    let pending_count = ref 0 in

    let process () =
      let consumed = !pending_count in
      pending_count := 0;

      my_stats.deltas_received <- my_stats.deltas_received + consumed;

      Registry.dec_inflight_node src.node consumed;

      let r = ReactiveFlatMap.process state in
      my_stats.entries_received <-
        my_stats.entries_received + r.entries_received;
      my_stats.adds_received <- my_stats.adds_received + r.adds_received;
      my_stats.removes_received <-
        my_stats.removes_received + r.removes_received;

      if r.entries_emitted > 0 then (
        let output_wave = ReactiveFlatMap.output_wave state in
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <- my_stats.entries_emitted + r.entries_emitted;
        my_stats.adds_emitted <- my_stats.adds_emitted + r.adds_emitted;
        my_stats.removes_emitted <- my_stats.removes_emitted + r.removes_emitted;
        notify_subscribers output_wave !subscribers)
    in

    let destroy () = ReactiveFlatMap.destroy state in
    let my_info =
      Registry.register_node ~name ~level:my_level ~process ~destroy
        ~stats:my_stats
    in
    Registry.add_edge ~from_name:src.name ~to_name:name ~label:"flatMap";

    (* Subscribe to source: push directly into pending map *)
    src.subscribe (fun wave ->
        Registry.inc_inflight_node src.node;
        incr pending_count;
        StableWave.iter_with wave ReactiveFlatMap.push state;
        Registry.mark_dirty_node my_info);

    (* Initialize from existing data *)
    src.iter (fun k v -> ReactiveFlatMap.init_entry state k v);

    {
      name;
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> ReactiveFlatMap.iter_target f state);
      get = (fun k -> ReactiveFlatMap.find_target state k);
      length = (fun () -> ReactiveFlatMap.target_length state);
      destroy;
      stats = my_stats;
      level = my_level;
      node = my_info;
    }
end

(** {1 Join} *)

module Join = struct
  let create ~name (left : ('k1, 'v1) t) (right : ('k2, 'v2) t) ~key_of ~f
      ?merge () : ('k3, 'v3) t =
    let my_level = max left.level right.level + 1 in
    let merge_fn =
      match merge with
      | Some m -> m
      | None -> fun _ v -> v
    in

    let subscribers = ref [] in
    let my_stats = create_stats () in
    let state =
      ReactiveJoin.create ~key_of ~f ~merge:merge_fn ~right_get:right.get
    in
    let left_pending_count = ref 0 in
    let right_pending_count = ref 0 in

    let process () =
      let consumed_left = !left_pending_count in
      let consumed_right = !right_pending_count in
      left_pending_count := 0;
      right_pending_count := 0;

      my_stats.deltas_received <-
        my_stats.deltas_received + consumed_left + consumed_right;

      Registry.dec_inflight_node left.node consumed_left;
      Registry.dec_inflight_node right.node consumed_right;

      let r = ReactiveJoin.process state in
      my_stats.entries_received <-
        my_stats.entries_received + r.entries_received;
      my_stats.adds_received <- my_stats.adds_received + r.adds_received;
      my_stats.removes_received <-
        my_stats.removes_received + r.removes_received;

      if r.entries_emitted > 0 then (
        let output_wave = ReactiveJoin.output_wave state in
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <- my_stats.entries_emitted + r.entries_emitted;
        my_stats.adds_emitted <- my_stats.adds_emitted + r.adds_emitted;
        my_stats.removes_emitted <- my_stats.removes_emitted + r.removes_emitted;
        notify_subscribers output_wave !subscribers)
    in

    let destroy () = ReactiveJoin.destroy state in
    let my_info =
      Registry.register_node ~name ~level:my_level ~process ~destroy
        ~stats:my_stats
    in
    Registry.add_edge ~from_name:left.name ~to_name:name ~label:"join";
    Registry.add_edge ~from_name:right.name ~to_name:name ~label:"join";
    Registry.add_combinator ~name:(name ^ "_join") ~shape:"join"
      ~inputs:[left.name; right.name] ~output:name;

    (* Subscribe to sources: push directly into pending maps *)
    left.subscribe (fun wave ->
        Registry.inc_inflight_node left.node;
        incr left_pending_count;
        StableWave.iter_with wave ReactiveJoin.push_left state;
        Registry.mark_dirty_node my_info);

    right.subscribe (fun wave ->
        Registry.inc_inflight_node right.node;
        incr right_pending_count;
        StableWave.iter_with wave ReactiveJoin.push_right state;
        Registry.mark_dirty_node my_info);

    (* Initialize from existing data *)
    left.iter (fun k1 v1 -> ReactiveJoin.init_entry state k1 v1);

    {
      name;
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> ReactiveJoin.iter_target f state);
      get = (fun k -> ReactiveJoin.find_target state k);
      length = (fun () -> ReactiveJoin.target_length state);
      destroy;
      stats = my_stats;
      level = my_level;
      node = my_info;
    }
end

(** {1 Union} *)

module Union = struct
  let create ~name (left : ('k, 'v) t) (right : ('k, 'v) t) ?merge () :
      ('k, 'v) t =
    let my_level = max left.level right.level + 1 in
    let merge_fn =
      match merge with
      | Some m -> m
      | None -> fun _ v -> v
    in

    let subscribers = ref [] in
    let my_stats = create_stats () in
    let state = ReactiveUnion.create ~merge:merge_fn in
    let left_pending_count = ref 0 in
    let right_pending_count = ref 0 in

    let process () =
      let consumed_left = !left_pending_count in
      let consumed_right = !right_pending_count in
      left_pending_count := 0;
      right_pending_count := 0;

      my_stats.deltas_received <-
        my_stats.deltas_received + consumed_left + consumed_right;

      Registry.dec_inflight_node left.node consumed_left;
      Registry.dec_inflight_node right.node consumed_right;

      let r = ReactiveUnion.process state in
      my_stats.entries_received <-
        my_stats.entries_received + r.entries_received;
      my_stats.adds_received <- my_stats.adds_received + r.adds_received;
      my_stats.removes_received <-
        my_stats.removes_received + r.removes_received;

      if r.entries_emitted > 0 then (
        let output_wave = ReactiveUnion.output_wave state in
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <- my_stats.entries_emitted + r.entries_emitted;
        my_stats.adds_emitted <- my_stats.adds_emitted + r.adds_emitted;
        my_stats.removes_emitted <- my_stats.removes_emitted + r.removes_emitted;
        notify_subscribers output_wave !subscribers)
    in

    let destroy () = ReactiveUnion.destroy state in
    let my_info =
      Registry.register_node ~name ~level:my_level ~process ~destroy
        ~stats:my_stats
    in
    Registry.add_edge ~from_name:left.name ~to_name:name ~label:"union";
    Registry.add_edge ~from_name:right.name ~to_name:name ~label:"union";
    Registry.add_combinator ~name:(name ^ "_union") ~shape:"union"
      ~inputs:[left.name; right.name] ~output:name;

    (* Subscribe to sources: push directly into pending maps *)
    left.subscribe (fun wave ->
        Registry.inc_inflight_node left.node;
        incr left_pending_count;
        StableWave.iter_with wave ReactiveUnion.push_left state;
        Registry.mark_dirty_node my_info);

    right.subscribe (fun wave ->
        Registry.inc_inflight_node right.node;
        incr right_pending_count;
        StableWave.iter_with wave ReactiveUnion.push_right state;
        Registry.mark_dirty_node my_info);

    (* Initialize from existing data - process left then right *)
    left.iter (fun k v -> ReactiveUnion.init_left state k v);
    right.iter (fun k v -> ReactiveUnion.init_right state k v);

    {
      name;
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> ReactiveUnion.iter_target f state);
      get = (fun k -> ReactiveUnion.find_target state k);
      length = (fun () -> ReactiveUnion.target_length state);
      destroy;
      stats = my_stats;
      level = my_level;
      node = my_info;
    }
end

(** {1 Fixpoint} *)

module Fixpoint = struct
  let stable_wave_map_replace pending k v = StableMap.replace pending k v

  let stable_wave_push wave k v = StableWave.push wave k v

  let create ~name ~(init : ('k, unit) t) ~(edges : ('k, 'k StableList.inner) t)
      () : ('k, unit) t =
    let my_level = max init.level edges.level + 1 in
    let int_env_or name default =
      match Sys.getenv_opt name with
      | None -> default
      | Some s -> (
        match int_of_string_opt s with
        | Some n when n > 0 -> n
        | _ -> default)
    in

    (* Internal state *)
    let max_nodes = int_env_or "RESCRIPT_REACTIVE_FIXPOINT_MAX_NODES" 100_000 in
    let max_edges =
      int_env_or "RESCRIPT_REACTIVE_FIXPOINT_MAX_EDGES" 1_000_000
    in
    let max_root_wave_entries =
      int_env_or "RESCRIPT_REACTIVE_FIXPOINT_MAX_ROOT_WAVE_ENTRIES" 4_096
    in
    let max_edge_wave_entries =
      int_env_or "RESCRIPT_REACTIVE_FIXPOINT_MAX_EDGE_WAVE_ENTRIES" 16_384
    in
    let state = ReactiveFixpoint.create ~max_nodes ~max_edges in
    let root_wave = StableWave.create ~max_entries:max_root_wave_entries () in
    let edge_wave = StableWave.create ~max_entries:max_edge_wave_entries () in
    let subscribers = ref [] in
    let my_stats = create_stats () in
    let root_pending : ('k, unit Maybe.t) StableMap.t = StableMap.create () in
    let edge_pending : ('k, 'k StableList.inner Maybe.t) StableMap.t =
      StableMap.create ()
    in
    let init_pending_count = ref 0 in
    let edges_pending_count = ref 0 in

    let process () =
      let consumed_init = !init_pending_count in
      let consumed_edges = !edges_pending_count in
      init_pending_count := 0;
      edges_pending_count := 0;

      my_stats.deltas_received <-
        my_stats.deltas_received + consumed_init + consumed_edges;
      Registry.dec_inflight_node init.node consumed_init;
      Registry.dec_inflight_node edges.node consumed_edges;

      (* Dump pending maps into waves *)
      StableWave.clear root_wave;
      StableWave.clear edge_wave;
      let root_entries = StableMap.cardinal root_pending in
      let edge_entries = StableMap.cardinal edge_pending in
      StableMap.iter_with stable_wave_push root_wave root_pending;
      StableMap.iter_with stable_wave_push edge_wave edge_pending;
      StableMap.clear root_pending;
      StableMap.clear edge_pending;

      my_stats.entries_received <-
        my_stats.entries_received + root_entries + edge_entries;
      my_stats.adds_received <-
        my_stats.adds_received + root_entries + edge_entries;

      ReactiveFixpoint.apply_wave state ~roots:root_wave ~edges:edge_wave;
      let out_wave = ReactiveFixpoint.output_wave state in
      let out_count = StableWave.count out_wave in
      if out_count > 0 then (
        notify_subscribers out_wave !subscribers;
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <- my_stats.entries_emitted + out_count)
    in

    let destroy () =
      StableMap.destroy root_pending;
      StableMap.destroy edge_pending;
      StableWave.destroy root_wave;
      StableWave.destroy edge_wave;
      ReactiveFixpoint.destroy state
    in
    let my_info =
      Registry.register_node ~name ~level:my_level ~process ~destroy
        ~stats:my_stats
    in
    Registry.add_edge ~from_name:init.name ~to_name:name ~label:"roots";
    Registry.add_edge ~from_name:edges.name ~to_name:name ~label:"edges";
    Registry.add_combinator ~name:(name ^ "_fp") ~shape:"fixpoint"
      ~inputs:[init.name; edges.name] ~output:name;

    (* Subscribe to sources: push directly into pending maps *)
    init.subscribe (fun wave ->
        Registry.inc_inflight_node init.node;
        init_pending_count := !init_pending_count + 1;
        StableWave.iter_with wave stable_wave_map_replace root_pending;
        Registry.mark_dirty_node my_info);

    edges.subscribe (fun wave ->
        Registry.inc_inflight_node edges.node;
        edges_pending_count := !edges_pending_count + 1;
        StableWave.iter_with wave stable_wave_map_replace edge_pending;
        Registry.mark_dirty_node my_info);

    (* Initialize from existing data *)
    let init_roots_wave =
      StableWave.create ~max_entries:(max 1 (init.length ())) ()
    in
    let init_edges_wave : ('k, 'k StableList.inner) StableWave.t =
      StableWave.create ~max_entries:(max 1 (edges.length ())) ()
    in
    StableWave.clear init_roots_wave;
    StableWave.clear init_edges_wave;
    init.iter (fun k _unit -> StableWave.push init_roots_wave k Stable.unit);
    edges.iter (fun k succs -> StableWave.push init_edges_wave k succs);
    ReactiveFixpoint.initialize state ~roots:init_roots_wave
      ~edges:init_edges_wave;
    StableWave.destroy init_roots_wave;
    StableWave.destroy init_edges_wave;

    {
      name;
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> ReactiveFixpoint.iter_current state f);
      get = (fun k -> ReactiveFixpoint.get_current state k);
      length = (fun () -> ReactiveFixpoint.current_length state);
      destroy;
      stats = my_stats;
      level = my_level;
      node = my_info;
    }
end

(** {1 Utilities} *)

let to_mermaid () = Registry.to_mermaid ()
let print_stats () = Registry.print_stats ()
let set_debug = set_debug
let destroy_graph () = Registry.destroy_graph ()
let reset () = Registry.clear ()
let reset_stats () = Registry.reset_stats ()
