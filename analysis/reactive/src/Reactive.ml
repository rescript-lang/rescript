(** Reactive V2: Accumulate-then-propagate scheduler for glitch-free semantics.
    
    Key design:
    1. Nodes accumulate batch deltas (don't process immediately)
    2. Scheduler visits nodes in dependency order
    3. Each node processes accumulated deltas exactly once per wave
    
    This eliminates glitches from multi-level dependencies. *)

(** {1 Deltas} *)

type ('k, 'v) delta =
  | Set of 'k * 'v
  | Remove of 'k
  | Batch of ('k * 'v option) list

let set k v = (k, Some v)
let remove k = (k, None)

let delta_to_entries = function
  | Set (k, v) -> [(k, Some v)]
  | Remove k -> [(k, None)]
  | Batch entries -> entries

let merge_entries entries =
  (* Deduplicate: later entries win *)
  let tbl = Hashtbl.create (List.length entries) in
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) entries;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

let count_adds_removes entries =
  List.fold_left
    (fun (adds, removes) (_, v) ->
      match v with
      | Some _ -> (adds + 1, removes)
      | None -> (adds, removes + 1))
    (0, 0) entries

(** {1 Statistics} *)

type stats = {
  (* Input tracking *)
  mutable deltas_received: int;
      (** Number of delta messages (Set/Remove/Batch) *)
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

(** Count adds and removes in a list of entries *)
let count_changes entries =
  let adds = ref 0 in
  let removes = ref 0 in
  List.iter
    (fun (_, v_opt) ->
      match v_opt with
      | Some _ -> incr adds
      | None -> incr removes)
    entries;
  (!adds, !removes)

(** {1 Node Registry} *)

module Registry = struct
  type node_info = {
    name: string;
    level: int;
    mutable upstream: string list;
    mutable downstream: string list;
    mutable dirty: bool;
    process: unit -> unit; (* Process accumulated deltas *)
    stats: stats;
  }

  let nodes : (string, node_info) Hashtbl.t = Hashtbl.create 64
  let edges : (string * string, string) Hashtbl.t = Hashtbl.create 128

  (* Combinator nodes: (combinator_id, (shape, inputs, output)) *)
  let combinators : (string, string * string list * string) Hashtbl.t =
    Hashtbl.create 32
  let dirty_nodes : string list ref = ref []

  let register ~name ~level ~process ~stats =
    let info =
      {
        name;
        level;
        upstream = [];
        downstream = [];
        dirty = false;
        process;
        stats;
      }
    in
    Hashtbl.replace nodes name info;
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

  let mark_dirty name =
    match Hashtbl.find_opt nodes name with
    | Some info when not info.dirty ->
      info.dirty <- true;
      dirty_nodes := name :: !dirty_nodes
    | _ -> ()

  let clear () =
    Hashtbl.clear nodes;
    Hashtbl.clear edges;
    Hashtbl.clear combinators;
    dirty_nodes := []

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

  (** Process all dirty nodes in level order *)
  let propagate () =
    if !propagating then
      failwith "Scheduler.propagate: already propagating (nested call)"
    else (
      propagating := true;
      incr wave_counter;

      while !Registry.dirty_nodes <> [] do
        (* Get all dirty nodes, sort by level *)
        let dirty = !Registry.dirty_nodes in
        Registry.dirty_nodes := [];

        let nodes_with_levels =
          dirty
          |> List.filter_map (fun name ->
                 match Hashtbl.find_opt Registry.nodes name with
                 | Some info -> Some (info.Registry.level, name, info)
                 | None -> None)
        in

        let sorted =
          List.sort
            (fun (l1, _, _) (l2, _, _) -> compare l1 l2)
            nodes_with_levels
        in

        (* Find minimum level *)
        match sorted with
        | [] -> ()
        | (min_level, _, _) :: _ ->
          (* Process all nodes at minimum level *)
          let at_level, rest =
            List.partition (fun (l, _, _) -> l = min_level) sorted
          in

          (* Put remaining back in dirty list *)
          List.iter
            (fun (_, name, _) ->
              Registry.dirty_nodes := name :: !Registry.dirty_nodes)
            rest;

          (* Process nodes at this level *)
          List.iter
            (fun (_, _, info) ->
              info.Registry.dirty <- false;
              let start = Sys.time () in
              info.Registry.process ();
              let elapsed = Sys.time () -. start in
              info.Registry.stats.process_time_ns <-
                Int64.add info.Registry.stats.process_time_ns
                  (Int64.of_float (elapsed *. 1e9));
              info.Registry.stats.process_count <-
                info.Registry.stats.process_count + 1)
            at_level
      done;

      propagating := false)

  let wave_count () = !wave_counter
  let reset_wave_count () = wave_counter := 0
end

(** {1 Collection Interface} *)

type ('k, 'v) t = {
  name: string;
  subscribe: (('k, 'v) delta -> unit) -> unit;
  iter: ('k -> 'v -> unit) -> unit;
  get: 'k -> 'v option;
  length: unit -> int;
  stats: stats;
  level: int;
}

let iter f t = t.iter f
let get t k = t.get k
let length t = t.length ()
let stats t = t.stats
let level t = t.level
let name t = t.name

(** {1 Source Collection} *)

let source ~name () =
  let tbl = Hashtbl.create 64 in
  let subscribers = ref [] in
  let my_stats = create_stats () in

  (* Pending deltas to propagate *)
  let pending = ref [] in

  let process () =
    if !pending <> [] then (
      let entries =
        !pending |> List.concat_map delta_to_entries |> merge_entries
      in
      pending := [];
      if entries <> [] then (
        let num_adds, num_removes = count_changes entries in
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <-
          my_stats.entries_emitted + List.length entries;
        my_stats.adds_emitted <- my_stats.adds_emitted + num_adds;
        my_stats.removes_emitted <- my_stats.removes_emitted + num_removes;
        let delta = Batch entries in
        List.iter (fun h -> h delta) !subscribers))
  in

  let _info = Registry.register ~name ~level:0 ~process ~stats:my_stats in

  let collection =
    {
      name;
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f tbl);
      get = (fun k -> Hashtbl.find_opt tbl k);
      length = (fun () -> Hashtbl.length tbl);
      stats = my_stats;
      level = 0;
    }
  in

  let emit delta =
    (* Track input *)
    my_stats.deltas_received <- my_stats.deltas_received + 1;
    let entries = delta_to_entries delta in
    my_stats.entries_received <- my_stats.entries_received + List.length entries;
    let num_adds, num_removes = count_adds_removes entries in
    my_stats.adds_received <- my_stats.adds_received + num_adds;
    my_stats.removes_received <- my_stats.removes_received + num_removes;

    (* Apply to internal state immediately *)
    (match delta with
    | Set (k, v) -> Hashtbl.replace tbl k v
    | Remove k -> Hashtbl.remove tbl k
    | Batch entries ->
      List.iter
        (fun (k, v_opt) ->
          match v_opt with
          | Some v -> Hashtbl.replace tbl k v
          | None -> Hashtbl.remove tbl k)
        entries);
    (* Accumulate for propagation *)
    pending := delta :: !pending;
    Registry.mark_dirty name;
    (* If not in propagation, start one *)
    if not (Scheduler.is_propagating ()) then Scheduler.propagate ()
  in

  (collection, emit)

(** {1 FlatMap} *)

let flatMap ~name (src : ('k1, 'v1) t) ~f ?merge () : ('k2, 'v2) t =
  let my_level = src.level + 1 in
  let merge_fn =
    match merge with
    | Some m -> m
    | None -> fun _ v -> v
  in

  (* Internal state *)
  let provenance : ('k1, 'k2 list) Hashtbl.t = Hashtbl.create 64 in
  let contributions : ('k2, ('k1, 'v2) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 256
  in
  let target : ('k2, 'v2) Hashtbl.t = Hashtbl.create 256 in
  let subscribers = ref [] in
  let my_stats = create_stats () in

  (* Pending input deltas *)
  let pending = ref [] in

  let recompute_target k2 =
    match Hashtbl.find_opt contributions k2 with
    | None ->
      Hashtbl.remove target k2;
      Some (k2, None)
    | Some contribs when Hashtbl.length contribs = 0 ->
      Hashtbl.remove contributions k2;
      Hashtbl.remove target k2;
      Some (k2, None)
    | Some contribs ->
      let values = Hashtbl.fold (fun _ v acc -> v :: acc) contribs [] in
      let merged =
        match values with
        | [] -> assert false
        | [v] -> v
        | v :: rest -> List.fold_left merge_fn v rest
      in
      Hashtbl.replace target k2 merged;
      Some (k2, Some merged)
  in

  let remove_source k1 =
    match Hashtbl.find_opt provenance k1 with
    | None -> []
    | Some target_keys ->
      Hashtbl.remove provenance k1;
      List.iter
        (fun k2 ->
          match Hashtbl.find_opt contributions k2 with
          | None -> ()
          | Some contribs -> Hashtbl.remove contribs k1)
        target_keys;
      target_keys
  in

  let add_source k1 entries =
    let target_keys = List.map fst entries in
    Hashtbl.replace provenance k1 target_keys;
    List.iter
      (fun (k2, v2) ->
        let contribs =
          match Hashtbl.find_opt contributions k2 with
          | Some c -> c
          | None ->
            let c = Hashtbl.create 4 in
            Hashtbl.replace contributions k2 c;
            c
        in
        Hashtbl.replace contribs k1 v2)
      entries;
    target_keys
  in

  let process_entry (k1, v1_opt) =
    let old_affected = remove_source k1 in
    let new_affected =
      match v1_opt with
      | None -> []
      | Some v1 ->
        let entries = f k1 v1 in
        add_source k1 entries
    in
    let all_affected = old_affected @ new_affected in
    (* Deduplicate *)
    let seen = Hashtbl.create (List.length all_affected) in
    List.filter_map
      (fun k2 ->
        if Hashtbl.mem seen k2 then None
        else (
          Hashtbl.replace seen k2 ();
          recompute_target k2))
      all_affected
  in

  let process () =
    if !pending <> [] then (
      (* Track input deltas *)
      my_stats.deltas_received <-
        my_stats.deltas_received + List.length !pending;
      let entries =
        !pending |> List.concat_map delta_to_entries |> merge_entries
      in
      pending := [];
      my_stats.entries_received <-
        my_stats.entries_received + List.length entries;
      let in_adds, in_removes = count_adds_removes entries in
      my_stats.adds_received <- my_stats.adds_received + in_adds;
      my_stats.removes_received <- my_stats.removes_received + in_removes;

      let output_entries = entries |> List.concat_map process_entry in
      if output_entries <> [] then (
        let num_adds, num_removes = count_changes output_entries in
        my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
        my_stats.entries_emitted <-
          my_stats.entries_emitted + List.length output_entries;
        my_stats.adds_emitted <- my_stats.adds_emitted + num_adds;
        my_stats.removes_emitted <- my_stats.removes_emitted + num_removes;
        let delta = Batch output_entries in
        List.iter (fun h -> h delta) !subscribers))
  in

  let _info =
    Registry.register ~name ~level:my_level ~process ~stats:my_stats
  in
  Registry.add_edge ~from_name:src.name ~to_name:name ~label:"flatMap";

  (* Subscribe to source: just accumulate *)
  src.subscribe (fun delta ->
      pending := delta :: !pending;
      Registry.mark_dirty name);

  (* Initialize from existing data *)
  src.iter (fun k v ->
      let entries = f k v in
      let _ = add_source k entries in
      List.iter
        (fun (k2, v2) ->
          let contribs =
            match Hashtbl.find_opt contributions k2 with
            | Some c -> c
            | None ->
              let c = Hashtbl.create 4 in
              Hashtbl.replace contributions k2 c;
              c
          in
          Hashtbl.replace contribs k v2;
          Hashtbl.replace target k2 v2)
        entries);

  {
    name;
    subscribe = (fun h -> subscribers := h :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
    stats = my_stats;
    level = my_level;
  }

(** {1 Join} *)

let join ~name (left : ('k1, 'v1) t) (right : ('k2, 'v2) t) ~key_of ~f ?merge ()
    : ('k3, 'v3) t =
  let my_level = max left.level right.level + 1 in
  let merge_fn =
    match merge with
    | Some m -> m
    | None -> fun _ v -> v
  in

  (* Internal state *)
  let left_entries : ('k1, 'v1) Hashtbl.t = Hashtbl.create 64 in
  let provenance : ('k1, 'k3 list) Hashtbl.t = Hashtbl.create 64 in
  let contributions : ('k3, ('k1, 'v3) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 256
  in
  let target : ('k3, 'v3) Hashtbl.t = Hashtbl.create 256 in
  let left_to_right_key : ('k1, 'k2) Hashtbl.t = Hashtbl.create 64 in
  let right_key_to_left_keys : ('k2, 'k1 list) Hashtbl.t = Hashtbl.create 64 in
  let subscribers = ref [] in
  let my_stats = create_stats () in

  (* Separate pending buffers for left and right *)
  let left_pending = ref [] in
  let right_pending = ref [] in

  let recompute_target k3 =
    match Hashtbl.find_opt contributions k3 with
    | None ->
      Hashtbl.remove target k3;
      Some (k3, None)
    | Some contribs when Hashtbl.length contribs = 0 ->
      Hashtbl.remove contributions k3;
      Hashtbl.remove target k3;
      Some (k3, None)
    | Some contribs ->
      let values = Hashtbl.fold (fun _ v acc -> v :: acc) contribs [] in
      let merged =
        match values with
        | [] -> assert false
        | [v] -> v
        | v :: rest -> List.fold_left merge_fn v rest
      in
      Hashtbl.replace target k3 merged;
      Some (k3, Some merged)
  in

  let remove_left_contributions k1 =
    match Hashtbl.find_opt provenance k1 with
    | None -> []
    | Some target_keys ->
      Hashtbl.remove provenance k1;
      List.iter
        (fun k3 ->
          match Hashtbl.find_opt contributions k3 with
          | None -> ()
          | Some contribs -> Hashtbl.remove contribs k1)
        target_keys;
      target_keys
  in

  let add_left_contributions k1 entries =
    let target_keys = List.map fst entries in
    Hashtbl.replace provenance k1 target_keys;
    List.iter
      (fun (k3, v3) ->
        let contribs =
          match Hashtbl.find_opt contributions k3 with
          | Some c -> c
          | None ->
            let c = Hashtbl.create 4 in
            Hashtbl.replace contributions k3 c;
            c
        in
        Hashtbl.replace contribs k1 v3)
      entries;
    target_keys
  in

  let process_left_entry k1 v1 =
    let old_affected = remove_left_contributions k1 in
    (* Update right key tracking *)
    (match Hashtbl.find_opt left_to_right_key k1 with
    | Some old_k2 -> (
      Hashtbl.remove left_to_right_key k1;
      match Hashtbl.find_opt right_key_to_left_keys old_k2 with
      | Some keys ->
        Hashtbl.replace right_key_to_left_keys old_k2
          (List.filter (fun k -> k <> k1) keys)
      | None -> ())
    | None -> ());
    let k2 = key_of k1 v1 in
    Hashtbl.replace left_to_right_key k1 k2;
    let keys =
      match Hashtbl.find_opt right_key_to_left_keys k2 with
      | Some ks -> ks
      | None -> []
    in
    Hashtbl.replace right_key_to_left_keys k2 (k1 :: keys);
    (* Compute output *)
    let right_val = right.get k2 in
    let new_entries = f k1 v1 right_val in
    let new_affected = add_left_contributions k1 new_entries in
    old_affected @ new_affected
  in

  let remove_left_entry k1 =
    Hashtbl.remove left_entries k1;
    let affected = remove_left_contributions k1 in
    (match Hashtbl.find_opt left_to_right_key k1 with
    | Some k2 -> (
      Hashtbl.remove left_to_right_key k1;
      match Hashtbl.find_opt right_key_to_left_keys k2 with
      | Some keys ->
        Hashtbl.replace right_key_to_left_keys k2
          (List.filter (fun k -> k <> k1) keys)
      | None -> ())
    | None -> ());
    affected
  in

  let process () =
    (* Track input deltas *)
    my_stats.deltas_received <-
      my_stats.deltas_received + List.length !left_pending
      + List.length !right_pending;

    (* Process both left and right pending *)
    let left_entries_list =
      !left_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    let right_entries_list =
      !right_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    left_pending := [];
    right_pending := [];

    my_stats.entries_received <-
      my_stats.entries_received
      + List.length left_entries_list
      + List.length right_entries_list;
    let left_adds, left_removes = count_adds_removes left_entries_list in
    let right_adds, right_removes = count_adds_removes right_entries_list in
    my_stats.adds_received <- my_stats.adds_received + left_adds + right_adds;
    my_stats.removes_received <-
      my_stats.removes_received + left_removes + right_removes;

    let all_affected = ref [] in

    (* Process left entries *)
    List.iter
      (fun (k1, v1_opt) ->
        match v1_opt with
        | Some v1 ->
          Hashtbl.replace left_entries k1 v1;
          let affected = process_left_entry k1 v1 in
          all_affected := affected @ !all_affected
        | None ->
          let affected = remove_left_entry k1 in
          all_affected := affected @ !all_affected)
      left_entries_list;

    (* Process right entries: reprocess affected left entries *)
    List.iter
      (fun (k2, _) ->
        match Hashtbl.find_opt right_key_to_left_keys k2 with
        | None -> ()
        | Some left_keys ->
          List.iter
            (fun k1 ->
              match Hashtbl.find_opt left_entries k1 with
              | Some v1 ->
                let affected = process_left_entry k1 v1 in
                all_affected := affected @ !all_affected
              | None -> ())
            left_keys)
      right_entries_list;

    (* Deduplicate and compute outputs *)
    let seen = Hashtbl.create (List.length !all_affected) in
    let output_entries =
      !all_affected
      |> List.filter_map (fun k3 ->
             if Hashtbl.mem seen k3 then None
             else (
               Hashtbl.replace seen k3 ();
               recompute_target k3))
    in

    if output_entries <> [] then (
      let num_adds, num_removes = count_changes output_entries in
      my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
      my_stats.entries_emitted <-
        my_stats.entries_emitted + List.length output_entries;
      my_stats.adds_emitted <- my_stats.adds_emitted + num_adds;
      my_stats.removes_emitted <- my_stats.removes_emitted + num_removes;
      let delta = Batch output_entries in
      List.iter (fun h -> h delta) !subscribers)
  in

  let _info =
    Registry.register ~name ~level:my_level ~process ~stats:my_stats
  in
  Registry.add_edge ~from_name:left.name ~to_name:name ~label:"join";
  Registry.add_edge ~from_name:right.name ~to_name:name ~label:"join";
  Registry.add_combinator ~name:(name ^ "_join") ~shape:"join"
    ~inputs:[left.name; right.name] ~output:name;

  (* Subscribe to sources: just accumulate *)
  left.subscribe (fun delta ->
      left_pending := delta :: !left_pending;
      Registry.mark_dirty name);

  right.subscribe (fun delta ->
      right_pending := delta :: !right_pending;
      Registry.mark_dirty name);

  (* Initialize from existing data *)
  left.iter (fun k1 v1 ->
      Hashtbl.replace left_entries k1 v1;
      let _ = process_left_entry k1 v1 in
      ());

  {
    name;
    subscribe = (fun h -> subscribers := h :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
    stats = my_stats;
    level = my_level;
  }

(** {1 Union} *)

let union ~name (left : ('k, 'v) t) (right : ('k, 'v) t) ?merge () : ('k, 'v) t
    =
  let my_level = max left.level right.level + 1 in
  let merge_fn =
    match merge with
    | Some m -> m
    | None -> fun _ v -> v
  in

  (* Internal state *)
  let left_values : ('k, 'v) Hashtbl.t = Hashtbl.create 64 in
  let right_values : ('k, 'v) Hashtbl.t = Hashtbl.create 64 in
  let target : ('k, 'v) Hashtbl.t = Hashtbl.create 128 in
  let subscribers = ref [] in
  let my_stats = create_stats () in

  (* Separate pending buffers *)
  let left_pending = ref [] in
  let right_pending = ref [] in

  let recompute_target k =
    match (Hashtbl.find_opt left_values k, Hashtbl.find_opt right_values k) with
    | None, None ->
      Hashtbl.remove target k;
      Some (k, None)
    | Some v, None | None, Some v ->
      Hashtbl.replace target k v;
      Some (k, Some v)
    | Some lv, Some rv ->
      let merged = merge_fn lv rv in
      Hashtbl.replace target k merged;
      Some (k, Some merged)
  in

  let process () =
    (* Track input deltas *)
    my_stats.deltas_received <-
      my_stats.deltas_received + List.length !left_pending
      + List.length !right_pending;

    let left_entries =
      !left_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    let right_entries =
      !right_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    left_pending := [];
    right_pending := [];

    my_stats.entries_received <-
      my_stats.entries_received + List.length left_entries
      + List.length right_entries;
    let left_adds, left_removes = count_adds_removes left_entries in
    let right_adds, right_removes = count_adds_removes right_entries in
    my_stats.adds_received <- my_stats.adds_received + left_adds + right_adds;
    my_stats.removes_received <-
      my_stats.removes_received + left_removes + right_removes;

    let all_affected = ref [] in

    (* Apply left entries *)
    List.iter
      (fun (k, v_opt) ->
        (match v_opt with
        | Some v -> Hashtbl.replace left_values k v
        | None -> Hashtbl.remove left_values k);
        all_affected := k :: !all_affected)
      left_entries;

    (* Apply right entries *)
    List.iter
      (fun (k, v_opt) ->
        (match v_opt with
        | Some v -> Hashtbl.replace right_values k v
        | None -> Hashtbl.remove right_values k);
        all_affected := k :: !all_affected)
      right_entries;

    (* Deduplicate and compute outputs *)
    let seen = Hashtbl.create (List.length !all_affected) in
    let output_entries =
      !all_affected
      |> List.filter_map (fun k ->
             if Hashtbl.mem seen k then None
             else (
               Hashtbl.replace seen k ();
               recompute_target k))
    in

    if output_entries <> [] then (
      let num_adds, num_removes = count_changes output_entries in
      my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
      my_stats.entries_emitted <-
        my_stats.entries_emitted + List.length output_entries;
      my_stats.adds_emitted <- my_stats.adds_emitted + num_adds;
      my_stats.removes_emitted <- my_stats.removes_emitted + num_removes;
      let delta = Batch output_entries in
      List.iter (fun h -> h delta) !subscribers)
  in

  let _info =
    Registry.register ~name ~level:my_level ~process ~stats:my_stats
  in
  Registry.add_edge ~from_name:left.name ~to_name:name ~label:"union";
  Registry.add_edge ~from_name:right.name ~to_name:name ~label:"union";
  Registry.add_combinator ~name:(name ^ "_union") ~shape:"union"
    ~inputs:[left.name; right.name] ~output:name;

  (* Subscribe to sources: just accumulate *)
  left.subscribe (fun delta ->
      left_pending := delta :: !left_pending;
      Registry.mark_dirty name);

  right.subscribe (fun delta ->
      right_pending := delta :: !right_pending;
      Registry.mark_dirty name);

  (* Initialize from existing data - process left then right *)
  left.iter (fun k v ->
      Hashtbl.replace left_values k v;
      let merged = merge_fn v v in
      (* self-merge for single value *)
      Hashtbl.replace target k merged);
  right.iter (fun k v ->
      Hashtbl.replace right_values k v;
      (* Right takes precedence, but merge if left exists *)
      let merged =
        match Hashtbl.find_opt left_values k with
        | Some lv -> merge_fn lv v
        | None -> v
      in
      Hashtbl.replace target k merged);

  {
    name;
    subscribe = (fun h -> subscribers := h :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
    stats = my_stats;
    level = my_level;
  }

(** {1 Fixpoint} *)

let fixpoint ~name ~(init : ('k, unit) t) ~(edges : ('k, 'k list) t) () :
    ('k, unit) t =
  let my_level = max init.level edges.level + 1 in

  (* Internal state *)
  let current : ('k, unit) Hashtbl.t = Hashtbl.create 256 in
  let edge_map : ('k, 'k list) Hashtbl.t = Hashtbl.create 256 in
  let subscribers = ref [] in
  let my_stats = create_stats () in

  (* Separate pending buffers *)
  let init_pending = ref [] in
  let edges_pending = ref [] in

  (* Track which nodes are roots *)
  let roots : ('k, unit) Hashtbl.t = Hashtbl.create 64 in

  (* BFS helper to find all reachable from roots *)
  let recompute_all () =
    let new_current = Hashtbl.create (Hashtbl.length current) in
    let frontier = Queue.create () in

    (* Start from all roots *)
    Hashtbl.iter
      (fun k () ->
        Hashtbl.replace new_current k ();
        Queue.add k frontier)
      roots;

    (* BFS *)
    while not (Queue.is_empty frontier) do
      let k = Queue.pop frontier in
      match Hashtbl.find_opt edge_map k with
      | None -> ()
      | Some successors ->
        List.iter
          (fun succ ->
            if not (Hashtbl.mem new_current succ) then (
              Hashtbl.replace new_current succ ();
              Queue.add succ frontier))
          successors
    done;
    new_current
  in

  let process () =
    (* Track input deltas *)
    my_stats.deltas_received <-
      my_stats.deltas_received + List.length !init_pending
      + List.length !edges_pending;

    let init_entries =
      !init_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    let edges_entries =
      !edges_pending |> List.concat_map delta_to_entries |> merge_entries
    in
    init_pending := [];
    edges_pending := [];

    my_stats.entries_received <-
      my_stats.entries_received + List.length init_entries
      + List.length edges_entries;
    let init_adds, init_removes = count_adds_removes init_entries in
    let edges_adds, edges_removes = count_adds_removes edges_entries in
    my_stats.adds_received <- my_stats.adds_received + init_adds + edges_adds;
    my_stats.removes_received <-
      my_stats.removes_received + init_removes + edges_removes;

    let output_entries = ref [] in
    let needs_full_recompute = ref false in

    (* Apply edge updates *)
    List.iter
      (fun (k, v_opt) ->
        match v_opt with
        | Some successors ->
          let old = Hashtbl.find_opt edge_map k in
          Hashtbl.replace edge_map k successors;
          (* If edges changed for a current node, may need recompute *)
          if Hashtbl.mem current k && old <> Some successors then
            needs_full_recompute := true
        | None ->
          if Hashtbl.mem edge_map k then (
            Hashtbl.remove edge_map k;
            if Hashtbl.mem current k then needs_full_recompute := true))
      edges_entries;

    (* Apply init updates *)
    List.iter
      (fun (k, v_opt) ->
        match v_opt with
        | Some () -> Hashtbl.replace roots k ()
        | None ->
          if Hashtbl.mem roots k then (
            Hashtbl.remove roots k;
            needs_full_recompute := true))
      init_entries;

    (* Either do incremental expansion or full recompute *)
    (if !needs_full_recompute then (
       (* Full recompute: find what changed *)
       let new_current = recompute_all () in

       (* Find removed entries *)
       Hashtbl.iter
         (fun k () ->
           if not (Hashtbl.mem new_current k) then
             output_entries := (k, None) :: !output_entries)
         current;

       (* Find added entries *)
       Hashtbl.iter
         (fun k () ->
           if not (Hashtbl.mem current k) then
             output_entries := (k, Some ()) :: !output_entries)
         new_current;

       (* Update current *)
       Hashtbl.reset current;
       Hashtbl.iter (fun k v -> Hashtbl.replace current k v) new_current)
     else
       (* Incremental: BFS from new roots *)
       let frontier = Queue.create () in

       init_entries
       |> List.iter (fun (k, v_opt) ->
              match v_opt with
              | Some () when not (Hashtbl.mem current k) ->
                Hashtbl.replace current k ();
                output_entries := (k, Some ()) :: !output_entries;
                Queue.add k frontier
              | _ -> ());

       while not (Queue.is_empty frontier) do
         let k = Queue.pop frontier in
         match Hashtbl.find_opt edge_map k with
         | None -> ()
         | Some successors ->
           List.iter
             (fun succ ->
               if not (Hashtbl.mem current succ) then (
                 Hashtbl.replace current succ ();
                 output_entries := (succ, Some ()) :: !output_entries;
                 Queue.add succ frontier))
             successors
       done);

    if !output_entries <> [] then (
      let num_adds, num_removes = count_changes !output_entries in
      my_stats.deltas_emitted <- my_stats.deltas_emitted + 1;
      my_stats.entries_emitted <-
        my_stats.entries_emitted + List.length !output_entries;
      my_stats.adds_emitted <- my_stats.adds_emitted + num_adds;
      my_stats.removes_emitted <- my_stats.removes_emitted + num_removes;
      let delta = Batch !output_entries in
      List.iter (fun h -> h delta) !subscribers)
  in

  let _info =
    Registry.register ~name ~level:my_level ~process ~stats:my_stats
  in
  Registry.add_edge ~from_name:init.name ~to_name:name ~label:"roots";
  Registry.add_edge ~from_name:edges.name ~to_name:name ~label:"edges";
  Registry.add_combinator ~name:(name ^ "_fp") ~shape:"fixpoint"
    ~inputs:[init.name; edges.name] ~output:name;

  (* Subscribe to sources: just accumulate *)
  init.subscribe (fun delta ->
      init_pending := delta :: !init_pending;
      Registry.mark_dirty name);

  edges.subscribe (fun delta ->
      edges_pending := delta :: !edges_pending;
      Registry.mark_dirty name);

  (* Initialize from existing data *)
  (* First, copy edges *)
  edges.iter (fun k v -> Hashtbl.replace edge_map k v);
  (* Then, BFS from existing init values *)
  let frontier = Queue.create () in
  init.iter (fun k () ->
      Hashtbl.replace roots k ();
      (* Track roots *)
      if not (Hashtbl.mem current k) then (
        Hashtbl.replace current k ();
        Queue.add k frontier));
  while not (Queue.is_empty frontier) do
    let k = Queue.pop frontier in
    match Hashtbl.find_opt edge_map k with
    | None -> ()
    | Some successors ->
      List.iter
        (fun succ ->
          if not (Hashtbl.mem current succ) then (
            Hashtbl.replace current succ ();
            Queue.add succ frontier))
        successors
  done;

  {
    name;
    subscribe = (fun h -> subscribers := h :: !subscribers);
    iter = (fun f -> Hashtbl.iter f current);
    get = (fun k -> Hashtbl.find_opt current k);
    length = (fun () -> Hashtbl.length current);
    stats = my_stats;
    level = my_level;
  }

(** {1 Utilities} *)

let to_mermaid () = Registry.to_mermaid ()
let print_stats () = Registry.print_stats ()
let reset () = Registry.clear ()
let reset_stats () = Registry.reset_stats ()
