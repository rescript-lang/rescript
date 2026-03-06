(** [list_iter_with f arg xs] calls [f arg x] for each [x] in [xs].
    Unlike [List.iter (f arg) xs], this avoids allocating a closure
    when [f] is a top-level function. *)
let rec list_iter_with f arg = function
  | [] -> ()
  | x :: rest ->
    f arg x;
    list_iter_with f arg rest

(* Note on set representations:
   [current] and [roots] stay as map-of-unit because they are updated as
   first-class maps in multiple places. [pred_map] is represented by
   [ReactivePoolMapSet] because its semantics are exactly map-of-set with
   churn-safe remove+recycle behavior. *)

type 'k metrics_state = {
  mutable delete_queue_pops: int;
  mutable delete_edges_scanned: int;
  mutable rederive_queue_pops: int;
  mutable rederived_nodes: int;
  mutable rederive_edges_scanned: int;
  mutable expansion_queue_pops: int;
  mutable expansion_edges_scanned: int;
  scratch_reachable: ('k, unit) ReactiveHash.Map.t;
}
(** Per-call metrics scratch state. Allocated once per fixpoint instance,
    mutable fields are reset and incremented in-place — zero allocation. *)

type 'k t = {
  current: ('k, unit) ReactiveHash.Map.t;
  edge_map: ('k, 'k list) ReactiveHash.Map.t;
  pred_map: ('k, 'k) ReactivePoolMapSet.t;
  roots: ('k, unit) ReactiveHash.Map.t;
  output_wave: ('k, unit ReactiveMaybe.t) ReactiveWave.t;
  (* Scratch tables — allocated once, cleared per apply_list call *)
  deleted_nodes: ('k, unit) ReactiveHash.Map.t;
  rederive_pending: ('k, unit) ReactiveHash.Map.t;
  expansion_seen: ('k, unit) ReactiveHash.Map.t;
  old_successors_for_changed: ('k, 'k list) ReactiveHash.Map.t;
  new_successors_for_changed: ('k, 'k list) ReactiveHash.Map.t;
  (* Scratch sets for analyze_edge_change / apply_edge_update *)
  scratch_set_a: 'k ReactiveHash.Set.t;
  scratch_set_b: 'k ReactiveHash.Set.t;
  edge_has_new: 'k ReactiveHash.Set.t;
  (* Scratch queues *)
  delete_queue: 'k ReactiveQueue.t;
  rederive_queue: 'k ReactiveQueue.t;
  expansion_queue: 'k ReactiveQueue.t;
  added_roots_queue: 'k ReactiveQueue.t;
  edge_change_queue: 'k ReactiveQueue.t;
  metrics: 'k metrics_state;
}

(* Standalone version for Invariants (no scratch sets available).
   Debug-only — allocates temporary Hashtbl. *)
let analyze_edge_change_has_new ~old_succs ~new_succs =
  match (old_succs, new_succs) with
  | [], [] -> false
  | [], _ -> true
  | _, [] -> false
  | _, _ ->
    let old_set = Hashtbl.create (List.length old_succs) in
    List.iter (fun k -> Hashtbl.replace old_set k ()) old_succs;
    List.exists (fun tgt -> not (Hashtbl.mem old_set tgt)) new_succs

(* Full-reachability BFS into [visited]. Returns (node_work, edge_work).
   [visited] is cleared before use; zero allocation when [visited] is
   pre-allocated (e.g. Metrics scratch map). *)
let bfs_seed_root visited frontier _t k () =
  ReactiveHash.Map.replace visited k ();
  ReactiveQueue.push frontier k

let bfs_visit_succ visited frontier succ =
  if not (ReactiveHash.Map.mem visited succ) then (
    ReactiveHash.Map.replace visited succ ();
    ReactiveQueue.push frontier succ)

let compute_reachable ~visited t =
  ReactiveHash.Map.clear visited;
  let frontier = t.delete_queue in
  ReactiveQueue.clear frontier;
  let node_work = ref 0 in
  let edge_work = ref 0 in
  ReactiveHash.Map.iter_with (bfs_seed_root visited frontier) t t.roots;
  while not (ReactiveQueue.is_empty frontier) do
    let k = ReactiveQueue.pop frontier in
    incr node_work;
    let r = ReactiveHash.Map.find_maybe t.edge_map k in
    if ReactiveMaybe.is_some r then (
      let succs = ReactiveMaybe.unsafe_get r in
      edge_work := !edge_work + List.length succs;
      list_iter_with (bfs_visit_succ visited) frontier succs)
  done;
  (!node_work, !edge_work)

module Metrics = struct
  let enabled =
    match Sys.getenv_opt "RESCRIPT_REACTIVE_FIXPOINT_METRICS" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  type t = {
    mutable apply_calls: int;
    mutable init_entries_total: int;
    mutable edge_entries_total: int;
    mutable output_entries_total: int;
    mutable deleted_nodes_total: int;
    mutable rederived_nodes_total: int;
    mutable delete_then_rederive_calls: int;
    mutable incr_node_work_total: int;
    mutable incr_edge_work_total: int;
    mutable full_node_work_total: int;
    mutable full_edge_work_total: int;
    mutable max_init_entries: int;
    mutable max_edge_entries: int;
    mutable max_deleted_nodes: int;
    mutable max_rederived_nodes: int;
  }

  let totals =
    {
      apply_calls = 0;
      init_entries_total = 0;
      edge_entries_total = 0;
      output_entries_total = 0;
      deleted_nodes_total = 0;
      rederived_nodes_total = 0;
      delete_then_rederive_calls = 0;
      incr_node_work_total = 0;
      incr_edge_work_total = 0;
      full_node_work_total = 0;
      full_edge_work_total = 0;
      max_init_entries = 0;
      max_edge_entries = 0;
      max_deleted_nodes = 0;
      max_rederived_nodes = 0;
    }

  let reset_per_call (m : _ metrics_state) =
    if enabled then (
      m.delete_queue_pops <- 0;
      m.delete_edges_scanned <- 0;
      m.rederive_queue_pops <- 0;
      m.rederived_nodes <- 0;
      m.rederive_edges_scanned <- 0;
      m.expansion_queue_pops <- 0;
      m.expansion_edges_scanned <- 0)

  let update ~init_entries ~edge_entries ~output_entries ~deleted_nodes
      ~rederived_nodes ~incr_node_work ~incr_edge_work ~full_node_work
      ~full_edge_work =
    if enabled then (
      totals.apply_calls <- totals.apply_calls + 1;
      totals.init_entries_total <- totals.init_entries_total + init_entries;
      totals.edge_entries_total <- totals.edge_entries_total + edge_entries;
      totals.output_entries_total <-
        totals.output_entries_total + output_entries;
      totals.deleted_nodes_total <- totals.deleted_nodes_total + deleted_nodes;
      totals.rederived_nodes_total <-
        totals.rederived_nodes_total + rederived_nodes;
      if deleted_nodes > 0 && rederived_nodes > 0 then
        totals.delete_then_rederive_calls <-
          totals.delete_then_rederive_calls + 1;
      totals.incr_node_work_total <-
        totals.incr_node_work_total + incr_node_work;
      totals.incr_edge_work_total <-
        totals.incr_edge_work_total + incr_edge_work;
      totals.full_node_work_total <-
        totals.full_node_work_total + full_node_work;
      totals.full_edge_work_total <-
        totals.full_edge_work_total + full_edge_work;
      totals.max_init_entries <- max totals.max_init_entries init_entries;
      totals.max_edge_entries <- max totals.max_edge_entries edge_entries;
      totals.max_deleted_nodes <- max totals.max_deleted_nodes deleted_nodes;
      totals.max_rederived_nodes <-
        max totals.max_rederived_nodes rederived_nodes)

  let emit_summary () =
    if enabled then
      let pct_incr_nodes =
        if totals.full_node_work_total = 0 then 0.
        else
          100.
          *. float_of_int totals.incr_node_work_total
          /. float_of_int totals.full_node_work_total
      in
      let pct_incr_edges =
        if totals.full_edge_work_total = 0 then 0.
        else
          100.
          *. float_of_int totals.incr_edge_work_total
          /. float_of_int totals.full_edge_work_total
      in
      prerr_endline
        (Printf.sprintf
           "[ReactiveFixpointMetrics] apply_calls=%d init_entries_total=%d \
            edge_entries_total=%d output_entries_total=%d \
            deleted_nodes_total=%d rederived_nodes_total=%d \
            delete_then_rederive_calls=%d incr_node_work_total=%d \
            full_node_work_total=%d incr_edge_work_total=%d \
            full_edge_work_total=%d incr_vs_full_nodes_pct=%.2f \
            incr_vs_full_edges_pct=%.2f max_init_entries=%d \
            max_edge_entries=%d max_deleted_nodes=%d max_rederived_nodes=%d"
           totals.apply_calls totals.init_entries_total
           totals.edge_entries_total totals.output_entries_total
           totals.deleted_nodes_total totals.rederived_nodes_total
           totals.delete_then_rederive_calls totals.incr_node_work_total
           totals.full_node_work_total totals.incr_edge_work_total
           totals.full_edge_work_total pct_incr_nodes pct_incr_edges
           totals.max_init_entries totals.max_edge_entries
           totals.max_deleted_nodes totals.max_rederived_nodes)

  let emit_summary_on_signal _ = emit_summary ()
  let () =
    if enabled then
      Sys.set_signal Sys.sigusr1 (Sys.Signal_handle emit_summary_on_signal)
  let () = at_exit emit_summary
end

module Invariants = struct
  let enabled =
    match Sys.getenv_opt "RESCRIPT_REACTIVE_FIXPOINT_ASSERT" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  let () =
    if enabled then
      prerr_endline
        "[ReactiveFixpoint] debug invariants enabled \
         (RESCRIPT_REACTIVE_FIXPOINT_ASSERT)"

  let assert_ condition message =
    if enabled && not condition then failwith message

  (* Debug-only: copies a ReactiveHash.Map set into a Hashtbl for diffing.
     These allocations are acceptable since Invariants is opt-in debug code. *)
  let copy_rh_set_to_hashtbl (rh : ('k, unit) ReactiveHash.Map.t) =
    let out = Hashtbl.create (ReactiveHash.Map.cardinal rh) in
    ReactiveHash.Map.iter (fun k () -> Hashtbl.replace out k ()) rh;
    out

  let set_equal a b =
    Hashtbl.length a = Hashtbl.length b
    &&
    let ok = ref true in
    Hashtbl.iter (fun k () -> if not (Hashtbl.mem b k) then ok := false) a;
    !ok

  let assert_edge_has_new_consistent ~edge_change_queue
      ~old_successors_for_changed ~new_successors_for_changed ~edge_has_new =
    if enabled then (
      let q_copy = ReactiveQueue.create () in
      (* Drain and re-push to iterate without consuming *)
      let items = ref [] in
      while not (ReactiveQueue.is_empty edge_change_queue) do
        let src = ReactiveQueue.pop edge_change_queue in
        items := src :: !items;
        ReactiveQueue.push q_copy src
      done;
      (* Restore queue *)
      List.iter
        (fun src -> ReactiveQueue.push edge_change_queue src)
        (List.rev !items);
      (* Check each *)
      List.iter
        (fun src ->
          let r_old =
            ReactiveHash.Map.find_maybe old_successors_for_changed src
          in
          let old_succs =
            if ReactiveMaybe.is_some r_old then ReactiveMaybe.unsafe_get r_old
            else []
          in
          let r_new =
            ReactiveHash.Map.find_maybe new_successors_for_changed src
          in
          let new_succs =
            if ReactiveMaybe.is_some r_new then ReactiveMaybe.unsafe_get r_new
            else []
          in
          let expected_has_new =
            analyze_edge_change_has_new ~old_succs ~new_succs
          in
          let actual_has_new = ReactiveHash.Set.mem edge_has_new src in
          assert_
            (expected_has_new = actual_has_new)
            "ReactiveFixpoint.apply invariant failed: inconsistent edge_has_new")
        !items)

  let assert_deleted_nodes_closed ~current ~deleted_nodes ~old_successors =
    if enabled then
      ReactiveHash.Map.iter
        (fun k () ->
          assert_
            (ReactiveHash.Map.mem current k)
            "ReactiveFixpoint.apply invariant failed: deleted node not in \
             current";
          List.iter
            (fun succ ->
              if ReactiveHash.Map.mem current succ then
                assert_
                  (ReactiveHash.Map.mem deleted_nodes succ)
                  "ReactiveFixpoint.apply invariant failed: deleted closure \
                   broken")
            (old_successors k))
        deleted_nodes

  let assert_no_supported_deleted_left ~deleted_nodes ~current ~supported =
    if enabled then
      ReactiveHash.Map.iter
        (fun k () ->
          if not (ReactiveHash.Map.mem current k) then
            assert_
              (not (supported k))
              "ReactiveFixpoint.apply invariant failed: supported deleted node \
               left behind")
        deleted_nodes

  let assert_current_minus_deleted ~pre_current ~current ~deleted_nodes =
    if enabled then (
      let expected = Hashtbl.copy pre_current in
      ReactiveHash.Map.iter
        (fun k () -> Hashtbl.remove expected k)
        deleted_nodes;
      let current_ht = copy_rh_set_to_hashtbl current in
      assert_
        (set_equal expected current_ht)
        "ReactiveFixpoint.apply invariant failed: current != pre_current minus \
         deleted")

  let assert_removal_output_matches ~output_entries ~deleted_nodes ~current =
    if enabled then (
      let expected = Hashtbl.create (ReactiveHash.Map.cardinal deleted_nodes) in
      ReactiveHash.Map.iter
        (fun k () ->
          if not (ReactiveHash.Map.mem current k) then
            Hashtbl.replace expected k ())
        deleted_nodes;
      let actual = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, mv) ->
          if not (ReactiveMaybe.is_some mv) then Hashtbl.replace actual k ())
        output_entries;
      assert_
        (set_equal expected actual)
        "ReactiveFixpoint.apply invariant failed: removal output mismatch")

  let assert_final_fixpoint_and_delta ~visited ~t ~pre_current ~output_entries =
    if enabled then (
      ignore (compute_reachable ~visited t);
      let reachable = copy_rh_set_to_hashtbl visited in
      let current_ht = copy_rh_set_to_hashtbl t.current in
      assert_
        (set_equal reachable current_ht)
        "ReactiveFixpoint.apply invariant failed: current is not a fixed-point \
         closure";

      let expected_adds =
        Hashtbl.create (ReactiveHash.Map.cardinal t.current)
      in
      let expected_removes = Hashtbl.create (Hashtbl.length pre_current) in
      ReactiveHash.Map.iter
        (fun k () ->
          if not (Hashtbl.mem pre_current k) then
            Hashtbl.replace expected_adds k ())
        t.current;
      Hashtbl.iter
        (fun k () ->
          if not (ReactiveHash.Map.mem t.current k) then
            Hashtbl.replace expected_removes k ())
        pre_current;

      let actual_adds = Hashtbl.create (List.length output_entries) in
      let actual_removes = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, mv) ->
          if ReactiveMaybe.is_some mv then Hashtbl.replace actual_adds k ()
          else Hashtbl.replace actual_removes k ())
        output_entries;

      let adds_ok = set_equal expected_adds actual_adds in
      let removes_ok = set_equal expected_removes actual_removes in
      if not (adds_ok && removes_ok) then
        failwith
          (Printf.sprintf
             "ReactiveFixpoint.apply invariant failed: output delta mismatch \
              (pre=%d final=%d output=%d expected_adds=%d actual_adds=%d \
              expected_removes=%d actual_removes=%d)"
             (Hashtbl.length pre_current)
             (ReactiveHash.Map.cardinal t.current)
             (List.length output_entries)
             (Hashtbl.length expected_adds)
             (Hashtbl.length actual_adds)
             (Hashtbl.length expected_removes)
             (Hashtbl.length actual_removes)))
end

let create ~max_nodes ~max_edges =
  if max_nodes <= 0 then
    invalid_arg "ReactiveFixpoint.create: max_nodes must be > 0";
  if max_edges <= 0 then
    invalid_arg "ReactiveFixpoint.create: max_edges must be > 0";
  {
    current = ReactiveHash.Map.create ();
    edge_map = ReactiveHash.Map.create ();
    pred_map = ReactivePoolMapSet.create ~capacity:128;
    roots = ReactiveHash.Map.create ();
    output_wave = ReactiveWave.create ~max_entries:max_nodes ();
    deleted_nodes = ReactiveHash.Map.create ();
    rederive_pending = ReactiveHash.Map.create ();
    expansion_seen = ReactiveHash.Map.create ();
    old_successors_for_changed = ReactiveHash.Map.create ();
    scratch_set_a = ReactiveHash.Set.create ();
    scratch_set_b = ReactiveHash.Set.create ();
    edge_has_new = ReactiveHash.Set.create ();
    delete_queue = ReactiveQueue.create ();
    rederive_queue = ReactiveQueue.create ();
    expansion_queue = ReactiveQueue.create ();
    added_roots_queue = ReactiveQueue.create ();
    edge_change_queue = ReactiveQueue.create ();
    new_successors_for_changed = ReactiveHash.Map.create ();
    metrics =
      {
        delete_queue_pops = 0;
        delete_edges_scanned = 0;
        rederive_queue_pops = 0;
        rederived_nodes = 0;
        rederive_edges_scanned = 0;
        expansion_queue_pops = 0;
        expansion_edges_scanned = 0;
        scratch_reachable = ReactiveHash.Map.create ();
      };
  }

let destroy t = ReactiveWave.destroy t.output_wave
let output_wave t = t.output_wave

type 'k root_wave = ('k, unit ReactiveMaybe.t) ReactiveWave.t
type 'k edge_wave = ('k, 'k list ReactiveMaybe.t) ReactiveWave.t
type 'k output_wave = ('k, unit ReactiveMaybe.t) ReactiveWave.t
type 'k root_snapshot = ('k, unit) ReactiveWave.t
type 'k edge_snapshot = ('k, 'k list) ReactiveWave.t

let iter_current t f = ReactiveHash.Map.iter f t.current
let get_current t k = ReactiveHash.Map.find_maybe t.current k
let current_length t = ReactiveHash.Map.cardinal t.current

let recompute_current t = ignore (compute_reachable ~visited:t.current t)

let add_pred t ~target ~pred = ReactivePoolMapSet.add t.pred_map target pred

let remove_pred t ~target ~pred =
  ReactivePoolMapSet.remove_from_set_and_recycle_if_empty t.pred_map target pred

let has_live_pred_key t pred = ReactiveHash.Map.mem t.current pred

let has_live_predecessor t k =
  let r = ReactivePoolMapSet.find_maybe t.pred_map k in
  if ReactiveMaybe.is_some r then
    ReactiveHash.Set.exists_with has_live_pred_key t
      (ReactiveMaybe.unsafe_get r)
  else false

let apply_edge_update t ~src ~new_successors =
  let r = ReactiveHash.Map.find_maybe t.edge_map src in
  let old_successors =
    if ReactiveMaybe.is_some r then ReactiveMaybe.unsafe_get r else []
  in
  match (old_successors, new_successors) with
  | [], [] -> ReactiveHash.Map.remove t.edge_map src
  | [], _ ->
    List.iter (fun target -> add_pred t ~target ~pred:src) new_successors;
    ReactiveHash.Map.replace t.edge_map src new_successors
  | _, [] ->
    List.iter (fun target -> remove_pred t ~target ~pred:src) old_successors;
    ReactiveHash.Map.remove t.edge_map src
  | _, _ ->
    ReactiveHash.Set.clear t.scratch_set_a;
    ReactiveHash.Set.clear t.scratch_set_b;
    List.iter (fun k -> ReactiveHash.Set.add t.scratch_set_a k) new_successors;
    List.iter (fun k -> ReactiveHash.Set.add t.scratch_set_b k) old_successors;

    List.iter
      (fun target ->
        if not (ReactiveHash.Set.mem t.scratch_set_a target) then
          remove_pred t ~target ~pred:src)
      old_successors;

    List.iter
      (fun target ->
        if not (ReactiveHash.Set.mem t.scratch_set_b target) then
          add_pred t ~target ~pred:src)
      new_successors;

    ReactiveHash.Map.replace t.edge_map src new_successors

let initialize t ~roots ~edges =
  ReactiveHash.Map.clear t.roots;
  ReactiveHash.Map.clear t.edge_map;
  ReactivePoolMapSet.clear t.pred_map;
  ReactiveWave.iter roots (fun k _ ->
      ReactiveHash.Map.replace t.roots
        (ReactiveAllocator.unsafe_from_offheap k)
        ());
  ReactiveWave.iter edges (fun k successors ->
      apply_edge_update t
        ~src:(ReactiveAllocator.unsafe_from_offheap k)
        ~new_successors:(ReactiveAllocator.unsafe_from_offheap successors));
  recompute_current t

let is_supported t k =
  ReactiveHash.Map.mem t.roots k || has_live_predecessor t k

let old_successors t k =
  let r = ReactiveHash.Map.find_maybe t.old_successors_for_changed k in
  if ReactiveMaybe.is_some r then ReactiveMaybe.unsafe_get r
  else
    let r2 = ReactiveHash.Map.find_maybe t.edge_map k in
    if ReactiveMaybe.is_some r2 then ReactiveMaybe.unsafe_get r2 else []

let mark_deleted t k =
  if
    ReactiveHash.Map.mem t.current k
    && not (ReactiveHash.Map.mem t.deleted_nodes k)
  then (
    ReactiveHash.Map.replace t.deleted_nodes k ();
    ReactiveQueue.push t.delete_queue k)

let enqueue_expand t k =
  if
    ReactiveHash.Map.mem t.current k
    && not (ReactiveHash.Map.mem t.expansion_seen k)
  then (
    ReactiveHash.Map.replace t.expansion_seen k ();
    ReactiveQueue.push t.expansion_queue k)

let add_live t k =
  if not (ReactiveHash.Map.mem t.current k) then (
    ReactiveHash.Map.replace t.current k ();
    if not (ReactiveHash.Map.mem t.deleted_nodes k) then
      ReactiveWave.push t.output_wave
        (ReactiveAllocator.unsafe_to_offheap k)
        (ReactiveMaybe.maybe_unit_to_offheap (ReactiveMaybe.some ()));
    enqueue_expand t k)

let enqueue_rederive_if_needed t k =
  if
    ReactiveHash.Map.mem t.deleted_nodes k
    && (not (ReactiveHash.Map.mem t.current k))
    && (not (ReactiveHash.Map.mem t.rederive_pending k))
    && is_supported t k
  then (
    ReactiveHash.Map.replace t.rederive_pending k ();
    ReactiveQueue.push t.rederive_queue k)

let scan_root_entry t k mv =
  let had_root = ReactiveHash.Map.mem t.roots k in
  if ReactiveMaybe.is_some mv then (
    if not had_root then ReactiveQueue.push t.added_roots_queue k)
  else if had_root then mark_deleted t k

let set_add_k set k = ReactiveHash.Set.add set k

let rec mark_deleted_unless_in_set t set = function
  | [] -> ()
  | k :: rest ->
    if not (ReactiveHash.Set.mem set k) then mark_deleted t k;
    mark_deleted_unless_in_set t set rest

let rec list_exists_not_in_set set = function
  | [] -> false
  | k :: rest ->
    (not (ReactiveHash.Set.mem set k)) || list_exists_not_in_set set rest

let scan_edge_entry t src mv =
  let r = ReactiveHash.Map.find_maybe t.edge_map src in
  let old_succs =
    if ReactiveMaybe.is_some r then ReactiveMaybe.unsafe_get r else []
  in
  let new_succs =
    if ReactiveMaybe.is_some mv then ReactiveMaybe.unsafe_get mv else []
  in
  ReactiveHash.Map.replace t.old_successors_for_changed src old_succs;
  ReactiveHash.Map.replace t.new_successors_for_changed src new_succs;
  ReactiveQueue.push t.edge_change_queue src;
  let src_is_live = ReactiveHash.Map.mem t.current src in
  match (old_succs, new_succs) with
  | [], [] -> ()
  | [], _ -> ReactiveHash.Set.add t.edge_has_new src
  | _, [] -> if src_is_live then list_iter_with mark_deleted t old_succs
  | _, _ ->
    ReactiveHash.Set.clear t.scratch_set_a;
    ReactiveHash.Set.clear t.scratch_set_b;
    list_iter_with set_add_k t.scratch_set_a new_succs;
    list_iter_with set_add_k t.scratch_set_b old_succs;
    if src_is_live then mark_deleted_unless_in_set t t.scratch_set_a old_succs;
    if list_exists_not_in_set t.scratch_set_b new_succs then
      ReactiveHash.Set.add t.edge_has_new src

let apply_root_mutation t k mv =
  if ReactiveMaybe.is_some mv then ReactiveHash.Map.replace t.roots k ()
  else ReactiveHash.Map.remove t.roots k

let emit_removal t k () =
  if not (ReactiveHash.Map.mem t.current k) then
    ReactiveWave.push t.output_wave
      (ReactiveAllocator.unsafe_to_offheap k)
      ReactiveMaybe.none_offheap

let rebuild_edge_change_queue t src _succs =
  ReactiveQueue.push t.edge_change_queue src

let remove_from_current t k () = ReactiveHash.Map.remove t.current k

let enqueue_rederive_if_needed_kv t k () = enqueue_rederive_if_needed t k

let apply_list t ~roots ~edges =
  let pre_current =
    if Invariants.enabled then
      Some (Invariants.copy_rh_set_to_hashtbl t.current)
    else None
  in
  (* Clear all scratch state up front *)
  ReactiveHash.Map.clear t.deleted_nodes;
  ReactiveQueue.clear t.delete_queue;
  ReactiveQueue.clear t.added_roots_queue;
  ReactiveQueue.clear t.edge_change_queue;
  ReactiveHash.Map.clear t.old_successors_for_changed;
  ReactiveHash.Map.clear t.new_successors_for_changed;
  ReactiveHash.Set.clear t.edge_has_new;
  let m = t.metrics in
  Metrics.reset_per_call m;

  (* Phase 1a: scan init entries — seed delete queue for removed roots,
     buffer added roots for later expansion *)
  ReactiveWave.iter_with roots
    (fun t k mv ->
      scan_root_entry t
        (ReactiveAllocator.unsafe_from_offheap k)
        (ReactiveAllocator.unsafe_from_offheap mv))
    t;

  (* Phase 1b: scan edge entries — seed delete queue for removed targets,
     store new_succs and has_new_edge for later phases *)
  ReactiveWave.iter_with edges
    (fun t src mv ->
      scan_edge_entry t
        (ReactiveAllocator.unsafe_from_offheap src)
        (ReactiveAllocator.unsafe_from_offheap mv))
    t;

  Invariants.assert_edge_has_new_consistent
    ~edge_change_queue:t.edge_change_queue
    ~old_successors_for_changed:t.old_successors_for_changed
    ~new_successors_for_changed:t.new_successors_for_changed
    ~edge_has_new:t.edge_has_new;

  (* Phase 2: delete BFS *)
  while not (ReactiveQueue.is_empty t.delete_queue) do
    let k = ReactiveQueue.pop t.delete_queue in
    let succs = old_successors t k in
    if Metrics.enabled then (
      m.delete_queue_pops <- m.delete_queue_pops + 1;
      m.delete_edges_scanned <- m.delete_edges_scanned + List.length succs);
    list_iter_with mark_deleted t succs
  done;
  if Invariants.enabled then
    Invariants.assert_deleted_nodes_closed ~current:t.current
      ~deleted_nodes:t.deleted_nodes ~old_successors:(old_successors t);

  (* Phase 3: apply root and edge mutations *)
  ReactiveWave.iter_with roots
    (fun t k mv ->
      apply_root_mutation t
        (ReactiveAllocator.unsafe_from_offheap k)
        (ReactiveAllocator.unsafe_from_offheap mv))
    t;

  (* Apply edge updates by draining edge_change_queue. *)
  while not (ReactiveQueue.is_empty t.edge_change_queue) do
    let src = ReactiveQueue.pop t.edge_change_queue in
    let r = ReactiveHash.Map.find_maybe t.new_successors_for_changed src in
    let new_succs =
      if ReactiveMaybe.is_some r then ReactiveMaybe.unsafe_get r else []
    in
    apply_edge_update t ~src ~new_successors:new_succs
  done;
  (* Rebuild edge_change_queue from new_successors_for_changed keys for
     use in expansion seeding below *)
  ReactiveHash.Map.iter_with rebuild_edge_change_queue t
    t.new_successors_for_changed;

  ReactiveHash.Map.iter_with remove_from_current t t.deleted_nodes;
  (match pre_current with
  | Some pre ->
    Invariants.assert_current_minus_deleted ~pre_current:pre ~current:t.current
      ~deleted_nodes:t.deleted_nodes
  | None -> ());

  (* Phase 4: rederive *)
  ReactiveQueue.clear t.rederive_queue;
  ReactiveHash.Map.clear t.rederive_pending;

  ReactiveHash.Map.iter_with enqueue_rederive_if_needed_kv t t.deleted_nodes;

  while not (ReactiveQueue.is_empty t.rederive_queue) do
    let k = ReactiveQueue.pop t.rederive_queue in
    if Metrics.enabled then m.rederive_queue_pops <- m.rederive_queue_pops + 1;
    ReactiveHash.Map.remove t.rederive_pending k;
    if
      ReactiveHash.Map.mem t.deleted_nodes k
      && (not (ReactiveHash.Map.mem t.current k))
      && is_supported t k
    then (
      ReactiveHash.Map.replace t.current k ();
      if Metrics.enabled then m.rederived_nodes <- m.rederived_nodes + 1;
      let r = ReactiveHash.Map.find_maybe t.edge_map k in
      if ReactiveMaybe.is_some r then (
        let succs = ReactiveMaybe.unsafe_get r in
        if Metrics.enabled then
          m.rederive_edges_scanned <-
            m.rederive_edges_scanned + List.length succs;
        list_iter_with enqueue_rederive_if_needed t succs))
  done;
  if Invariants.enabled then
    Invariants.assert_no_supported_deleted_left ~deleted_nodes:t.deleted_nodes
      ~current:t.current ~supported:(is_supported t);

  (* Phase 5: expansion *)
  ReactiveQueue.clear t.expansion_queue;
  ReactiveHash.Map.clear t.expansion_seen;

  (* Seed expansion from added roots *)
  while not (ReactiveQueue.is_empty t.added_roots_queue) do
    add_live t (ReactiveQueue.pop t.added_roots_queue)
  done;

  (* Seed expansion from edge changes with new edges *)
  while not (ReactiveQueue.is_empty t.edge_change_queue) do
    let src = ReactiveQueue.pop t.edge_change_queue in
    if
      ReactiveHash.Map.mem t.current src
      && ReactiveHash.Set.mem t.edge_has_new src
    then enqueue_expand t src
  done;

  while not (ReactiveQueue.is_empty t.expansion_queue) do
    let k = ReactiveQueue.pop t.expansion_queue in
    if Metrics.enabled then m.expansion_queue_pops <- m.expansion_queue_pops + 1;
    let r = ReactiveHash.Map.find_maybe t.edge_map k in
    if ReactiveMaybe.is_some r then (
      let succs = ReactiveMaybe.unsafe_get r in
      if Metrics.enabled then
        m.expansion_edges_scanned <-
          m.expansion_edges_scanned + List.length succs;
      list_iter_with add_live t succs)
  done;
  ReactiveHash.Map.iter_with emit_removal t t.deleted_nodes;
  let output_entries_list =
    if Invariants.enabled then (
      let entries = ref [] in
      ReactiveWave.iter t.output_wave (fun k v_opt ->
          entries :=
            ( ReactiveAllocator.unsafe_from_offheap k,
              ReactiveAllocator.unsafe_from_offheap v_opt )
            :: !entries);
      !entries)
    else []
  in
  Invariants.assert_removal_output_matches ~output_entries:output_entries_list
    ~deleted_nodes:t.deleted_nodes ~current:t.current;
  (match pre_current with
  | Some pre ->
    Invariants.assert_final_fixpoint_and_delta
      ~visited:t.metrics.scratch_reachable ~t ~pre_current:pre
      ~output_entries:output_entries_list
  | None -> ());

  if Metrics.enabled then
    let full_node_work, full_edge_work =
      compute_reachable ~visited:t.metrics.scratch_reachable t
    in
    let init_count = ReactiveWave.count roots in
    let edge_count = ReactiveWave.count edges in
    let incr_node_work =
      init_count + edge_count + m.delete_queue_pops + m.rederive_queue_pops
      + m.expansion_queue_pops
    in
    let incr_edge_work =
      m.delete_edges_scanned + m.rederive_edges_scanned
      + m.expansion_edges_scanned
    in
    Metrics.update ~init_entries:init_count ~edge_entries:edge_count
      ~output_entries:(ReactiveWave.count t.output_wave)
      ~deleted_nodes:(ReactiveHash.Map.cardinal t.deleted_nodes)
      ~rederived_nodes:m.rederived_nodes ~incr_node_work ~incr_edge_work
      ~full_node_work ~full_edge_work

let apply_wave t ~roots ~edges =
  ReactiveWave.clear t.output_wave;
  apply_list t ~roots ~edges;
  ()
