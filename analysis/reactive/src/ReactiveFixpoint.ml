(* Note on set representations:
   [pred_map] is represented by [StableMapSet] because its semantics are
   exactly map-of-set with churn-safe remove+recycle behavior. *)

type 'k metrics_state = {
  mutable delete_queue_pops: int;
  mutable delete_edges_scanned: int;
  mutable rederive_queue_pops: int;
  mutable rederived_nodes: int;
  mutable rederive_edges_scanned: int;
  mutable expansion_queue_pops: int;
  mutable expansion_edges_scanned: int;
  scratch_reachable: 'k StableSet.t;
}
(** Per-call metrics scratch state. Allocated once per fixpoint instance,
    mutable fields are reset and incremented in-place — zero allocation. *)

type 'k t = {
  current: 'k StableSet.t;
  edge_map: ('k, 'k StableList.t) StableMap.t;
  pred_map: ('k, 'k) StableMapSet.t;
  roots: 'k StableSet.t;
  output_wave: ('k, unit Maybe.t) StableWave.t;
  (* Scratch tables — allocated once, cleared per apply_list call *)
  deleted_nodes: 'k StableSet.t;
  rederive_pending: 'k StableSet.t;
  expansion_seen: 'k StableSet.t;
  old_successors_for_changed: ('k, 'k StableList.t) StableMap.t;
  new_successors_for_changed: ('k, 'k StableList.t) StableMap.t;
  (* Scratch sets for analyze_edge_change / apply_edge_update *)
  scratch_set_a: 'k StableSet.t;
  scratch_set_b: 'k StableSet.t;
  edge_has_new: 'k StableSet.t;
  (* Scratch queues *)
  delete_queue: 'k StableQueue.t;
  rederive_queue: 'k StableQueue.t;
  expansion_queue: 'k StableQueue.t;
  added_roots_queue: 'k StableQueue.t;
  edge_change_queue: 'k StableQueue.t;
  (* Scratch sets for Invariants — allocated once, zero-alloc when used *)
  inv_pre_current: 'k StableSet.t;
  inv_scratch_a: 'k StableSet.t;
  inv_scratch_b: 'k StableSet.t;
  metrics: 'k metrics_state;
}

let analyze_edge_change_has_new scratch ~old_succs ~new_succs =
  if StableList.is_empty old_succs then not (StableList.is_empty new_succs)
  else if StableList.is_empty new_succs then false
  else begin
    StableSet.clear scratch;
    StableList.iter (fun k -> StableSet.add scratch k) old_succs;
    StableList.exists (fun tgt -> not (StableSet.mem scratch tgt)) new_succs
  end

let[@inline] enqueue q k = StableQueue.push q k

(* Helpers for StableList values stored in StableMap/StableWave.
   The map stores 'v Stable.t, so find returns StableList.t Stable.t
   and replace expects StableList.t Stable.t. *)
let[@inline] succs_of_stable (s : 'a StableList.t Stable.t) : 'a StableList.t =
  Stable.to_linear_value s

let[@inline] find_succs map k =
  let r = StableMap.find_maybe map k in
  if Maybe.is_some r then succs_of_stable (Maybe.unsafe_get r)
  else StableList.empty ()

let[@inline] succs_of_maybe mv =
  if Maybe.is_some mv then succs_of_stable (Maybe.unsafe_get mv)
  else StableList.empty ()

(* Full-reachability BFS into [visited]. Returns (node_work, edge_work).
   [visited] is cleared before use; zero allocation when [visited] is
   pre-allocated (e.g. Metrics scratch map). *)
let bfs_seed_root visited frontier _t k () =
  StableSet.add visited k;
  StableQueue.push frontier k

let bfs_visit_succ visited frontier succ =
  if not (StableSet.mem visited succ) then (
    StableSet.add visited succ;
    StableQueue.push frontier succ)

let compute_reachable ~visited t =
  StableSet.clear visited;
  let frontier = t.delete_queue in
  StableQueue.clear frontier;
  let node_work = ref 0 in
  let edge_work = ref 0 in
  StableSet.iter_with
    (fun (visited, frontier) k -> bfs_seed_root visited frontier t k ())
    (visited, frontier) t.roots;
  while not (StableQueue.is_empty frontier) do
    let k = StableQueue.pop frontier in
    incr node_work;
    let r = StableMap.find_maybe t.edge_map k in
    if Maybe.is_some r then (
      let succs = succs_of_stable (Maybe.unsafe_get r) in
      edge_work := !edge_work + StableList.length succs;
      StableList.iter_with (bfs_visit_succ visited) frontier succs)
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

  let stable_set_equal a b =
    StableSet.cardinal a = StableSet.cardinal b
    &&
    let ok = ref true in
    StableSet.iter_with
      (fun (ok, b) k -> if !ok && not (StableSet.mem b k) then ok := false)
      (ok, b) a;
    !ok

  let copy_stable_set ~dst src =
    StableSet.clear dst;
    StableSet.iter_with (fun dst k -> StableSet.add dst k) dst src

  let assert_edge_has_new_consistent ~inv_scratch_a ~old_successors_for_changed
      ~new_successors_for_changed ~edge_has_new =
    if enabled then
      StableMap.iter_with
        (fun (scratch, new_map, edge_has_new) src old_succs_s ->
          let old_succs = succs_of_stable old_succs_s in
          let new_succs = find_succs new_map src in
          let expected_has_new =
            analyze_edge_change_has_new scratch ~old_succs ~new_succs
          in
          let actual_has_new = StableSet.mem edge_has_new src in
          assert_
            (expected_has_new = actual_has_new)
            "ReactiveFixpoint.apply invariant failed: inconsistent edge_has_new")
        (inv_scratch_a, new_successors_for_changed, edge_has_new)
        old_successors_for_changed

  let assert_deleted_nodes_closed ~current ~deleted_nodes
      ~(old_successors : 'k Stable.t -> 'k StableList.t) =
    if enabled then
      StableSet.iter_with
        (fun () k ->
          assert_ (StableSet.mem current k)
            "ReactiveFixpoint.apply invariant failed: deleted node not in \
             current";
          StableList.iter
            (fun succ ->
              if StableSet.mem current succ then
                assert_
                  (StableSet.mem deleted_nodes succ)
                  "ReactiveFixpoint.apply invariant failed: deleted closure \
                   broken")
            (old_successors k))
        () deleted_nodes

  let assert_no_supported_deleted_left ~deleted_nodes ~current ~supported =
    if enabled then
      StableSet.iter_with
        (fun () k ->
          if not (StableSet.mem current k) then
            assert_
              (not (supported k))
              "ReactiveFixpoint.apply invariant failed: supported deleted node \
               left behind")
        () deleted_nodes

  let assert_current_minus_deleted ~inv_scratch_a ~pre_current ~current
      ~deleted_nodes =
    if enabled then (
      copy_stable_set ~dst:inv_scratch_a pre_current;
      StableSet.iter_with
        (fun dst k -> StableSet.remove dst k)
        inv_scratch_a deleted_nodes;
      assert_
        (stable_set_equal inv_scratch_a current)
        "ReactiveFixpoint.apply invariant failed: current != pre_current minus \
         deleted")

  let assert_removal_output_matches ~inv_scratch_a ~inv_scratch_b ~output_wave
      ~deleted_nodes ~current =
    if enabled then (
      StableSet.clear inv_scratch_a;
      StableSet.iter_with
        (fun (dst, current) k ->
          if not (StableSet.mem current k) then StableSet.add dst k)
        (inv_scratch_a, current) deleted_nodes;
      StableSet.clear inv_scratch_b;
      StableWave.iter_with output_wave
        (fun dst k mv ->
          if not (Maybe.is_some (Stable.to_linear_value mv)) then
            StableSet.add dst k)
        inv_scratch_b;
      assert_
        (stable_set_equal inv_scratch_a inv_scratch_b)
        "ReactiveFixpoint.apply invariant failed: removal output mismatch")

  let assert_final_fixpoint_and_delta ~inv_scratch_a ~inv_scratch_b ~visited ~t
      ~pre_current ~output_wave =
    if enabled then (
      ignore (compute_reachable ~visited t);
      assert_
        (stable_set_equal visited t.current)
        "ReactiveFixpoint.apply invariant failed: current is not a fixed-point \
         closure";
      (* Check adds *)
      StableSet.clear inv_scratch_a;
      StableSet.iter_with
        (fun (dst, pre) k -> if not (StableSet.mem pre k) then StableSet.add dst k)
        (inv_scratch_a, pre_current) t.current;
      StableSet.clear inv_scratch_b;
      StableWave.iter_with output_wave
        (fun dst k mv ->
          if Maybe.is_some (Stable.to_linear_value mv) then StableSet.add dst k)
        inv_scratch_b;
      let adds_ok = stable_set_equal inv_scratch_a inv_scratch_b in
      (* Check removes *)
      StableSet.clear inv_scratch_a;
      StableSet.iter_with
        (fun (dst, current) k ->
          if not (StableSet.mem current k) then StableSet.add dst k)
        (inv_scratch_a, t.current) pre_current;
      StableSet.clear inv_scratch_b;
      StableWave.iter_with output_wave
        (fun dst k mv ->
          if not (Maybe.is_some (Stable.to_linear_value mv)) then
            StableSet.add dst k)
        inv_scratch_b;
      let removes_ok = stable_set_equal inv_scratch_a inv_scratch_b in
      if not (adds_ok && removes_ok) then
        failwith
          (Printf.sprintf
             "ReactiveFixpoint.apply invariant failed: output delta mismatch \
              (pre=%d final=%d output=%d)"
             (StableSet.cardinal pre_current)
             (StableSet.cardinal t.current)
             (StableWave.count output_wave)))
end

let create ~max_nodes ~max_edges =
  if max_nodes <= 0 then
    invalid_arg "ReactiveFixpoint.create: max_nodes must be > 0";
  if max_edges <= 0 then
    invalid_arg "ReactiveFixpoint.create: max_edges must be > 0";
  {
    current = StableSet.create ();
    edge_map = StableMap.create ();
    pred_map = StableMapSet.create ();
    roots = StableSet.create ();
    output_wave = StableWave.create ~max_entries:max_nodes ();
    deleted_nodes = StableSet.create ();
    rederive_pending = StableSet.create ();
    expansion_seen = StableSet.create ();
    old_successors_for_changed = StableMap.create ();
    scratch_set_a = StableSet.create ();
    scratch_set_b = StableSet.create ();
    edge_has_new = StableSet.create ();
    delete_queue = StableQueue.create ();
    rederive_queue = StableQueue.create ();
    expansion_queue = StableQueue.create ();
    added_roots_queue = StableQueue.create ();
    edge_change_queue = StableQueue.create ();
    inv_pre_current = StableSet.create ();
    inv_scratch_a = StableSet.create ();
    inv_scratch_b = StableSet.create ();
    new_successors_for_changed = StableMap.create ();
    metrics =
      {
        delete_queue_pops = 0;
        delete_edges_scanned = 0;
        rederive_queue_pops = 0;
        rederived_nodes = 0;
        rederive_edges_scanned = 0;
        expansion_queue_pops = 0;
        expansion_edges_scanned = 0;
        scratch_reachable = StableSet.create ();
      };
  }

let destroy t =
  StableSet.destroy t.current;
  StableMap.destroy t.edge_map;
  StableMapSet.destroy t.pred_map;
  StableSet.destroy t.roots;
  StableSet.destroy t.deleted_nodes;
  StableSet.destroy t.rederive_pending;
  StableSet.destroy t.expansion_seen;
  StableMap.destroy t.old_successors_for_changed;
  StableMap.destroy t.new_successors_for_changed;
  StableSet.destroy t.scratch_set_a;
  StableSet.destroy t.scratch_set_b;
  StableSet.destroy t.edge_has_new;
  StableQueue.destroy t.delete_queue;
  StableQueue.destroy t.rederive_queue;
  StableQueue.destroy t.expansion_queue;
  StableQueue.destroy t.added_roots_queue;
  StableQueue.destroy t.edge_change_queue;
  StableSet.destroy t.inv_pre_current;
  StableSet.destroy t.inv_scratch_a;
  StableSet.destroy t.inv_scratch_b;
  StableSet.destroy t.metrics.scratch_reachable;
  StableWave.destroy t.output_wave
let output_wave t = t.output_wave

type 'k root_wave = ('k, unit Maybe.t) StableWave.t
type 'k edge_wave = ('k, 'k StableList.t Maybe.t) StableWave.t
type 'k output_wave = ('k, unit Maybe.t) StableWave.t
type 'k root_snapshot = ('k, unit) StableWave.t
type 'k edge_snapshot = ('k, 'k StableList.t) StableWave.t

let iter_current t f =
  StableSet.iter_with (fun f k -> f k Stable.unit) f t.current

let get_current t k =
  if StableSet.mem t.current k then Maybe.some Stable.unit else Maybe.none

let current_length t = StableSet.cardinal t.current

let recompute_current t = ignore (compute_reachable ~visited:t.current t)

let add_pred t ~target ~pred = StableMapSet.add t.pred_map target pred

let remove_pred t ~target ~pred =
  StableMapSet.remove_from_set_and_recycle_if_empty t.pred_map target pred

let has_live_pred_key t pred = StableSet.mem t.current pred

let has_live_predecessor t k =
  StableMapSet.exists_inner_with t.pred_map k t has_live_pred_key

let add_pred_for_src (t, src) target = add_pred t ~target ~pred:src
let remove_pred_for_src (t, src) target = remove_pred t ~target ~pred:src

let apply_edge_update t ~src ~new_successors =
  let old_successors = find_succs t.edge_map src in
  if StableList.is_empty old_successors && StableList.is_empty new_successors
  then StableMap.remove t.edge_map src
  else if StableList.is_empty old_successors then (
    StableList.iter_with add_pred_for_src (t, src) new_successors;
    StableMap.replace t.edge_map src (StableList.to_stable new_successors))
  else if StableList.is_empty new_successors then (
    StableList.iter_with remove_pred_for_src (t, src) old_successors;
    StableMap.remove t.edge_map src)
  else (
    StableSet.clear t.scratch_set_a;
    StableSet.clear t.scratch_set_b;
    StableList.iter (fun k -> StableSet.add t.scratch_set_a k) new_successors;
    StableList.iter (fun k -> StableSet.add t.scratch_set_b k) old_successors;

    StableList.iter_with
      (fun () target ->
        if not (StableSet.mem t.scratch_set_a target) then
          remove_pred t ~target ~pred:src)
      () old_successors;

    StableList.iter_with
      (fun () target ->
        if not (StableSet.mem t.scratch_set_b target) then
          add_pred t ~target ~pred:src)
      () new_successors;

    StableMap.replace t.edge_map src (StableList.to_stable new_successors))

let initialize t ~roots ~edges =
  StableSet.clear t.roots;
  StableMap.clear t.edge_map;
  StableMapSet.clear t.pred_map;
  StableWave.iter roots (fun k _ -> StableSet.add t.roots k);
  StableWave.iter edges (fun k successors ->
      apply_edge_update t ~src:k ~new_successors:(succs_of_stable successors));
  recompute_current t

let is_supported t k = StableSet.mem t.roots k || has_live_predecessor t k

let old_successors t k =
  let r = StableMap.find_maybe t.old_successors_for_changed k in
  if Maybe.is_some r then succs_of_stable (Maybe.unsafe_get r)
  else find_succs t.edge_map k

let mark_deleted t k =
  if StableSet.mem t.current k && not (StableSet.mem t.deleted_nodes k) then (
    StableSet.add t.deleted_nodes k;
    enqueue t.delete_queue k)

let enqueue_expand t k =
  if StableSet.mem t.current k && not (StableSet.mem t.expansion_seen k) then (
    StableSet.add t.expansion_seen k;
    enqueue t.expansion_queue k)

let add_live t k =
  if not (StableSet.mem t.current k) then (
    StableSet.add t.current k;
    if not (StableSet.mem t.deleted_nodes k) then
      StableWave.push t.output_wave k (Maybe.to_stable (Maybe.some Stable.unit));
    enqueue_expand t k)

let enqueue_rederive_if_needed t k =
  if
    StableSet.mem t.deleted_nodes k
    && (not (StableSet.mem t.current k))
    && (not (StableSet.mem t.rederive_pending k))
    && is_supported t k
  then (
    StableSet.add t.rederive_pending k;
    enqueue t.rederive_queue k)

let scan_root_entry t k mv =
  let had_root = StableSet.mem t.roots k in
  if Maybe.is_some mv then (if not had_root then enqueue t.added_roots_queue k)
  else if had_root then mark_deleted t k

let set_add_k set k = StableSet.add set k

let mark_deleted_if_absent (t, set) k =
  if not (StableSet.mem set k) then mark_deleted t k

let not_in_set set k = not (StableSet.mem set k)

let mark_deleted_unless_in_set t set xs =
  StableList.iter_with mark_deleted_if_absent (t, set) xs

let exists_not_in_set set xs = StableList.exists_with not_in_set set xs

let scan_edge_entry t src mv =
  let old_succs = find_succs t.edge_map src in
  let new_succs = succs_of_maybe mv in
  StableMap.replace t.old_successors_for_changed src
    (StableList.to_stable old_succs);
  StableMap.replace t.new_successors_for_changed src
    (StableList.to_stable new_succs);
  enqueue t.edge_change_queue src;
  let src_is_live = StableSet.mem t.current src in
  match (old_succs, new_succs) with
  | _ when StableList.is_empty old_succs && StableList.is_empty new_succs -> ()
  | _ when StableList.is_empty old_succs -> StableSet.add t.edge_has_new src
  | _ when StableList.is_empty new_succs ->
    if src_is_live then StableList.iter_with mark_deleted t old_succs
  | _ ->
    StableSet.clear t.scratch_set_a;
    StableSet.clear t.scratch_set_b;
    StableList.iter_with set_add_k t.scratch_set_a new_succs;
    StableList.iter_with set_add_k t.scratch_set_b old_succs;
    if src_is_live then mark_deleted_unless_in_set t t.scratch_set_a old_succs;
    if exists_not_in_set t.scratch_set_b new_succs then
      StableSet.add t.edge_has_new src

let apply_root_mutation t k mv =
  if Maybe.is_some mv then StableSet.add t.roots k
  else StableSet.remove t.roots k

let emit_removal t k () =
  if not (StableSet.mem t.current k) then
    StableWave.push t.output_wave k Maybe.none_stable

let rebuild_edge_change_queue t src _succs =
  StableQueue.push t.edge_change_queue src

let remove_from_current t k = StableSet.remove t.current k

let enqueue_rederive_if_needed_kv t k = enqueue_rederive_if_needed t k

let apply_list t ~roots ~edges =
  if Invariants.enabled then
    Invariants.copy_stable_set ~dst:t.inv_pre_current t.current;
  (* Clear all scratch state up front *)
  StableSet.clear t.deleted_nodes;
  StableQueue.clear t.delete_queue;
  StableQueue.clear t.added_roots_queue;
  StableQueue.clear t.edge_change_queue;
  StableMap.clear t.old_successors_for_changed;
  StableMap.clear t.new_successors_for_changed;
  StableSet.clear t.edge_has_new;
  let m = t.metrics in
  Metrics.reset_per_call m;

  (* Phase 1a: scan init entries — seed delete queue for removed roots,
     buffer added roots for later expansion *)
  StableWave.iter_with roots
    (fun t k mv -> scan_root_entry t k (Stable.to_linear_value mv))
    t;

  (* Phase 1b: scan edge entries — seed delete queue for removed targets,
     store new_succs and has_new_edge for later phases *)
  StableWave.iter_with edges
    (fun t src mv -> scan_edge_entry t src (Maybe.of_stable mv))
    t;

  Invariants.assert_edge_has_new_consistent ~inv_scratch_a:t.inv_scratch_a
    ~old_successors_for_changed:t.old_successors_for_changed
    ~new_successors_for_changed:t.new_successors_for_changed
    ~edge_has_new:t.edge_has_new;

  (* Phase 2: delete BFS *)
  while not (StableQueue.is_empty t.delete_queue) do
    let k = StableQueue.pop t.delete_queue in
    let succs = old_successors t k in
    if Metrics.enabled then (
      m.delete_queue_pops <- m.delete_queue_pops + 1;
      m.delete_edges_scanned <- m.delete_edges_scanned + StableList.length succs);
    StableList.iter_with mark_deleted t succs
  done;
  if Invariants.enabled then
    Invariants.assert_deleted_nodes_closed ~current:t.current
      ~deleted_nodes:t.deleted_nodes ~old_successors:(old_successors t);

  (* Phase 3: apply root and edge mutations *)
  StableWave.iter_with roots
    (fun t k mv -> apply_root_mutation t k (Stable.to_linear_value mv))
    t;

  (* Apply edge updates by draining edge_change_queue. *)
  while not (StableQueue.is_empty t.edge_change_queue) do
    let src = StableQueue.pop t.edge_change_queue in
    let new_succs = find_succs t.new_successors_for_changed src in
    apply_edge_update t ~src ~new_successors:new_succs
  done;
  (* Rebuild edge_change_queue from new_successors_for_changed keys for
     use in expansion seeding below *)
  StableMap.iter_with rebuild_edge_change_queue t t.new_successors_for_changed;

  StableSet.iter_with remove_from_current t t.deleted_nodes;
  if Invariants.enabled then
    Invariants.assert_current_minus_deleted ~inv_scratch_a:t.inv_scratch_a
      ~pre_current:t.inv_pre_current ~current:t.current
      ~deleted_nodes:t.deleted_nodes;

  (* Phase 4: rederive *)
  StableQueue.clear t.rederive_queue;
  StableSet.clear t.rederive_pending;

  StableSet.iter_with
    (fun t k -> enqueue_rederive_if_needed_kv t k)
    t t.deleted_nodes;

  while not (StableQueue.is_empty t.rederive_queue) do
    let k = StableQueue.pop t.rederive_queue in
    if Metrics.enabled then m.rederive_queue_pops <- m.rederive_queue_pops + 1;
    StableSet.remove t.rederive_pending k;
    if
      StableSet.mem t.deleted_nodes k
      && (not (StableSet.mem t.current k))
      && is_supported t k
    then (
      StableSet.add t.current k;
      if Metrics.enabled then m.rederived_nodes <- m.rederived_nodes + 1;
      let r = StableMap.find_maybe t.edge_map k in
      if Maybe.is_some r then (
        let succs = succs_of_stable (Maybe.unsafe_get r) in
        if Metrics.enabled then
          m.rederive_edges_scanned <-
            m.rederive_edges_scanned + StableList.length succs;
        StableList.iter_with enqueue_rederive_if_needed t succs))
  done;
  if Invariants.enabled then
    Invariants.assert_no_supported_deleted_left ~deleted_nodes:t.deleted_nodes
      ~current:t.current ~supported:(is_supported t);

  (* Phase 5: expansion *)
  StableQueue.clear t.expansion_queue;
  StableSet.clear t.expansion_seen;

  (* Seed expansion from added roots *)
  while not (StableQueue.is_empty t.added_roots_queue) do
    add_live t (StableQueue.pop t.added_roots_queue)
  done;

  (* Seed expansion from edge changes with new edges *)
  while not (StableQueue.is_empty t.edge_change_queue) do
    let src = StableQueue.pop t.edge_change_queue in
    if StableSet.mem t.current src && StableSet.mem t.edge_has_new src then
      enqueue_expand t src
  done;

  while not (StableQueue.is_empty t.expansion_queue) do
    let k = StableQueue.pop t.expansion_queue in
    if Metrics.enabled then m.expansion_queue_pops <- m.expansion_queue_pops + 1;
    let r = StableMap.find_maybe t.edge_map k in
    if Maybe.is_some r then (
      let succs = succs_of_stable (Maybe.unsafe_get r) in
      if Metrics.enabled then
        m.expansion_edges_scanned <-
          m.expansion_edges_scanned + StableList.length succs;
      StableList.iter_with add_live t succs)
  done;
  StableSet.iter_with (fun t k -> emit_removal t k ()) t t.deleted_nodes;
  Invariants.assert_removal_output_matches ~inv_scratch_a:t.inv_scratch_a
    ~inv_scratch_b:t.inv_scratch_b ~output_wave:t.output_wave
    ~deleted_nodes:t.deleted_nodes ~current:t.current;
  if Invariants.enabled then
    Invariants.assert_final_fixpoint_and_delta ~inv_scratch_a:t.inv_scratch_a
      ~inv_scratch_b:t.inv_scratch_b ~visited:t.metrics.scratch_reachable ~t
      ~pre_current:t.inv_pre_current ~output_wave:t.output_wave;

  if Metrics.enabled then
    let full_node_work, full_edge_work =
      compute_reachable ~visited:t.metrics.scratch_reachable t
    in
    let init_count = StableWave.count roots in
    let edge_count = StableWave.count edges in
    let incr_node_work =
      init_count + edge_count + m.delete_queue_pops + m.rederive_queue_pops
      + m.expansion_queue_pops
    in
    let incr_edge_work =
      m.delete_edges_scanned + m.rederive_edges_scanned
      + m.expansion_edges_scanned
    in
    Metrics.update ~init_entries:init_count ~edge_entries:edge_count
      ~output_entries:(StableWave.count t.output_wave)
      ~deleted_nodes:(StableSet.cardinal t.deleted_nodes)
      ~rederived_nodes:m.rederived_nodes ~incr_node_work ~incr_edge_work
      ~full_node_work ~full_edge_work

let apply_wave t ~roots ~edges =
  StableWave.clear t.output_wave;
  apply_list t ~roots ~edges;
  ()
