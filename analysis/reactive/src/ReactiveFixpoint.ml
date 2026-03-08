(* Note on set representations:
   [pred_map] is represented by [ReactivePoolMapSet] because its semantics are
   exactly map-of-set with churn-safe remove+recycle behavior. *)

type 'k metrics_state = {
  mutable delete_queue_pops: int;
  mutable delete_edges_scanned: int;
  mutable rederive_queue_pops: int;
  mutable rederived_nodes: int;
  mutable rederive_edges_scanned: int;
  mutable expansion_queue_pops: int;
  mutable expansion_edges_scanned: int;
  scratch_reachable: 'k ReactiveSet.t;
}
(** Per-call metrics scratch state. Allocated once per fixpoint instance,
    mutable fields are reset and incremented in-place — zero allocation. *)

type 'k t = {
  current: 'k ReactiveSet.t;
  edge_map: ('k, 'k StableList.inner) ReactiveMap.t;
  pred_map: ('k, 'k) ReactivePoolMapSet.t;
  roots: 'k ReactiveSet.t;
  output_wave: ('k, unit Maybe.t) ReactiveWave.t;
  (* Scratch tables — allocated once, cleared per apply_list call *)
  deleted_nodes: 'k ReactiveSet.t;
  rederive_pending: 'k ReactiveSet.t;
  expansion_seen: 'k ReactiveSet.t;
  old_successors_for_changed: ('k, 'k StableList.inner) ReactiveMap.t;
  new_successors_for_changed: ('k, 'k StableList.inner) ReactiveMap.t;
  (* Scratch sets for analyze_edge_change / apply_edge_update *)
  scratch_set_a: 'k ReactiveSet.t;
  scratch_set_b: 'k ReactiveSet.t;
  edge_has_new: 'k ReactiveSet.t;
  (* Scratch queues *)
  delete_queue: 'k StableQueue.t;
  rederive_queue: 'k StableQueue.t;
  expansion_queue: 'k StableQueue.t;
  added_roots_queue: 'k StableQueue.t;
  edge_change_queue: 'k StableQueue.t;
  metrics: 'k metrics_state;
}

(* Standalone version for Invariants (no scratch sets available).
   Debug-only — allocates temporary Hashtbl. *)
let analyze_edge_change_has_new ~old_succs ~new_succs =
  if StableList.is_empty old_succs then not (StableList.is_empty new_succs)
  else if StableList.is_empty new_succs then false
  else
    let old_set = Hashtbl.create (StableList.length old_succs) in
    StableList.iter (fun k -> Hashtbl.replace old_set k ()) old_succs;
    StableList.exists (fun tgt -> not (Hashtbl.mem old_set tgt)) new_succs

let[@inline] stable_key k = Stable.unsafe_of_value k
let[@inline] enqueue q k = StableQueue.push q (stable_key k)

(* Full-reachability BFS into [visited]. Returns (node_work, edge_work).
   [visited] is cleared before use; zero allocation when [visited] is
   pre-allocated (e.g. Metrics scratch map). *)
let bfs_seed_root visited frontier _t k () =
  ReactiveSet.add visited (stable_key k);
  enqueue frontier k

let bfs_visit_succ visited frontier succ =
  if not (ReactiveSet.mem visited (stable_key succ)) then (
    ReactiveSet.add visited (stable_key succ);
    enqueue frontier succ)

let compute_reachable ~visited t =
  ReactiveSet.clear visited;
  let frontier = t.delete_queue in
  StableQueue.clear frontier;
  let node_work = ref 0 in
  let edge_work = ref 0 in
  ReactiveSet.iter_with
    (fun (visited, frontier) k ->
      bfs_seed_root visited frontier t (Stable.unsafe_to_value k) ())
    (visited, frontier) t.roots;
  while not (StableQueue.is_empty frontier) do
    let k = StableQueue.pop frontier in
    incr node_work;
    let r = ReactiveMap.find_maybe t.edge_map k in
    if Maybe.is_some r then (
      let succs = Maybe.unsafe_get r in
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

  (* Debug-only: copies a set into a Hashtbl for diffing.
     These allocations are acceptable since Invariants is opt-in debug code. *)
  let copy_set_to_hashtbl (s : 'k ReactiveSet.t) =
    let out = Hashtbl.create (ReactiveSet.cardinal s) in
    ReactiveSet.iter_with
      (fun out k -> Hashtbl.replace out (Stable.unsafe_to_value k) ())
      out s;
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
      let q_copy = StableQueue.create () in
      (* Drain and re-push to iterate without consuming *)
      let items = ref [] in
      while not (StableQueue.is_empty edge_change_queue) do
        let src = Stable.unsafe_to_value (StableQueue.pop edge_change_queue) in
        items := src :: !items;
        enqueue q_copy src
      done;
      (* Restore queue *)
      List.iter (fun src -> enqueue edge_change_queue src) (List.rev !items);
      StableQueue.destroy q_copy;
      (* Check each *)
      List.iter
        (fun src ->
          let r_old =
            ReactiveMap.find_maybe old_successors_for_changed (stable_key src)
          in
          let old_succs =
            if Maybe.is_some r_old then Maybe.unsafe_get r_old
            else StableList.empty ()
          in
          let r_new =
            ReactiveMap.find_maybe new_successors_for_changed (stable_key src)
          in
          let new_succs =
            if Maybe.is_some r_new then Maybe.unsafe_get r_new
            else StableList.empty ()
          in
          let expected_has_new =
            analyze_edge_change_has_new ~old_succs ~new_succs
          in
          let actual_has_new = ReactiveSet.mem edge_has_new (stable_key src) in
          assert_
            (expected_has_new = actual_has_new)
            "ReactiveFixpoint.apply invariant failed: inconsistent edge_has_new")
        !items)

  let assert_deleted_nodes_closed ~current ~deleted_nodes
      ~(old_successors : 'k -> 'k StableList.t) =
    if enabled then
      ReactiveSet.iter_with
        (fun () k ->
          let k = Stable.unsafe_to_value k in
          assert_
            (ReactiveSet.mem current (stable_key k))
            "ReactiveFixpoint.apply invariant failed: deleted node not in \
             current";
          StableList.iter
            (fun succ ->
              if ReactiveSet.mem current (stable_key succ) then
                assert_
                  (ReactiveSet.mem deleted_nodes (stable_key succ))
                  "ReactiveFixpoint.apply invariant failed: deleted closure \
                   broken")
            (old_successors k))
        () deleted_nodes

  let assert_no_supported_deleted_left ~deleted_nodes ~current ~supported =
    if enabled then
      ReactiveSet.iter_with
        (fun () k ->
          let k = Stable.unsafe_to_value k in
          if not (ReactiveSet.mem current (stable_key k)) then
            assert_
              (not (supported k))
              "ReactiveFixpoint.apply invariant failed: supported deleted node \
               left behind")
        () deleted_nodes

  let assert_current_minus_deleted ~pre_current ~current ~deleted_nodes =
    if enabled then (
      let expected = Hashtbl.copy pre_current in
      ReactiveSet.iter_with
        (fun expected k -> Hashtbl.remove expected (Stable.unsafe_to_value k))
        expected deleted_nodes;
      let current_ht = copy_set_to_hashtbl current in
      assert_
        (set_equal expected current_ht)
        "ReactiveFixpoint.apply invariant failed: current != pre_current minus \
         deleted")

  let assert_removal_output_matches ~output_entries ~deleted_nodes ~current =
    if enabled then (
      let expected = Hashtbl.create (ReactiveSet.cardinal deleted_nodes) in
      ReactiveSet.iter_with
        (fun expected k ->
          let k = Stable.unsafe_to_value k in
          if not (ReactiveSet.mem current (stable_key k)) then
            Hashtbl.replace expected k ())
        expected deleted_nodes;
      let actual = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, mv) ->
          if not (Maybe.is_some mv) then Hashtbl.replace actual k ())
        output_entries;
      assert_
        (set_equal expected actual)
        "ReactiveFixpoint.apply invariant failed: removal output mismatch")

  let assert_final_fixpoint_and_delta ~visited ~t ~pre_current ~output_entries =
    if enabled then (
      ignore (compute_reachable ~visited t);
      let reachable = copy_set_to_hashtbl visited in
      let current_ht = copy_set_to_hashtbl t.current in
      assert_
        (set_equal reachable current_ht)
        "ReactiveFixpoint.apply invariant failed: current is not a fixed-point \
         closure";

      let expected_adds = Hashtbl.create (ReactiveSet.cardinal t.current) in
      let expected_removes = Hashtbl.create (Hashtbl.length pre_current) in
      ReactiveSet.iter_with
        (fun expected_adds k ->
          let k = Stable.unsafe_to_value k in
          if not (Hashtbl.mem pre_current k) then
            Hashtbl.replace expected_adds k ())
        expected_adds t.current;
      Hashtbl.iter
        (fun k () ->
          if not (ReactiveSet.mem t.current (stable_key k)) then
            Hashtbl.replace expected_removes k ())
        pre_current;

      let actual_adds = Hashtbl.create (List.length output_entries) in
      let actual_removes = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, mv) ->
          if Maybe.is_some mv then Hashtbl.replace actual_adds k ()
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
             (ReactiveSet.cardinal t.current)
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
    current = ReactiveSet.create ();
    edge_map = ReactiveMap.create ();
    pred_map = ReactivePoolMapSet.create ~capacity:128;
    roots = ReactiveSet.create ();
    output_wave = ReactiveWave.create ~max_entries:max_nodes ();
    deleted_nodes = ReactiveSet.create ();
    rederive_pending = ReactiveSet.create ();
    expansion_seen = ReactiveSet.create ();
    old_successors_for_changed = ReactiveMap.create ();
    scratch_set_a = ReactiveSet.create ();
    scratch_set_b = ReactiveSet.create ();
    edge_has_new = ReactiveSet.create ();
    delete_queue = StableQueue.create ();
    rederive_queue = StableQueue.create ();
    expansion_queue = StableQueue.create ();
    added_roots_queue = StableQueue.create ();
    edge_change_queue = StableQueue.create ();
    new_successors_for_changed = ReactiveMap.create ();
    metrics =
      {
        delete_queue_pops = 0;
        delete_edges_scanned = 0;
        rederive_queue_pops = 0;
        rederived_nodes = 0;
        rederive_edges_scanned = 0;
        expansion_queue_pops = 0;
        expansion_edges_scanned = 0;
        scratch_reachable = ReactiveSet.create ();
      };
  }

let destroy t =
  ReactiveSet.destroy t.current;
  ReactiveMap.destroy t.edge_map;
  ReactiveSet.destroy t.roots;
  ReactiveSet.destroy t.deleted_nodes;
  ReactiveSet.destroy t.rederive_pending;
  ReactiveSet.destroy t.expansion_seen;
  ReactiveMap.destroy t.old_successors_for_changed;
  ReactiveMap.destroy t.new_successors_for_changed;
  ReactiveSet.destroy t.scratch_set_a;
  ReactiveSet.destroy t.scratch_set_b;
  ReactiveSet.destroy t.edge_has_new;
  StableQueue.destroy t.delete_queue;
  StableQueue.destroy t.rederive_queue;
  StableQueue.destroy t.expansion_queue;
  StableQueue.destroy t.added_roots_queue;
  StableQueue.destroy t.edge_change_queue;
  ReactiveSet.destroy t.metrics.scratch_reachable;
  ReactiveWave.destroy t.output_wave
let output_wave t = t.output_wave

type 'k root_wave = ('k, unit Maybe.t) ReactiveWave.t
type 'k edge_wave = ('k, 'k list Maybe.t) ReactiveWave.t
type 'k output_wave = ('k, unit Maybe.t) ReactiveWave.t
type 'k root_snapshot = ('k, unit) ReactiveWave.t
type 'k edge_snapshot = ('k, 'k list) ReactiveWave.t

let iter_current t f =
  ReactiveSet.iter_with (fun f k -> f (Stable.unsafe_to_value k) ()) f t.current

let get_current t k =
  if ReactiveSet.mem t.current (stable_key k) then Maybe.some () else Maybe.none

let current_length t = ReactiveSet.cardinal t.current

let recompute_current t = ignore (compute_reachable ~visited:t.current t)

let add_pred t ~target ~pred = ReactivePoolMapSet.add t.pred_map target pred

let remove_pred t ~target ~pred =
  ReactivePoolMapSet.remove_from_set_and_recycle_if_empty t.pred_map target pred

let has_live_pred_key t pred = ReactiveSet.mem t.current (stable_key pred)

let has_live_predecessor t k =
  let r = ReactivePoolMapSet.find_maybe t.pred_map k in
  if Maybe.is_some r then
    ReactiveHash.Set.exists_with has_live_pred_key t (Maybe.unsafe_get r)
  else false

let add_pred_for_src (t, src) target = add_pred t ~target ~pred:src
let remove_pred_for_src (t, src) target = remove_pred t ~target ~pred:src

let apply_edge_update t ~src ~new_successors =
  let r = ReactiveMap.find_maybe t.edge_map (stable_key src) in
  let old_successors =
    if Maybe.is_some r then Maybe.unsafe_get r else StableList.empty ()
  in
  if StableList.is_empty old_successors && StableList.is_empty new_successors
  then ReactiveMap.remove t.edge_map (stable_key src)
  else if StableList.is_empty old_successors then (
    StableList.iter_with add_pred_for_src (t, src) new_successors;
    ReactiveMap.replace t.edge_map (stable_key src) new_successors)
  else if StableList.is_empty new_successors then (
    StableList.iter_with remove_pred_for_src (t, src) old_successors;
    ReactiveMap.remove t.edge_map (stable_key src))
  else (
    ReactiveSet.clear t.scratch_set_a;
    ReactiveSet.clear t.scratch_set_b;
    StableList.iter
      (fun k -> ReactiveSet.add t.scratch_set_a (stable_key k))
      new_successors;
    StableList.iter
      (fun k -> ReactiveSet.add t.scratch_set_b (stable_key k))
      old_successors;

    StableList.iter_with
      (fun () target ->
        if not (ReactiveSet.mem t.scratch_set_a (stable_key target)) then
          remove_pred t ~target ~pred:src)
      () old_successors;

    StableList.iter_with
      (fun () target ->
        if not (ReactiveSet.mem t.scratch_set_b (stable_key target)) then
          add_pred t ~target ~pred:src)
      () new_successors;

    ReactiveMap.replace t.edge_map (stable_key src) new_successors)

let initialize t ~roots ~edges =
  ReactiveSet.clear t.roots;
  ReactiveMap.clear t.edge_map;
  ReactivePoolMapSet.clear t.pred_map;
  ReactiveWave.iter roots (fun k _ -> ReactiveSet.add t.roots k);
  ReactiveWave.iter edges (fun k successors ->
      apply_edge_update t ~src:(Stable.unsafe_to_value k)
        ~new_successors:(StableList.of_stable_list successors));
  recompute_current t

let is_supported t k =
  ReactiveSet.mem t.roots (stable_key k) || has_live_predecessor t k

let old_successors t k =
  let r = ReactiveMap.find_maybe t.old_successors_for_changed (stable_key k) in
  if Maybe.is_some r then Maybe.unsafe_get r
  else
    let r2 = ReactiveMap.find_maybe t.edge_map (stable_key k) in
    if Maybe.is_some r2 then Maybe.unsafe_get r2 else StableList.empty ()

let mark_deleted t k =
  if
    ReactiveSet.mem t.current (stable_key k)
    && not (ReactiveSet.mem t.deleted_nodes (stable_key k))
  then (
    ReactiveSet.add t.deleted_nodes (stable_key k);
    enqueue t.delete_queue k)

let enqueue_expand t k =
  if
    ReactiveSet.mem t.current (stable_key k)
    && not (ReactiveSet.mem t.expansion_seen (stable_key k))
  then (
    ReactiveSet.add t.expansion_seen (stable_key k);
    enqueue t.expansion_queue k)

let add_live t k =
  if not (ReactiveSet.mem t.current (stable_key k)) then (
    ReactiveSet.add t.current (stable_key k);
    if not (ReactiveSet.mem t.deleted_nodes (stable_key k)) then
      ReactiveWave.push t.output_wave (Stable.unsafe_of_value k)
        (Maybe.maybe_unit_to_stable (Maybe.some ()));
    enqueue_expand t k)

let enqueue_rederive_if_needed t k =
  if
    ReactiveSet.mem t.deleted_nodes (stable_key k)
    && (not (ReactiveSet.mem t.current (stable_key k)))
    && (not (ReactiveSet.mem t.rederive_pending (stable_key k)))
    && is_supported t k
  then (
    ReactiveSet.add t.rederive_pending (stable_key k);
    enqueue t.rederive_queue k)

let scan_root_entry t k mv =
  let had_root = ReactiveSet.mem t.roots (stable_key k) in
  if Maybe.is_some mv then (if not had_root then enqueue t.added_roots_queue k)
  else if had_root then mark_deleted t k

let set_add_k set k = ReactiveSet.add set (stable_key k)

let mark_deleted_if_absent (t, set) k =
  if not (ReactiveSet.mem set (stable_key k)) then mark_deleted t k

let not_in_set set k = not (ReactiveSet.mem set (stable_key k))

let mark_deleted_unless_in_set t set xs =
  StableList.iter_with mark_deleted_if_absent (t, set) xs

let exists_not_in_set set xs = StableList.exists_with not_in_set set xs

let scan_edge_entry t src mv =
  let r = ReactiveMap.find_maybe t.edge_map (stable_key src) in
  let old_succs =
    if Maybe.is_some r then Maybe.unsafe_get r else StableList.empty ()
  in
  let new_succs =
    if Maybe.is_some mv then Maybe.unsafe_get mv else StableList.empty ()
  in
  ReactiveMap.replace t.old_successors_for_changed (stable_key src) old_succs;
  ReactiveMap.replace t.new_successors_for_changed (stable_key src) new_succs;
  enqueue t.edge_change_queue src;
  let src_is_live = ReactiveSet.mem t.current (stable_key src) in
  match (old_succs, new_succs) with
  | _ when StableList.is_empty old_succs && StableList.is_empty new_succs -> ()
  | _ when StableList.is_empty old_succs ->
    ReactiveSet.add t.edge_has_new (stable_key src)
  | _ when StableList.is_empty new_succs ->
    if src_is_live then StableList.iter_with mark_deleted t old_succs
  | _ ->
    ReactiveSet.clear t.scratch_set_a;
    ReactiveSet.clear t.scratch_set_b;
    StableList.iter_with set_add_k t.scratch_set_a new_succs;
    StableList.iter_with set_add_k t.scratch_set_b old_succs;
    if src_is_live then mark_deleted_unless_in_set t t.scratch_set_a old_succs;
    if exists_not_in_set t.scratch_set_b new_succs then
      ReactiveSet.add t.edge_has_new (stable_key src)

let apply_root_mutation t k mv =
  if Maybe.is_some mv then ReactiveSet.add t.roots (stable_key k)
  else ReactiveSet.remove t.roots (stable_key k)

let emit_removal t k () =
  if not (ReactiveSet.mem t.current (stable_key k)) then
    ReactiveWave.push t.output_wave (Stable.unsafe_of_value k) Maybe.none_stable

let rebuild_edge_change_queue t src _succs =
  StableQueue.push t.edge_change_queue src

let remove_from_current t k = ReactiveSet.remove t.current k

let enqueue_rederive_if_needed_kv t k = enqueue_rederive_if_needed t k

let apply_list t ~roots ~edges =
  let pre_current =
    if Invariants.enabled then Some (Invariants.copy_set_to_hashtbl t.current)
    else None
  in
  (* Clear all scratch state up front *)
  ReactiveSet.clear t.deleted_nodes;
  StableQueue.clear t.delete_queue;
  StableQueue.clear t.added_roots_queue;
  StableQueue.clear t.edge_change_queue;
  ReactiveMap.clear t.old_successors_for_changed;
  ReactiveMap.clear t.new_successors_for_changed;
  ReactiveSet.clear t.edge_has_new;
  let m = t.metrics in
  Metrics.reset_per_call m;

  (* Phase 1a: scan init entries — seed delete queue for removed roots,
     buffer added roots for later expansion *)
  ReactiveWave.iter_with roots
    (fun t k mv ->
      scan_root_entry t (Stable.unsafe_to_value k) (Stable.unsafe_to_value mv))
    t;

  (* Phase 1b: scan edge entries — seed delete queue for removed targets,
     store new_succs and has_new_edge for later phases *)
  ReactiveWave.iter_with edges
    (fun t src mv ->
      let mv = Stable.unsafe_to_value mv in
      let mv =
        if Maybe.is_some mv then
          Maybe.some (StableList.unsafe_of_list (Maybe.unsafe_get mv))
        else Maybe.none
      in
      scan_edge_entry t (Stable.unsafe_to_value src) mv)
    t;

  Invariants.assert_edge_has_new_consistent
    ~edge_change_queue:t.edge_change_queue
    ~old_successors_for_changed:t.old_successors_for_changed
    ~new_successors_for_changed:t.new_successors_for_changed
    ~edge_has_new:t.edge_has_new;

  (* Phase 2: delete BFS *)
  while not (StableQueue.is_empty t.delete_queue) do
    let k = Stable.unsafe_to_value (StableQueue.pop t.delete_queue) in
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
  ReactiveWave.iter_with roots
    (fun t k mv ->
      apply_root_mutation t (Stable.unsafe_to_value k)
        (Stable.unsafe_to_value mv))
    t;

  (* Apply edge updates by draining edge_change_queue. *)
  while not (StableQueue.is_empty t.edge_change_queue) do
    let src = StableQueue.pop t.edge_change_queue in
    let r = ReactiveMap.find_maybe t.new_successors_for_changed src in
    let new_succs =
      if Maybe.is_some r then Maybe.unsafe_get r else StableList.empty ()
    in
    apply_edge_update t
      ~src:(Stable.unsafe_to_value src)
      ~new_successors:new_succs
  done;
  (* Rebuild edge_change_queue from new_successors_for_changed keys for
     use in expansion seeding below *)
  ReactiveMap.iter_with rebuild_edge_change_queue t t.new_successors_for_changed;

  ReactiveSet.iter_with remove_from_current t t.deleted_nodes;
  (match pre_current with
  | Some pre ->
    Invariants.assert_current_minus_deleted ~pre_current:pre ~current:t.current
      ~deleted_nodes:t.deleted_nodes
  | None -> ());

  (* Phase 4: rederive *)
  StableQueue.clear t.rederive_queue;
  ReactiveSet.clear t.rederive_pending;

  ReactiveSet.iter_with
    (fun t k -> enqueue_rederive_if_needed_kv t (Stable.unsafe_to_value k))
    t t.deleted_nodes;

  while not (StableQueue.is_empty t.rederive_queue) do
    let k = StableQueue.pop t.rederive_queue in
    if Metrics.enabled then m.rederive_queue_pops <- m.rederive_queue_pops + 1;
    ReactiveSet.remove t.rederive_pending k;
    if
      ReactiveSet.mem t.deleted_nodes k
      && (not (ReactiveSet.mem t.current k))
      && is_supported t (Stable.unsafe_to_value k)
    then (
      ReactiveSet.add t.current k;
      if Metrics.enabled then m.rederived_nodes <- m.rederived_nodes + 1;
      let r = ReactiveMap.find_maybe t.edge_map k in
      if Maybe.is_some r then (
        let succs = Maybe.unsafe_get r in
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
  ReactiveSet.clear t.expansion_seen;

  (* Seed expansion from added roots *)
  while not (StableQueue.is_empty t.added_roots_queue) do
    add_live t (Stable.unsafe_to_value (StableQueue.pop t.added_roots_queue))
  done;

  (* Seed expansion from edge changes with new edges *)
  while not (StableQueue.is_empty t.edge_change_queue) do
    let src = StableQueue.pop t.edge_change_queue in
    if ReactiveSet.mem t.current src && ReactiveSet.mem t.edge_has_new src then
      enqueue_expand t (Stable.unsafe_to_value src)
  done;

  while not (StableQueue.is_empty t.expansion_queue) do
    let k = StableQueue.pop t.expansion_queue in
    if Metrics.enabled then m.expansion_queue_pops <- m.expansion_queue_pops + 1;
    let r = ReactiveMap.find_maybe t.edge_map k in
    if Maybe.is_some r then (
      let succs = Maybe.unsafe_get r in
      if Metrics.enabled then
        m.expansion_edges_scanned <-
          m.expansion_edges_scanned + StableList.length succs;
      StableList.iter_with add_live t succs)
  done;
  ReactiveSet.iter_with
    (fun t k -> emit_removal t (Stable.unsafe_to_value k) ())
    t t.deleted_nodes;
  let output_entries_list =
    if Invariants.enabled then (
      let entries = ref [] in
      ReactiveWave.iter t.output_wave (fun k v_opt ->
          entries :=
            (Stable.unsafe_to_value k, Stable.unsafe_to_value v_opt) :: !entries);
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
      ~deleted_nodes:(ReactiveSet.cardinal t.deleted_nodes)
      ~rederived_nodes:m.rederived_nodes ~incr_node_work ~incr_edge_work
      ~full_node_work ~full_edge_work

let apply_wave t ~roots ~edges =
  ReactiveWave.clear t.output_wave;
  apply_list t ~roots ~edges;
  ()
