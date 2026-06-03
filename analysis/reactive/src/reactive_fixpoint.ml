type 'k t = {
  current: ('k, unit) Hashtbl.t;
  edge_map: ('k, 'k list) Hashtbl.t;
  pred_map: ('k, ('k, unit) Hashtbl.t) Hashtbl.t;
  roots: ('k, unit) Hashtbl.t;
}

type 'k edge_change = {
  src: 'k;
  old_succs: 'k list;
  new_succs: 'k list;
  removed_targets: 'k list;
  has_new_edge: bool;
}

let analyze_edge_change ~old_succs ~new_succs =
  match (old_succs, new_succs) with
  | [], [] -> ([], false)
  | [], _ -> ([], true)
  | _, [] -> (old_succs, false)
  | _, _ ->
    let new_set = Hashtbl.create (List.length new_succs) in
    List.iter (fun k -> Hashtbl.replace new_set k ()) new_succs;
    let old_set = Hashtbl.create (List.length old_succs) in
    List.iter (fun k -> Hashtbl.replace old_set k ()) old_succs;
    let removed_targets =
      List.filter (fun target -> not (Hashtbl.mem new_set target)) old_succs
    in
    let has_new_edge =
      List.exists (fun tgt -> not (Hashtbl.mem old_set tgt)) new_succs
    in
    (removed_targets, has_new_edge)

let compute_reachable_from_roots_with_work t =
  let new_current = Hashtbl.create (Hashtbl.length t.current) in
  let frontier = Queue.create () in
  let nodes_visited = ref 0 in
  let edges_scanned = ref 0 in

  Hashtbl.iter
    (fun k () ->
      Hashtbl.replace new_current k ();
      incr nodes_visited;
      Queue.add k frontier)
    t.roots;

  while not (Queue.is_empty frontier) do
    let k = Queue.pop frontier in
    match Hashtbl.find_opt t.edge_map k with
    | None -> ()
    | Some successors ->
      edges_scanned := !edges_scanned + List.length successors;
      List.iter
        (fun succ ->
          if not (Hashtbl.mem new_current succ) then (
            Hashtbl.replace new_current succ ();
            incr nodes_visited;
            Queue.add succ frontier))
        successors
  done;
  (new_current, !nodes_visited, !edges_scanned)

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

  let copy_set tbl =
    let out = Hashtbl.create (Hashtbl.length tbl) in
    Hashtbl.iter (fun k () -> Hashtbl.replace out k ()) tbl;
    out

  let set_equal a b =
    Hashtbl.length a = Hashtbl.length b
    &&
    let ok = ref true in
    Hashtbl.iter (fun k () -> if not (Hashtbl.mem b k) then ok := false) a;
    !ok

  let assert_edge_changes_consistent edge_changes =
    (* Invariant: for each change, [removed_targets = old_succs \\ new_succs]
       and [has_new_edge <=> (new_succs \\ old_succs <> empty)]. *)
    if enabled then
      List.iter
        (fun ({old_succs; new_succs; removed_targets; has_new_edge; _} :
               _ edge_change) ->
          let expected_removed, expected_has_new =
            analyze_edge_change ~old_succs ~new_succs
          in
          assert_
            (removed_targets = expected_removed
            && has_new_edge = expected_has_new)
            "ReactiveFixpoint.apply invariant failed: inconsistent edge_change")
        edge_changes

  let assert_deleted_nodes_closed ~current ~deleted_nodes ~old_successors =
    (* Invariant: [deleted_nodes ⊆ current] and
       [k in deleted_nodes => old_successors(k) ∩ current ⊆ deleted_nodes]. *)
    if enabled then
      Hashtbl.iter
        (fun k () ->
          assert_ (Hashtbl.mem current k)
            "ReactiveFixpoint.apply invariant failed: deleted node not in \
             current";
          List.iter
            (fun succ ->
              if Hashtbl.mem current succ then
                assert_
                  (Hashtbl.mem deleted_nodes succ)
                  "ReactiveFixpoint.apply invariant failed: deleted closure \
                   broken")
            (old_successors k))
        deleted_nodes

  let assert_current_minus_deleted ~pre_current ~current ~deleted_nodes =
    (* Invariant: [current = pre_current \\ deleted_nodes]. *)
    if enabled then (
      let expected = copy_set pre_current in
      Hashtbl.iter (fun k () -> Hashtbl.remove expected k) deleted_nodes;
      assert_
        (set_equal expected current)
        "ReactiveFixpoint.apply invariant failed: current != pre_current minus \
         deleted")

  let assert_no_supported_deleted_left ~deleted_nodes ~current ~supported =
    (* Invariant: [k in deleted_nodes \\ current => not (supported k)]. *)
    if enabled then
      Hashtbl.iter
        (fun k () ->
          if not (Hashtbl.mem current k) then
            assert_
              (not (supported k))
              "ReactiveFixpoint.apply invariant failed: supported deleted node \
               left behind")
        deleted_nodes

  let assert_removal_output_matches ~output_entries ~deleted_nodes ~current =
    (* Invariant: [removal_keys(output_entries) = deleted_nodes \\ current]. *)
    if enabled then (
      let expected = Hashtbl.create (Hashtbl.length deleted_nodes) in
      Hashtbl.iter
        (fun k () ->
          if not (Hashtbl.mem current k) then Hashtbl.replace expected k ())
        deleted_nodes;
      let actual = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, v_opt) -> if v_opt = None then Hashtbl.replace actual k ())
        output_entries;
      assert_
        (set_equal expected actual)
        "ReactiveFixpoint.apply invariant failed: removal output mismatch")

  let assert_final_fixpoint_and_delta ~compute_reachable ~t ~pre_current
      ~output_entries =
    (* Invariant: [t.current = Reach(t.roots, t.edge_map)] and
       [adds(output_entries) = t.current \\ pre_current] and
       [removes(output_entries) = pre_current \\ t.current]. *)
    if enabled then (
      let reachable = compute_reachable t in
      assert_
        (set_equal reachable t.current)
        "ReactiveFixpoint.apply invariant failed: current is not a fixed-point \
         closure";

      let expected_adds = Hashtbl.create (Hashtbl.length t.current) in
      let expected_removes = Hashtbl.create (Hashtbl.length pre_current) in
      Hashtbl.iter
        (fun k () ->
          if not (Hashtbl.mem pre_current k) then
            Hashtbl.replace expected_adds k ())
        t.current;
      Hashtbl.iter
        (fun k () ->
          if not (Hashtbl.mem t.current k) then
            Hashtbl.replace expected_removes k ())
        pre_current;

      let actual_adds = Hashtbl.create (List.length output_entries) in
      let actual_removes = Hashtbl.create (List.length output_entries) in
      List.iter
        (fun (k, v_opt) ->
          match v_opt with
          | Some () -> Hashtbl.replace actual_adds k ()
          | None -> Hashtbl.replace actual_removes k ())
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
             (Hashtbl.length t.current)
             (List.length output_entries)
             (Hashtbl.length expected_adds)
             (Hashtbl.length actual_adds)
             (Hashtbl.length expected_removes)
             (Hashtbl.length actual_removes)))
end

let create () =
  {
    current = Hashtbl.create 256;
    edge_map = Hashtbl.create 256;
    pred_map = Hashtbl.create 256;
    roots = Hashtbl.create 64;
  }

let iter_current t f = Hashtbl.iter f t.current
let get_current t k = Hashtbl.find_opt t.current k
let current_length t = Hashtbl.length t.current

let compute_reachable_from_roots t =
  let reachable, _nodes, _edges = compute_reachable_from_roots_with_work t in
  reachable

let replace_current_with t new_current =
  Hashtbl.reset t.current;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.current k v) new_current

let add_pred t ~target ~pred =
  let preds =
    match Hashtbl.find_opt t.pred_map target with
    | Some ps -> ps
    | None ->
      let ps = Hashtbl.create 4 in
      Hashtbl.replace t.pred_map target ps;
      ps
  in
  Hashtbl.replace preds pred ()

let remove_pred t ~target ~pred =
  match Hashtbl.find_opt t.pred_map target with
  | None -> ()
  | Some preds ->
    Hashtbl.remove preds pred;
    if Hashtbl.length preds = 0 then Hashtbl.remove t.pred_map target

exception Found_live_pred

let has_live_predecessor t k =
  match Hashtbl.find_opt t.pred_map k with
  | None -> false
  | Some preds -> (
    try
      Hashtbl.iter
        (fun pred () ->
          if Hashtbl.mem t.current pred then raise Found_live_pred)
        preds;
      false
    with Found_live_pred -> true)

let apply_edge_update t ~src ~new_successors =
  let old_successors =
    match Hashtbl.find_opt t.edge_map src with
    | Some succs -> succs
    | None -> []
  in
  match (old_successors, new_successors) with
  | [], [] -> Hashtbl.remove t.edge_map src
  | [], _ ->
    List.iter (fun target -> add_pred t ~target ~pred:src) new_successors;
    Hashtbl.replace t.edge_map src new_successors
  | _, [] ->
    List.iter (fun target -> remove_pred t ~target ~pred:src) old_successors;
    Hashtbl.remove t.edge_map src
  | _, _ ->
    let new_set = Hashtbl.create (List.length new_successors) in
    List.iter (fun k -> Hashtbl.replace new_set k ()) new_successors;

    let old_set = Hashtbl.create (List.length old_successors) in
    List.iter (fun k -> Hashtbl.replace old_set k ()) old_successors;

    List.iter
      (fun target ->
        if not (Hashtbl.mem new_set target) then remove_pred t ~target ~pred:src)
      old_successors;

    List.iter
      (fun target ->
        if not (Hashtbl.mem old_set target) then add_pred t ~target ~pred:src)
      new_successors;

    Hashtbl.replace t.edge_map src new_successors

let initialize t ~roots_iter ~edges_iter =
  Hashtbl.reset t.roots;
  Hashtbl.reset t.edge_map;
  Hashtbl.reset t.pred_map;
  roots_iter (fun k () -> Hashtbl.replace t.roots k ());
  edges_iter (fun k successors ->
      apply_edge_update t ~src:k ~new_successors:successors);
  replace_current_with t (compute_reachable_from_roots t)

let apply t ~init_entries ~edge_entries =
  let pre_current =
    if Invariants.enabled then Some (Invariants.copy_set t.current) else None
  in
  let output_entries = ref [] in
  let removed_roots = ref [] in
  let added_roots = ref [] in
  let edge_changes : 'k edge_change list ref = ref [] in

  List.iter
    (fun (k, v_opt) ->
      let had_root = Hashtbl.mem t.roots k in
      match v_opt with
      | Some () -> if not had_root then added_roots := k :: !added_roots
      | None -> if had_root then removed_roots := k :: !removed_roots)
    init_entries;

  let old_successors_for_changed : ('k, 'k list) Hashtbl.t =
    Hashtbl.create 64
  in

  List.iter
    (fun (src, v_opt) ->
      let old_succs =
        match Hashtbl.find_opt t.edge_map src with
        | Some succs -> succs
        | None -> []
      in
      let new_succs =
        match v_opt with
        | Some succs -> succs
        | None -> []
      in
      let removed_targets, has_new_edge =
        analyze_edge_change ~old_succs ~new_succs
      in
      Hashtbl.replace old_successors_for_changed src old_succs;
      edge_changes :=
        {src; old_succs; new_succs; removed_targets; has_new_edge}
        :: !edge_changes)
    edge_entries;
  Invariants.assert_edge_changes_consistent !edge_changes;

  let deleted_nodes : ('k, unit) Hashtbl.t = Hashtbl.create 128 in
  let delete_queue = Queue.create () in
  let delete_queue_pops = ref 0 in
  let delete_edges_scanned = ref 0 in

  let mark_deleted k =
    if Hashtbl.mem t.current k && not (Hashtbl.mem deleted_nodes k) then (
      Hashtbl.replace deleted_nodes k ();
      Queue.add k delete_queue)
  in

  List.iter mark_deleted !removed_roots;

  List.iter
    (fun {src; removed_targets; _} ->
      if Hashtbl.mem t.current src then
        List.iter (fun target -> mark_deleted target) removed_targets)
    !edge_changes;

  let old_successors k =
    match Hashtbl.find_opt old_successors_for_changed k with
    | Some succs -> succs
    | None -> (
      match Hashtbl.find_opt t.edge_map k with
      | Some succs -> succs
      | None -> [])
  in

  while not (Queue.is_empty delete_queue) do
    let k = Queue.pop delete_queue in
    incr delete_queue_pops;
    let succs = old_successors k in
    delete_edges_scanned := !delete_edges_scanned + List.length succs;
    List.iter mark_deleted succs
  done;
  Invariants.assert_deleted_nodes_closed ~current:t.current ~deleted_nodes
    ~old_successors;

  List.iter
    (fun (k, v_opt) ->
      match v_opt with
      | Some () -> Hashtbl.replace t.roots k ()
      | None -> Hashtbl.remove t.roots k)
    init_entries;

  List.iter
    (fun {src; new_succs; _} ->
      apply_edge_update t ~src ~new_successors:new_succs)
    !edge_changes;

  Hashtbl.iter (fun k () -> Hashtbl.remove t.current k) deleted_nodes;
  (match pre_current with
  | Some pre ->
    Invariants.assert_current_minus_deleted ~pre_current:pre ~current:t.current
      ~deleted_nodes
  | None -> ());

  let supported k = Hashtbl.mem t.roots k || has_live_predecessor t k in

  let rederive_queue = Queue.create () in
  let rederive_pending : ('k, unit) Hashtbl.t = Hashtbl.create 128 in
  let rederive_queue_pops = ref 0 in
  let rederived_nodes = ref 0 in
  let rederive_edges_scanned = ref 0 in

  let enqueue_rederive_if_needed k =
    if
      Hashtbl.mem deleted_nodes k
      && (not (Hashtbl.mem t.current k))
      && (not (Hashtbl.mem rederive_pending k))
      && supported k
    then (
      Hashtbl.replace rederive_pending k ();
      Queue.add k rederive_queue)
  in

  Hashtbl.iter (fun k () -> enqueue_rederive_if_needed k) deleted_nodes;

  while not (Queue.is_empty rederive_queue) do
    let k = Queue.pop rederive_queue in
    incr rederive_queue_pops;
    Hashtbl.remove rederive_pending k;
    if
      Hashtbl.mem deleted_nodes k
      && (not (Hashtbl.mem t.current k))
      && supported k
    then (
      Hashtbl.replace t.current k ();
      incr rederived_nodes;
      match Hashtbl.find_opt t.edge_map k with
      | None -> ()
      | Some succs ->
        rederive_edges_scanned := !rederive_edges_scanned + List.length succs;
        List.iter enqueue_rederive_if_needed succs)
  done;
  Invariants.assert_no_supported_deleted_left ~deleted_nodes ~current:t.current
    ~supported;

  let expansion_queue = Queue.create () in
  let expansion_seen : ('k, unit) Hashtbl.t = Hashtbl.create 128 in
  let expansion_queue_pops = ref 0 in
  let expansion_edges_scanned = ref 0 in

  let enqueue_expand k =
    if Hashtbl.mem t.current k && not (Hashtbl.mem expansion_seen k) then (
      Hashtbl.replace expansion_seen k ();
      Queue.add k expansion_queue)
  in

  let add_live k =
    if not (Hashtbl.mem t.current k) then (
      Hashtbl.replace t.current k ();
      (* If a node was tentatively deleted in this wave and later rederived,
         suppress add output so downstream sees no net change for that key. *)
      if not (Hashtbl.mem deleted_nodes k) then
        output_entries := (k, Some ()) :: !output_entries;
      enqueue_expand k)
  in

  List.iter add_live !added_roots;

  List.iter
    (fun {src; has_new_edge; _} ->
      if Hashtbl.mem t.current src && has_new_edge then enqueue_expand src)
    !edge_changes;

  while not (Queue.is_empty expansion_queue) do
    let k = Queue.pop expansion_queue in
    incr expansion_queue_pops;
    match Hashtbl.find_opt t.edge_map k with
    | None -> ()
    | Some successors ->
      expansion_edges_scanned :=
        !expansion_edges_scanned + List.length successors;
      List.iter add_live successors
  done;
  Hashtbl.iter
    (fun k () ->
      if not (Hashtbl.mem t.current k) then
        output_entries := (k, None) :: !output_entries)
    deleted_nodes;
  Invariants.assert_removal_output_matches ~output_entries:!output_entries
    ~deleted_nodes ~current:t.current;
  (match pre_current with
  | Some pre ->
    Invariants.assert_final_fixpoint_and_delta
      ~compute_reachable:compute_reachable_from_roots ~t ~pre_current:pre
      ~output_entries:!output_entries
  | None -> ());

  (if Metrics.enabled then
     (* Metrics mode intentionally computes a full closure baseline to compare
       incremental work against full recomputation. Keep this opt-in only. *)
     let _full_reachable, full_node_work, full_edge_work =
       compute_reachable_from_roots_with_work t
     in
     let incr_node_work =
       List.length init_entries + List.length edge_entries + !delete_queue_pops
       + !rederive_queue_pops + !expansion_queue_pops
     in
     let incr_edge_work =
       !delete_edges_scanned + !rederive_edges_scanned
       + !expansion_edges_scanned
     in
     Metrics.update ~init_entries:(List.length init_entries)
       ~edge_entries:(List.length edge_entries)
       ~output_entries:(List.length !output_entries)
       ~deleted_nodes:(Hashtbl.length deleted_nodes)
       ~rederived_nodes:!rederived_nodes ~incr_node_work ~incr_edge_work
       ~full_node_work ~full_edge_work);

  !output_entries
