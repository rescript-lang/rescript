(** Allocation measurement for reactive combinators.

    Calls combinator internals directly (bypassing the source/emit
    layer) to measure only the combinator's own allocations. *)

open TestHelpers

let words_since = AllocMeasure.words_since

let off = Allocator.unsafe_to_offheap
let off_int = Allocator.int_to_offheap
let off_unit = Allocator.unit_to_offheap
let off_maybe_int = Maybe.maybe_int_to_offheap
let off_maybe_unit = Maybe.maybe_unit_to_offheap

let unsafe_wave_push wave k v = ReactiveWave.push wave (off k) (off v)

let print_offheap_usage () =
  let blocks = Allocator.live_block_count () in
  let slots = Allocator.live_block_capacity_slots () in
  let bytes = slots * Allocator.slot_size_bytes in
  Printf.printf "  offheap: blocks=%d slots=%d bytes=%d\n" blocks slots bytes

let reset_offheap_state () =
  Reactive.reset ();
  Allocator.reset ();
  assert (Allocator.live_block_count () = 0);
  assert (Allocator.live_block_capacity_slots () = 0)

let print_offheap_snapshot label =
  let blocks = Allocator.live_block_count () in
  let slots = Allocator.live_block_capacity_slots () in
  let bytes = slots * Allocator.slot_size_bytes in
  Printf.printf "    %s: blocks=%d slots=%d bytes=%d\n" label blocks slots bytes

(* ---- Fixpoint allocation ---- *)

let test_fixpoint_alloc_n n =
  let edge_values = Array.init (max 0 (n - 1)) (fun i -> [i + 1]) in
  Gc.full_major ();
  let root_snap = ReactiveWave.create ~max_entries:1 () in
  let edge_snap = ReactiveWave.create ~max_entries:n () in
  let remove_root = ReactiveWave.create ~max_entries:1 () in
  let add_root = ReactiveWave.create ~max_entries:1 () in
  let no_edges = ReactiveWave.create ~max_entries:1 () in
  let state = ReactiveFixpoint.create ~max_nodes:n ~max_edges:n in

  (* Chain graph: 0 -> 1 -> 2 -> ... -> n-1 *)
  ReactiveWave.push root_snap (off_int 0) (off_unit ());
  for i = 0 to n - 2 do
    ReactiveWave.push edge_snap (off_int i)
      (Allocator.to_offheap edge_values.(i))
  done;
  ReactiveFixpoint.initialize state ~roots:root_snap ~edges:edge_snap;
  assert (ReactiveFixpoint.current_length state = n);

  (* Pre-build waves once *)
  ReactiveWave.push remove_root (off_int 0) Maybe.none_offheap;
  ReactiveWave.push add_root (off_int 0) (off_maybe_unit (Maybe.some ()));

  (* Warmup *)
  for _ = 1 to 5 do
    ReactiveFixpoint.apply_wave state ~roots:remove_root ~edges:no_edges;
    ReactiveFixpoint.apply_wave state ~roots:add_root ~edges:no_edges
  done;
  assert (ReactiveFixpoint.current_length state = n);

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    ReactiveFixpoint.apply_wave state ~roots:remove_root ~edges:no_edges;
    ReactiveFixpoint.apply_wave state ~roots:add_root ~edges:no_edges
  done;
  assert (ReactiveFixpoint.current_length state = n);
  ReactiveWave.destroy root_snap;
  ReactiveWave.destroy edge_snap;
  ReactiveWave.destroy remove_root;
  ReactiveWave.destroy add_root;
  ReactiveWave.destroy no_edges;
  ReactiveFixpoint.destroy state;
  words_since () / iters

let test_fixpoint_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: fixpoint allocation ===\n";
  List.iter
    (fun n ->
      let words = test_fixpoint_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- FlatMap allocation ---- *)

let test_flatmap_alloc_n n =
  let state =
    ReactiveFlatMap.create ~f:(fun k v emit -> emit k v) ~merge:(fun _l r -> r)
  in

  (* Populate: n entries *)
  for i = 0 to n - 1 do
    ReactiveFlatMap.push state (off_int i) (off_maybe_int (Maybe.some i))
  done;
  ignore (ReactiveFlatMap.process state);
  assert (ReactiveFlatMap.target_length state = n);

  (* Warmup: toggle all entries (remove all, re-add all) *)
  for _ = 1 to 5 do
    for i = 0 to n - 1 do
      ReactiveFlatMap.push state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveFlatMap.process state);
    assert (ReactiveFlatMap.target_length state = 0);
    for i = 0 to n - 1 do
      ReactiveFlatMap.push state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveFlatMap.process state);
    assert (ReactiveFlatMap.target_length state = n)
  done;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    for i = 0 to n - 1 do
      ReactiveFlatMap.push state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveFlatMap.process state);
    for i = 0 to n - 1 do
      ReactiveFlatMap.push state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveFlatMap.process state)
  done;
  assert (ReactiveFlatMap.target_length state = n);
  ReactiveFlatMap.destroy state;
  words_since () / iters

let test_flatmap_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: flatMap allocation ===\n";
  List.iter
    (fun n ->
      let words = test_flatmap_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Union allocation ---- *)

let test_union_alloc_n n =
  let state = ReactiveUnion.create ~merge:(fun _l r -> r) in

  (* Populate: n entries on the left side *)
  for i = 0 to n - 1 do
    ReactiveUnion.push_left state (off_int i) (off_maybe_int (Maybe.some i))
  done;
  ignore (ReactiveUnion.process state);
  assert (ReactiveUnion.target_length state = n);

  (* Warmup: toggle all entries (remove all, re-add all) *)
  for _ = 1 to 5 do
    for i = 0 to n - 1 do
      ReactiveUnion.push_left state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveUnion.process state);
    assert (ReactiveUnion.target_length state = 0);
    for i = 0 to n - 1 do
      ReactiveUnion.push_left state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveUnion.process state);
    assert (ReactiveUnion.target_length state = n)
  done;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    for i = 0 to n - 1 do
      ReactiveUnion.push_left state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveUnion.process state);
    for i = 0 to n - 1 do
      ReactiveUnion.push_left state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveUnion.process state)
  done;
  assert (ReactiveUnion.target_length state = n);
  ReactiveUnion.destroy state;
  words_since () / iters

let test_union_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: union allocation ===\n";
  List.iter
    (fun n ->
      let words = test_union_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Join allocation ---- *)

let test_join_alloc_n n =
  let right_tbl = ReactiveHash.Map.create () in
  let state =
    ReactiveJoin.create
      ~key_of:(fun k _v -> k)
      ~f:(fun k v right_mb emit ->
        if Maybe.is_some right_mb then emit k (v + Maybe.unsafe_get right_mb))
      ~merge:(fun _l r -> r)
      ~right_get:(ReactiveHash.Map.find_maybe right_tbl)
  in

  (* Populate: n entries on the right, n on the left *)
  for i = 0 to n - 1 do
    ReactiveHash.Map.replace right_tbl i (i * 10)
  done;
  for i = 0 to n - 1 do
    ReactiveJoin.push_left state (off_int i) (off_maybe_int (Maybe.some i))
  done;
  ignore (ReactiveJoin.process state);
  assert (ReactiveJoin.target_length state = n);

  (* Warmup: toggle all left entries *)
  for _ = 1 to 5 do
    for i = 0 to n - 1 do
      ReactiveJoin.push_left state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveJoin.process state);
    assert (ReactiveJoin.target_length state = 0);
    for i = 0 to n - 1 do
      ReactiveJoin.push_left state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveJoin.process state);
    assert (ReactiveJoin.target_length state = n)
  done;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    for i = 0 to n - 1 do
      ReactiveJoin.push_left state (off i) Maybe.none_offheap
    done;
    ignore (ReactiveJoin.process state);
    for i = 0 to n - 1 do
      ReactiveJoin.push_left state (off_int i) (off_maybe_int (Maybe.some i))
    done;
    ignore (ReactiveJoin.process state)
  done;
  assert (ReactiveJoin.target_length state = n);
  ReactiveJoin.destroy state;
  words_since () / iters

let test_join_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: join allocation ===\n";
  List.iter
    (fun n ->
      let words = test_join_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Reactive.join end-to-end allocation ---- *)

let test_reactive_join_alloc_n n =
  Reactive.reset ();
  let left, emit_left = Reactive.Source.create ~name:"left" () in
  let right, emit_right = Reactive.Source.create ~name:"right" () in

  (* Join: for each (k, v) in left, look up k in right, produce (k, v + right_v) *)
  let joined =
    Reactive.Join.create ~name:"joined" left right
      ~key_of:(fun k _v -> k)
      ~f:(fun k v right_mb emit ->
        if Maybe.is_some right_mb then emit k (v + Maybe.unsafe_get right_mb))
      ()
  in

  (* Populate: n entries on right, then n entries on left *)
  for i = 0 to n - 1 do
    emit_set emit_right i (i * 10)
  done;
  for i = 0 to n - 1 do
    emit_set emit_left i i
  done;
  assert (Reactive.length joined = n);

  (* Pre-build waves for the hot loop: toggle all left entries *)
  let remove_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    ReactiveWave.push remove_wave (off i) Maybe.none_offheap
  done;
  let add_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    unsafe_wave_push add_wave i (Maybe.some i)
  done;

  (* Warmup *)
  for _ = 1 to 5 do
    emit_left remove_wave;
    assert (Reactive.length joined = 0);
    emit_left add_wave;
    assert (Reactive.length joined = n)
  done;
  ignore emit_right;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    emit_left remove_wave;
    emit_left add_wave
  done;
  assert (Reactive.length joined = n);
  ReactiveWave.destroy remove_wave;
  ReactiveWave.destroy add_wave;
  Reactive.destroy_graph ();
  words_since () / iters

let test_reactive_join_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: Reactive.join allocation ===\n";
  List.iter
    (fun n ->
      let words = test_reactive_join_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Reactive.fixpoint end-to-end allocation ---- *)

let test_reactive_fixpoint_alloc_n n =
  Reactive.reset ();
  let edge_values = Array.init (max 0 (n - 1)) (fun i -> [i + 1]) in
  Gc.full_major ();
  let edge_values_offheap = Array.map ReactiveOffheapList.of_list edge_values in
  let init, emit_root = Reactive.Source.create ~name:"init" () in
  let edges, emit_edges = Reactive.Source.create ~name:"edges" () in

  (* Chain graph: 0 -> 1 -> 2 -> ... -> n-1 *)
  let edge_wave = ReactiveWave.create ~max_entries:(max 1 (n - 1)) () in
  ReactiveWave.clear edge_wave;
  for i = 0 to n - 2 do
    ReactiveWave.push edge_wave (off_int i)
      (Maybe.maybe_offheap_list_to_offheap (Maybe.some edge_values_offheap.(i)))
  done;
  emit_edges edge_wave;
  let reachable = Reactive.Fixpoint.create ~name:"reachable" ~init ~edges () in

  (* Add root to populate *)
  emit_set emit_root 0 ();
  assert (Reactive.length reachable = n);

  (* Pre-build waves for the hot loop *)
  let remove_wave = ReactiveWave.create ~max_entries:1 () in
  ReactiveWave.push remove_wave (off_int 0) Maybe.none_offheap;
  let add_wave = ReactiveWave.create ~max_entries:1 () in
  ReactiveWave.push add_wave (off_int 0) (off_maybe_unit (Maybe.some ()));

  (* Warmup *)
  for _ = 1 to 5 do
    emit_root remove_wave;
    assert (Reactive.length reachable = 0);
    emit_root add_wave;
    assert (Reactive.length reachable = n)
  done;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    emit_root remove_wave;
    emit_root add_wave
  done;
  assert (Reactive.length reachable = n);
  ReactiveWave.destroy edge_wave;
  ReactiveWave.destroy remove_wave;
  ReactiveWave.destroy add_wave;
  Reactive.destroy_graph ();
  words_since () / iters

let test_reactive_fixpoint_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: Reactive.fixpoint allocation ===\n";
  List.iter
    (fun n ->
      let words = test_reactive_fixpoint_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Reactive.union end-to-end allocation ---- *)

let test_reactive_union_alloc_n n =
  Reactive.reset ();
  let left, emit_left = Reactive.Source.create ~name:"left" () in
  let right, emit_right = Reactive.Source.create ~name:"right" () in

  let merged = Reactive.Union.create ~name:"merged" left right () in

  (* Populate: n entries on the left side *)
  for i = 0 to n - 1 do
    emit_set emit_left i i
  done;
  assert (Reactive.length merged = n);

  (* Pre-build waves: single wave with all n entries *)
  let remove_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    ReactiveWave.push remove_wave (off i) Maybe.none_offheap
  done;
  let add_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    unsafe_wave_push add_wave i (Maybe.some i)
  done;

  (* Warmup *)
  for _ = 1 to 5 do
    emit_left remove_wave;
    assert (Reactive.length merged = 0);
    emit_left add_wave;
    assert (Reactive.length merged = n)
  done;
  ignore emit_right;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    emit_left remove_wave;
    emit_left add_wave
  done;
  assert (Reactive.length merged = n);
  ReactiveWave.destroy remove_wave;
  ReactiveWave.destroy add_wave;
  Reactive.destroy_graph ();
  words_since () / iters

let test_reactive_union_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: Reactive.union allocation ===\n";
  List.iter
    (fun n ->
      let words = test_reactive_union_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- Reactive.flatMap end-to-end allocation ---- *)

let test_reactive_flatmap_alloc_n n =
  Reactive.reset ();
  let src, emit_src = Reactive.Source.create ~name:"src" () in

  let derived =
    Reactive.FlatMap.create ~name:"derived" src ~f:(fun k v emit -> emit k v) ()
  in

  (* Populate: n entries *)
  for i = 0 to n - 1 do
    emit_set emit_src i i
  done;
  assert (Reactive.length derived = n);

  (* Pre-build waves *)
  let remove_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    ReactiveWave.push remove_wave (off i) Maybe.none_offheap
  done;
  let add_wave = ReactiveWave.create ~max_entries:n () in
  for i = 0 to n - 1 do
    unsafe_wave_push add_wave i (Maybe.some i)
  done;

  (* Warmup *)
  for _ = 1 to 5 do
    emit_src remove_wave;
    assert (Reactive.length derived = 0);
    emit_src add_wave;
    assert (Reactive.length derived = n)
  done;

  (* Measure *)
  let iters = 100 in
  ignore (words_since ());
  for _ = 1 to iters do
    emit_src remove_wave;
    emit_src add_wave
  done;
  assert (Reactive.length derived = n);
  ReactiveWave.destroy remove_wave;
  ReactiveWave.destroy add_wave;
  Reactive.destroy_graph ();
  words_since () / iters

let test_reactive_flatmap_alloc () =
  reset_offheap_state ();
  Printf.printf "=== Test: Reactive.flatMap allocation ===\n";
  List.iter
    (fun n ->
      let words = test_reactive_flatmap_alloc_n n in
      Printf.printf "  n=%d: %d words/iter\n" n words)
    [10; 100; 1000];
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- PoolMapSet allocation ---- *)

type empty_set_stats = {mutable total: int; mutable empty: int}

let count_pool_empty_sets pms =
  let s = {total = 0; empty = 0} in
  ReactivePoolMapSet.iter_with pms s (fun st _k set ->
      st.total <- st.total + 1;
      if ReactiveHash.Set.cardinal set = 0 then st.empty <- st.empty + 1);
  s

let test_pool_map_set_pattern_drain_key_churn () =
  reset_offheap_state ();
  Printf.printf "=== Test: PoolMapSet pattern (drain_key churn) ===\n";
  let n = 100 in
  let iters = 100 in
  let pms = ReactivePoolMapSet.create ~capacity:(n * 2) in

  for i = 0 to n - 1 do
    ReactivePoolMapSet.add pms i i
  done;

  let miss_before = ReactivePoolMapSet.debug_miss_count pms in
  ignore (words_since ());
  for iter = 1 to iters do
    let base = iter * n in
    for i = 0 to n - 1 do
      ReactivePoolMapSet.drain_key pms (base - n + i) () (fun () _ -> ())
    done;
    for i = 0 to n - 1 do
      ReactivePoolMapSet.add pms (base + i) i
    done
  done;
  let words = words_since () / iters in
  let miss_after = ReactivePoolMapSet.debug_miss_count pms in
  let miss_delta = miss_after - miss_before in
  let st = count_pool_empty_sets pms in
  Printf.printf
    "  words/iter=%d, pool_miss_delta=%d, outer=%d, empty_sets=%d/%d\n" words
    miss_delta
    (ReactivePoolMapSet.cardinal pms)
    st.empty st.total;
  assert (ReactivePoolMapSet.cardinal pms = n);
  assert (st.empty = 0);
  assert (miss_delta = 0);
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

let test_pool_map_set_pattern_remove_recycle_churn () =
  reset_offheap_state ();
  Printf.printf
    "=== Test: PoolMapSet pattern (remove_from_set_and_recycle_if_empty churn) \
     ===\n";
  let n = 100 in
  let iters = 100 in
  let pms = ReactivePoolMapSet.create ~capacity:(n * 2) in

  for i = 0 to n - 1 do
    ReactivePoolMapSet.add pms i i
  done;

  let miss_before = ReactivePoolMapSet.debug_miss_count pms in
  ignore (words_since ());
  for iter = 1 to iters do
    let base = iter * n in
    for i = 0 to n - 1 do
      ReactivePoolMapSet.remove_from_set_and_recycle_if_empty pms
        (base - n + i)
        i
    done;
    for i = 0 to n - 1 do
      ReactivePoolMapSet.add pms (base + i) i
    done
  done;
  let words = words_since () / iters in
  let miss_after = ReactivePoolMapSet.debug_miss_count pms in
  let miss_delta = miss_after - miss_before in
  let st = count_pool_empty_sets pms in
  Printf.printf
    "  words/iter=%d, pool_miss_delta=%d, outer=%d, empty_sets=%d/%d\n" words
    miss_delta
    (ReactivePoolMapSet.cardinal pms)
    st.empty st.total;
  assert (ReactivePoolMapSet.cardinal pms = n);
  assert (st.empty = 0);
  assert (miss_delta = 0);
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

(* ---- PoolMapMap allocation ---- *)

type inner_map_stats = {mutable empty: int}

let count_empty_inner_maps pmm ~start ~count =
  let s = {empty = 0} in
  for i = 0 to count - 1 do
    if ReactivePoolMapMap.inner_cardinal pmm (start + i) = 0 then
      s.empty <- s.empty + 1
  done;
  s

let test_pool_map_map_pattern_drain_outer_churn () =
  reset_offheap_state ();
  Printf.printf "=== Test: PoolMapMap pattern (drain_outer churn) ===\n";
  let n = 100 in
  let iters = 100 in
  let pmm = ReactivePoolMapMap.create ~capacity:(n * 2) in

  for i = 0 to n - 1 do
    ReactivePoolMapMap.replace pmm i i i
  done;

  let miss_before = ReactivePoolMapMap.debug_miss_count pmm in
  ignore (words_since ());
  for iter = 1 to iters do
    let base = iter * n in
    for i = 0 to n - 1 do
      ReactivePoolMapMap.drain_outer pmm (base - n + i) () (fun () _ _ -> ())
    done;
    for i = 0 to n - 1 do
      ReactivePoolMapMap.replace pmm (base + i) i i
    done
  done;
  let words = words_since () / iters in
  let miss_after = ReactivePoolMapMap.debug_miss_count pmm in
  let miss_delta = miss_after - miss_before in
  let final_start = iters * n in
  let st = count_empty_inner_maps pmm ~start:final_start ~count:n in
  Printf.printf
    "  words/iter=%d, pool_miss_delta=%d, outer=%d, empty_inners=%d/%d\n" words
    miss_delta
    (ReactivePoolMapMap.outer_cardinal pmm)
    st.empty n;
  assert (ReactivePoolMapMap.outer_cardinal pmm = n);
  assert (st.empty = 0);
  assert (miss_delta = 0);
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

let test_pool_map_map_pattern_remove_recycle_churn () =
  reset_offheap_state ();
  Printf.printf
    "=== Test: PoolMapMap pattern (remove_from_inner_and_recycle_if_empty \
     churn) ===\n";
  let n = 100 in
  let iters = 100 in
  let pmm = ReactivePoolMapMap.create ~capacity:(n * 2) in

  for i = 0 to n - 1 do
    ReactivePoolMapMap.replace pmm i i i
  done;

  let miss_before = ReactivePoolMapMap.debug_miss_count pmm in
  ignore (words_since ());
  for iter = 1 to iters do
    let base = iter * n in
    for i = 0 to n - 1 do
      ReactivePoolMapMap.remove_from_inner_and_recycle_if_empty pmm
        (base - n + i)
        i
    done;
    for i = 0 to n - 1 do
      ReactivePoolMapMap.replace pmm (base + i) i i
    done
  done;
  let words = words_since () / iters in
  let miss_after = ReactivePoolMapMap.debug_miss_count pmm in
  let miss_delta = miss_after - miss_before in
  let final_start = iters * n in
  let st = count_empty_inner_maps pmm ~start:final_start ~count:n in
  Printf.printf
    "  words/iter=%d, pool_miss_delta=%d, outer=%d, empty_inners=%d/%d\n" words
    miss_delta
    (ReactivePoolMapMap.outer_cardinal pmm)
    st.empty n;
  assert (ReactivePoolMapMap.outer_cardinal pmm = n);
  assert (st.empty = 0);
  assert (miss_delta = 0);
  print_offheap_usage ();
  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Allocation Tests ======\n\n";
  test_union_alloc ();
  test_flatmap_alloc ();
  test_join_alloc ();
  test_fixpoint_alloc ();
  test_reactive_union_alloc ();
  test_reactive_flatmap_alloc ();
  test_reactive_join_alloc ();
  test_reactive_fixpoint_alloc ();
  test_pool_map_set_pattern_drain_key_churn ();
  test_pool_map_set_pattern_remove_recycle_churn ();
  test_pool_map_map_pattern_drain_outer_churn ();
  test_pool_map_map_pattern_remove_recycle_churn ()
