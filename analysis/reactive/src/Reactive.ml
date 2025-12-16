(** Reactive collections for incremental computation.

    Provides composable reactive collections with delta-based updates. *)

(** {1 Deltas} *)

type ('k, 'v) delta = Set of 'k * 'v | Remove of 'k

let apply_delta tbl = function
  | Set (k, v) -> Hashtbl.replace tbl k v
  | Remove k -> Hashtbl.remove tbl k

let apply_deltas tbl deltas = List.iter (apply_delta tbl) deltas

(** {1 Reactive Collection} *)

type ('k, 'v) t = {
  subscribe: (('k, 'v) delta -> unit) -> unit;
  iter: ('k -> 'v -> unit) -> unit;
  get: 'k -> 'v option;
  length: unit -> int;
}
(** A reactive collection that can emit deltas and be read.
    All collections share this interface, enabling composition. *)

(** {1 Collection operations} *)

let iter f t = t.iter f
let get t k = t.get k
let length t = t.length ()

(** {1 FlatMap} *)

(** Transform a collection into another collection.
    Each source entry maps to multiple target entries via [f].
    Optional [merge] combines values when multiple sources produce the same key. *)
let flatMap (source : ('k1, 'v1) t) ~f ?merge () : ('k2, 'v2) t =
  let merge =
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
  let subscribers : (('k2, 'v2) delta -> unit) list ref = ref [] in

  let emit delta = List.iter (fun h -> h delta) !subscribers in

  let recompute_target k2 =
    match Hashtbl.find_opt contributions k2 with
    | None ->
      Hashtbl.remove target k2;
      Some (Remove k2)
    | Some contribs when Hashtbl.length contribs = 0 ->
      Hashtbl.remove contributions k2;
      Hashtbl.remove target k2;
      Some (Remove k2)
    | Some contribs ->
      let values = Hashtbl.fold (fun _ v acc -> v :: acc) contribs [] in
      let merged =
        match values with
        | [] -> assert false
        | [v] -> v
        | v :: rest -> List.fold_left merge v rest
      in
      Hashtbl.replace target k2 merged;
      Some (Set (k2, merged))
  in

  let remove_source k1 =
    match Hashtbl.find_opt provenance k1 with
    | None -> []
    | Some target_keys ->
      Hashtbl.remove provenance k1;
      target_keys
      |> List.iter (fun k2 ->
             match Hashtbl.find_opt contributions k2 with
             | None -> ()
             | Some contribs -> Hashtbl.remove contribs k1);
      target_keys
  in

  let add_source k1 entries =
    let target_keys = List.map fst entries in
    Hashtbl.replace provenance k1 target_keys;
    entries
    |> List.iter (fun (k2, v2) ->
           let contribs =
             match Hashtbl.find_opt contributions k2 with
             | Some c -> c
             | None ->
               let c = Hashtbl.create 4 in
               Hashtbl.replace contributions k2 c;
               c
           in
           Hashtbl.replace contribs k1 v2);
    target_keys
  in

  let handle_delta delta =
    let downstream =
      match delta with
      | Remove k1 ->
        let affected = remove_source k1 in
        affected |> List.filter_map recompute_target
      | Set (k1, v1) ->
        let old_affected = remove_source k1 in
        let new_entries = f k1 v1 in
        let new_affected = add_source k1 new_entries in
        let all_affected = old_affected @ new_affected in
        let seen = Hashtbl.create (List.length all_affected) in
        all_affected
        |> List.filter_map (fun k2 ->
               if Hashtbl.mem seen k2 then None
               else (
                 Hashtbl.replace seen k2 ();
                 recompute_target k2))
    in
    List.iter emit downstream
  in

  (* Subscribe to future deltas *)
  source.subscribe handle_delta;

  (* Populate from existing entries *)
  source.iter (fun k v -> handle_delta (Set (k, v)));

  (* Return collection interface *)
  {
    subscribe = (fun handler -> subscribers := handler :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
  }

(** {1 Lookup} *)

(** Lookup a single key reactively.
    Returns a collection with that single entry that updates when the
    source's value at that key changes.
    
    This is useful for creating reactive subscriptions to specific keys. *)
let lookup (source : ('k, 'v) t) ~key : ('k, 'v) t =
  let current : ('k, 'v option) Hashtbl.t = Hashtbl.create 1 in
  let subscribers : (('k, 'v) delta -> unit) list ref = ref [] in

  let emit delta = List.iter (fun h -> h delta) !subscribers in

  let handle_delta delta =
    match delta with
    | Set (k, v) when k = key ->
      Hashtbl.replace current key (Some v);
      emit (Set (key, v))
    | Remove k when k = key ->
      Hashtbl.remove current key;
      emit (Remove key)
    | _ -> () (* Ignore deltas for other keys *)
  in

  (* Subscribe to source *)
  source.subscribe handle_delta;

  (* Initialize with current value *)
  (match source.get key with
  | Some v -> Hashtbl.replace current key (Some v)
  | None -> ());

  {
    subscribe = (fun handler -> subscribers := handler :: !subscribers);
    iter =
      (fun f ->
        match Hashtbl.find_opt current key with
        | Some (Some v) -> f key v
        | _ -> ());
    get =
      (fun k ->
        if k = key then
          match Hashtbl.find_opt current key with
          | Some v -> v
          | None -> None
        else None);
    length =
      (fun () ->
        match Hashtbl.find_opt current key with
        | Some (Some _) -> 1
        | _ -> 0);
  }

(** {1 Join} *)

(** Join two collections: for each entry in [left], look up a key in [right].
    
    [key_of] extracts the lookup key from each left entry.
    [f] combines left entry with looked-up right value (if present).
    
    When either collection changes, affected entries are recomputed.
    This is more efficient than nested flatMap for join patterns. *)
let join (left : ('k1, 'v1) t) (right : ('k2, 'v2) t)
    ~(key_of : 'k1 -> 'v1 -> 'k2)
    ~(f : 'k1 -> 'v1 -> 'v2 option -> ('k3 * 'v3) list) ?merge () : ('k3, 'v3) t
    =
  let merge_fn =
    match merge with
    | Some m -> m
    | None -> fun _ v -> v
  in
  (* Track: for each left key, which right key was looked up *)
  let left_to_right_key : ('k1, 'k2) Hashtbl.t = Hashtbl.create 64 in
  (* Track: for each right key, which left keys depend on it *)
  let right_key_to_left_keys : ('k2, 'k1 list) Hashtbl.t = Hashtbl.create 64 in
  (* Current left entries *)
  let left_entries : ('k1, 'v1) Hashtbl.t = Hashtbl.create 64 in
  (* Provenance and contributions for output *)
  let provenance : ('k1, 'k3 list) Hashtbl.t = Hashtbl.create 64 in
  let contributions : ('k3, ('k1, 'v3) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 256
  in
  let target : ('k3, 'v3) Hashtbl.t = Hashtbl.create 256 in
  let subscribers : (('k3, 'v3) delta -> unit) list ref = ref [] in

  let emit delta = List.iter (fun h -> h delta) !subscribers in

  let recompute_target k3 =
    match Hashtbl.find_opt contributions k3 with
    | None ->
      Hashtbl.remove target k3;
      Some (Remove k3)
    | Some contribs when Hashtbl.length contribs = 0 ->
      Hashtbl.remove contributions k3;
      Hashtbl.remove target k3;
      Some (Remove k3)
    | Some contribs ->
      let values = Hashtbl.fold (fun _ v acc -> v :: acc) contribs [] in
      let merged =
        match values with
        | [] -> assert false
        | [v] -> v
        | v :: rest -> List.fold_left merge_fn v rest
      in
      Hashtbl.replace target k3 merged;
      Some (Set (k3, merged))
  in

  let remove_left_contributions k1 =
    match Hashtbl.find_opt provenance k1 with
    | None -> []
    | Some target_keys ->
      Hashtbl.remove provenance k1;
      target_keys
      |> List.iter (fun k3 ->
             match Hashtbl.find_opt contributions k3 with
             | None -> ()
             | Some contribs -> Hashtbl.remove contribs k1);
      target_keys
  in

  let add_left_contributions k1 entries =
    let target_keys = List.map fst entries in
    Hashtbl.replace provenance k1 target_keys;
    entries
    |> List.iter (fun (k3, v3) ->
           let contribs =
             match Hashtbl.find_opt contributions k3 with
             | Some c -> c
             | None ->
               let c = Hashtbl.create 4 in
               Hashtbl.replace contributions k3 c;
               c
           in
           Hashtbl.replace contribs k1 v3);
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
    let all_affected = old_affected @ new_affected in
    let seen = Hashtbl.create (List.length all_affected) in
    all_affected
    |> List.filter_map (fun k3 ->
           if Hashtbl.mem seen k3 then None
           else (
             Hashtbl.replace seen k3 ();
             recompute_target k3))
  in

  let remove_left_entry k1 =
    Hashtbl.remove left_entries k1;
    let affected = remove_left_contributions k1 in
    (* Clean up tracking *)
    (match Hashtbl.find_opt left_to_right_key k1 with
    | Some k2 -> (
      Hashtbl.remove left_to_right_key k1;
      match Hashtbl.find_opt right_key_to_left_keys k2 with
      | Some keys ->
        Hashtbl.replace right_key_to_left_keys k2
          (List.filter (fun k -> k <> k1) keys)
      | None -> ())
    | None -> ());
    affected |> List.filter_map recompute_target
  in

  let handle_left_delta delta =
    let downstream =
      match delta with
      | Set (k1, v1) ->
        Hashtbl.replace left_entries k1 v1;
        process_left_entry k1 v1
      | Remove k1 -> remove_left_entry k1
    in
    List.iter emit downstream
  in

  let handle_right_delta delta =
    (* When right changes, reprocess all left entries that depend on it *)
    let downstream =
      match delta with
      | Set (k2, _) | Remove k2 -> (
        match Hashtbl.find_opt right_key_to_left_keys k2 with
        | None -> []
        | Some left_keys ->
          left_keys
          |> List.concat_map (fun k1 ->
                 match Hashtbl.find_opt left_entries k1 with
                 | Some v1 -> process_left_entry k1 v1
                 | None -> []))
    in
    List.iter emit downstream
  in

  (* Subscribe to both sources *)
  left.subscribe handle_left_delta;
  right.subscribe handle_right_delta;

  (* Initialize from existing entries *)
  left.iter (fun k1 v1 ->
      Hashtbl.replace left_entries k1 v1;
      let deltas = process_left_entry k1 v1 in
      List.iter emit deltas);

  {
    subscribe = (fun handler -> subscribers := handler :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
  }

(** {1 Union} *)

(** Combine two collections into one.
    
    Returns a collection containing all entries from both [left] and [right].
    When the same key exists in both, [merge] combines values (defaults to 
    preferring right). *)
let union (left : ('k, 'v) t) (right : ('k, 'v) t) ?merge () : ('k, 'v) t =
  let merge_fn =
    match merge with
    | Some m -> m
    | None -> fun _ v -> v
  in
  (* Track contributions from each side *)
  let left_values : ('k, 'v) Hashtbl.t = Hashtbl.create 64 in
  let right_values : ('k, 'v) Hashtbl.t = Hashtbl.create 64 in
  let target : ('k, 'v) Hashtbl.t = Hashtbl.create 128 in
  let subscribers : (('k, 'v) delta -> unit) list ref = ref [] in

  let emit delta = List.iter (fun h -> h delta) !subscribers in

  let recompute_key k =
    match (Hashtbl.find_opt left_values k, Hashtbl.find_opt right_values k) with
    | None, None ->
      Hashtbl.remove target k;
      Some (Remove k)
    | Some v, None | None, Some v ->
      Hashtbl.replace target k v;
      Some (Set (k, v))
    | Some v1, Some v2 ->
      let merged = merge_fn v1 v2 in
      Hashtbl.replace target k merged;
      Some (Set (k, merged))
  in

  let handle_left_delta delta =
    let downstream =
      match delta with
      | Set (k, v) ->
        Hashtbl.replace left_values k v;
        recompute_key k |> Option.to_list
      | Remove k ->
        Hashtbl.remove left_values k;
        recompute_key k |> Option.to_list
    in
    List.iter emit downstream
  in

  let handle_right_delta delta =
    let downstream =
      match delta with
      | Set (k, v) ->
        Hashtbl.replace right_values k v;
        recompute_key k |> Option.to_list
      | Remove k ->
        Hashtbl.remove right_values k;
        recompute_key k |> Option.to_list
    in
    List.iter emit downstream
  in

  (* Subscribe to both sources *)
  left.subscribe handle_left_delta;
  right.subscribe handle_right_delta;

  (* Initialize from existing entries *)
  left.iter (fun k v ->
      Hashtbl.replace left_values k v;
      ignore (recompute_key k));
  right.iter (fun k v ->
      Hashtbl.replace right_values k v;
      ignore (recompute_key k));

  {
    subscribe = (fun handler -> subscribers := handler :: !subscribers);
    iter = (fun f -> Hashtbl.iter f target);
    get = (fun k -> Hashtbl.find_opt target k);
    length = (fun () -> Hashtbl.length target);
  }

(** {1 Fixpoint} *)

(** Incremental Fixpoint Computation.

    This implements the incremental fixpoint algorithm using:
    - BFS for expansion (when base or edges grow)
    - Well-founded derivation for contraction (when base or edges shrink)
    
    The fixpoint combinator maintains the least fixpoint of a monotone operator:
    
      F(S) = base ∪ step(S)
      
    where step(S) = ⋃{successors(x) | x ∈ S}
    
    Key insight: The rank of an element is its BFS distance from base.
    Cycle members have equal ranks, so they cannot provide well-founded
    support to each other, ensuring unreachable cycles are correctly removed. *)

module Fixpoint = struct
  type 'k state = {
    current: ('k, unit) Hashtbl.t; (* Current fixpoint set *)
    rank: ('k, int) Hashtbl.t; (* BFS distance from base *)
    inv_index: ('k, 'k list) Hashtbl.t;
        (* Inverse step relation: target → sources *)
    base: ('k, unit) Hashtbl.t; (* Current base set *)
    edges: ('k, 'k list) Hashtbl.t; (* Current edges snapshot *)
  }

  let create () =
    {
      current = Hashtbl.create 256;
      rank = Hashtbl.create 256;
      inv_index = Hashtbl.create 256;
      base = Hashtbl.create 64;
      edges = Hashtbl.create 256;
    }

  (* Inverse index helpers *)
  let add_to_inv_index state ~source ~target =
    let sources =
      match Hashtbl.find_opt state.inv_index target with
      | Some s -> s
      | None -> []
    in
    if not (List.mem source sources) then
      Hashtbl.replace state.inv_index target (source :: sources)

  let remove_from_inv_index state ~source ~target =
    match Hashtbl.find_opt state.inv_index target with
    | None -> ()
    | Some sources ->
      let filtered = List.filter (fun s -> s <> source) sources in
      if filtered = [] then Hashtbl.remove state.inv_index target
      else Hashtbl.replace state.inv_index target filtered

  let iter_step_inv state x f =
    match Hashtbl.find_opt state.inv_index x with
    | None -> ()
    | Some sources -> List.iter f sources

  (* Get successors from edges *)
  let get_successors state x =
    match Hashtbl.find_opt state.edges x with
    | None -> []
    | Some succs -> succs

  (* Expansion: BFS from frontier, returns list of newly added elements *)
  let expand state ~frontier =
    let added = ref [] in
    let current_frontier = Hashtbl.create 64 in
    let next_frontier = Hashtbl.create 64 in

    (* Initialize current frontier *)
    List.iter (fun x -> Hashtbl.replace current_frontier x ()) frontier;

    let r = ref 0 in

    while Hashtbl.length current_frontier > 0 do
      (* Add all frontier elements to current with rank r *)
      Hashtbl.iter
        (fun x () ->
          if not (Hashtbl.mem state.current x) then (
            Hashtbl.replace state.current x ();
            Hashtbl.replace state.rank x !r;
            added := x :: !added))
        current_frontier;

      (* Compute next frontier: successors not yet in current *)
      Hashtbl.clear next_frontier;
      Hashtbl.iter
        (fun x () ->
          let successors = get_successors state x in
          List.iter
            (fun y ->
              (* Update inverse index: record that x derives y *)
              add_to_inv_index state ~source:x ~target:y;
              (* Add to next frontier if not already in current *)
              if not (Hashtbl.mem state.current y) then
                Hashtbl.replace next_frontier y ())
            successors)
        current_frontier;

      (* Swap frontiers *)
      Hashtbl.clear current_frontier;
      Hashtbl.iter
        (fun x () -> Hashtbl.replace current_frontier x ())
        next_frontier;
      incr r
    done;

    !added

  (* Check if element has a well-founded deriver in the current set *)
  let has_well_founded_deriver state x ~dying =
    match Hashtbl.find_opt state.rank x with
    | None -> false
    | Some rx ->
      let found = ref false in
      iter_step_inv state x (fun y ->
          if not !found then
            let in_current = Hashtbl.mem state.current y in
            let not_dying = not (Hashtbl.mem dying y) in
            match Hashtbl.find_opt state.rank y with
            | None -> ()
            | Some ry ->
              if in_current && not_dying && ry < rx then found := true);
      !found

  (* Contraction: remove elements that lost support, returns list of removed *)
  let contract state ~worklist =
    let dying = Hashtbl.create 64 in
    let current_worklist = Hashtbl.create 64 in

    (* Initialize worklist *)
    List.iter (fun x -> Hashtbl.replace current_worklist x ()) worklist;

    while Hashtbl.length current_worklist > 0 do
      (* Pop an element from worklist *)
      let x =
        let result = ref None in
        Hashtbl.iter
          (fun k () -> if !result = None then result := Some k)
          current_worklist;
        match !result with
        | None -> assert false (* worklist not empty *)
        | Some k ->
          Hashtbl.remove current_worklist k;
          k
      in

      (* Skip if already dying or in base *)
      if Hashtbl.mem dying x || Hashtbl.mem state.base x then ()
      else
        (* Check for well-founded deriver *)
        let has_support = has_well_founded_deriver state x ~dying in

        if not has_support then (
          (* x dies: no well-founded support *)
          Hashtbl.replace dying x ();

          (* Find dependents: elements z such that x derives z *)
          let successors = get_successors state x in
          List.iter
            (fun z ->
              if Hashtbl.mem state.current z && not (Hashtbl.mem dying z) then
                Hashtbl.replace current_worklist z ())
            successors)
    done;

    (* Remove dying elements from current and rank *)
    let removed = ref [] in
    Hashtbl.iter
      (fun x () ->
        Hashtbl.remove state.current x;
        Hashtbl.remove state.rank x;
        removed := x :: !removed)
      dying;

    !removed

  (* Apply a delta from init (base) collection *)
  let apply_init_delta state delta =
    match delta with
    | Set (k, ()) ->
      let was_in_base = Hashtbl.mem state.base k in
      Hashtbl.replace state.base k ();
      if was_in_base then ([], []) (* Already in base, no change *)
      else
        (* New base element: expand from it *)
        let added = expand state ~frontier:[k] in
        (added, [])
    | Remove k ->
      if not (Hashtbl.mem state.base k) then ([], [])
        (* Not in base, no change *)
      else (
        (* Mirror the verified algorithm's contraction+re-derivation pattern:
           removing from base can invalidate the previously-shortest witness rank
           for reachable nodes, so contraction alone can remove nodes incorrectly.
           We contract first, then attempt to re-derive removed nodes via surviving
           predecessors (using the inverse index), then expand. *)
        Hashtbl.remove state.base k;

        let contraction_worklist =
          if Hashtbl.mem state.current k then [k] else []
        in
        let all_removed =
          if contraction_worklist <> [] then
            contract state ~worklist:contraction_worklist
          else []
        in

        let expansion_frontier = ref [] in
        let removed_set = Hashtbl.create (List.length all_removed) in
        List.iter (fun x -> Hashtbl.replace removed_set x ()) all_removed;

        if Hashtbl.length removed_set > 0 then
          Hashtbl.iter
            (fun y () ->
              iter_step_inv state y (fun x ->
                  if Hashtbl.mem state.current x then
                    expansion_frontier := y :: !expansion_frontier))
            removed_set;

        let all_added =
          if !expansion_frontier <> [] then
            expand state ~frontier:!expansion_frontier
          else []
        in

        let net_removed =
          List.filter (fun x -> not (Hashtbl.mem state.current x)) all_removed
        in
        let net_added =
          List.filter (fun x -> not (Hashtbl.mem removed_set x)) all_added
        in

        (net_added, net_removed))

  (* Compute edge diff between old and new successors *)
  let compute_edge_diff old_succs new_succs =
    let old_set = Hashtbl.create (List.length old_succs) in
    List.iter (fun x -> Hashtbl.replace old_set x ()) old_succs;
    let new_set = Hashtbl.create (List.length new_succs) in
    List.iter (fun x -> Hashtbl.replace new_set x ()) new_succs;

    let removed =
      List.filter (fun x -> not (Hashtbl.mem new_set x)) old_succs
    in
    let added = List.filter (fun x -> not (Hashtbl.mem old_set x)) new_succs in
    (removed, added)

  (* Apply a delta from edges collection *)
  let apply_edges_delta state delta =
    match delta with
    | Set (source, new_succs) ->
      let old_succs =
        match Hashtbl.find_opt state.edges source with
        | None -> []
        | Some s -> s
      in
      Hashtbl.replace state.edges source new_succs;

      let removed_targets, added_targets =
        compute_edge_diff old_succs new_succs
      in

      (* Process removed edges *)
      let contraction_worklist = ref [] in
      List.iter
        (fun target ->
          remove_from_inv_index state ~source ~target;
          if
            Hashtbl.mem state.current source && Hashtbl.mem state.current target
          then contraction_worklist := target :: !contraction_worklist)
        removed_targets;

      let all_removed =
        if !contraction_worklist <> [] then
          contract state ~worklist:!contraction_worklist
        else []
      in

      (* Process added edges *)
      let expansion_frontier = ref [] in
      List.iter
        (fun target ->
          add_to_inv_index state ~source ~target;
          if
            Hashtbl.mem state.current source
            && not (Hashtbl.mem state.current target)
          then expansion_frontier := target :: !expansion_frontier)
        added_targets;

      (* Check if any removed element can be re-derived via remaining edges *)
      let removed_set = Hashtbl.create (List.length all_removed) in
      List.iter (fun x -> Hashtbl.replace removed_set x ()) all_removed;

      if Hashtbl.length removed_set > 0 then
        Hashtbl.iter
          (fun y () ->
            iter_step_inv state y (fun x ->
                if Hashtbl.mem state.current x then
                  expansion_frontier := y :: !expansion_frontier))
          removed_set;

      let all_added =
        if !expansion_frontier <> [] then
          expand state ~frontier:!expansion_frontier
        else []
      in

      (* Compute net changes *)
      let net_removed =
        List.filter (fun x -> not (Hashtbl.mem state.current x)) all_removed
      in
      let net_added =
        List.filter (fun x -> not (Hashtbl.mem removed_set x)) all_added
      in

      (net_added, net_removed)
    | Remove source ->
      let old_succs =
        match Hashtbl.find_opt state.edges source with
        | None -> []
        | Some s -> s
      in
      Hashtbl.remove state.edges source;

      (* All edges from source are removed *)
      let contraction_worklist = ref [] in
      List.iter
        (fun target ->
          remove_from_inv_index state ~source ~target;
          if
            Hashtbl.mem state.current source && Hashtbl.mem state.current target
          then contraction_worklist := target :: !contraction_worklist)
        old_succs;

      let all_removed =
        if !contraction_worklist <> [] then
          contract state ~worklist:!contraction_worklist
        else []
      in

      (* Check if any removed element can be re-derived via remaining edges.
         This mirrors the reference implementation's "step 7" re-derivation pass:
         removing an edge can invalidate the previously-shortest witness rank for a
         node while preserving reachability via a longer path. Contraction alone
         can incorrectly remove such nodes because their stored rank is stale/too low. *)
      let expansion_frontier = ref [] in

      let removed_set = Hashtbl.create (List.length all_removed) in
      List.iter (fun x -> Hashtbl.replace removed_set x ()) all_removed;

      if Hashtbl.length removed_set > 0 then
        Hashtbl.iter
          (fun y () ->
            iter_step_inv state y (fun x ->
                if Hashtbl.mem state.current x then
                  expansion_frontier := y :: !expansion_frontier))
          removed_set;

      let all_added =
        if !expansion_frontier <> [] then
          expand state ~frontier:!expansion_frontier
        else []
      in

      (* Compute net changes *)
      let net_removed =
        List.filter (fun x -> not (Hashtbl.mem state.current x)) all_removed
      in
      let net_added =
        List.filter (fun x -> not (Hashtbl.mem removed_set x)) all_added
      in

      (net_added, net_removed)
end

(** Compute transitive closure via incremental fixpoint.
    
    Starting from keys in [init], follows edges to discover all reachable keys.
    
    When [init] or [edges] changes, the fixpoint updates incrementally:
    - Expansion: BFS from new base elements or newly reachable successors
    - Contraction: Well-founded cascade removal when elements lose support *)
let fixpoint ~(init : ('k, unit) t) ~(edges : ('k, 'k list) t) () : ('k, unit) t
    =
  let state = Fixpoint.create () in
  let subscribers : (('k, unit) delta -> unit) list ref = ref [] in

  let emit delta = List.iter (fun h -> h delta) !subscribers in

  let emit_changes (added, removed) =
    List.iter (fun k -> emit (Set (k, ()))) added;
    List.iter (fun k -> emit (Remove k)) removed
  in

  (* Handle init deltas *)
  let handle_init_delta delta =
    let changes = Fixpoint.apply_init_delta state delta in
    emit_changes changes
  in

  (* Handle edges deltas *)
  let handle_edges_delta delta =
    let changes = Fixpoint.apply_edges_delta state delta in
    emit_changes changes
  in

  (* Subscribe to changes *)
  init.subscribe handle_init_delta;
  edges.subscribe handle_edges_delta;

  (* Initialize from existing data *)
  (* First, load all edges so expansion works correctly *)
  edges.iter (fun k succs -> Hashtbl.replace state.edges k succs);

  (* Build inverse index for existing edges *)
  Hashtbl.iter
    (fun source succs ->
      List.iter
        (fun target -> Fixpoint.add_to_inv_index state ~source ~target)
        succs)
    state.edges;

  (* Then process init elements *)
  let initial_frontier = ref [] in
  init.iter (fun k () ->
      Hashtbl.replace state.base k ();
      initial_frontier := k :: !initial_frontier);
  ignore (Fixpoint.expand state ~frontier:!initial_frontier);

  {
    subscribe = (fun handler -> subscribers := handler :: !subscribers);
    iter = (fun f -> Hashtbl.iter f state.current);
    get = (fun k -> Hashtbl.find_opt state.current k);
    length = (fun () -> Hashtbl.length state.current);
  }
