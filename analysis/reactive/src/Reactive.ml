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
