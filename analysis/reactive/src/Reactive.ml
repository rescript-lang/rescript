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
