type 'k t = {
  current: ('k, unit) Hashtbl.t;
  edge_map: ('k, 'k list) Hashtbl.t;
  roots: ('k, unit) Hashtbl.t;
}

let create () =
  {
    current = Hashtbl.create 256;
    edge_map = Hashtbl.create 256;
    roots = Hashtbl.create 64;
  }

let iter_current t f = Hashtbl.iter f t.current
let get_current t k = Hashtbl.find_opt t.current k
let current_length t = Hashtbl.length t.current

(* BFS helper to find all reachable from roots. *)
let compute_reachable_from_roots t =
  let new_current = Hashtbl.create (Hashtbl.length t.current) in
  let frontier = Queue.create () in

  Hashtbl.iter
    (fun k () ->
      Hashtbl.replace new_current k ();
      Queue.add k frontier)
    t.roots;

  while not (Queue.is_empty frontier) do
    let k = Queue.pop frontier in
    match Hashtbl.find_opt t.edge_map k with
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

let replace_current_with t new_current =
  Hashtbl.reset t.current;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.current k v) new_current

let initialize t ~roots_iter ~edges_iter =
  Hashtbl.reset t.roots;
  Hashtbl.reset t.edge_map;
  roots_iter (fun k () -> Hashtbl.replace t.roots k ());
  edges_iter (fun k successors -> Hashtbl.replace t.edge_map k successors);
  replace_current_with t (compute_reachable_from_roots t)

let apply t ~init_entries ~edge_entries =
  let output_entries = ref [] in

  let needs_full_recompute = ref false in

  List.iter
    (fun (k, v_opt) ->
      match v_opt with
      | Some successors ->
        let old = Hashtbl.find_opt t.edge_map k in
        Hashtbl.replace t.edge_map k successors;
        (* If edges changed for a current node, may need recompute. *)
        if Hashtbl.mem t.current k && old <> Some successors then
          needs_full_recompute := true
      | None ->
        if Hashtbl.mem t.edge_map k then (
          Hashtbl.remove t.edge_map k;
          if Hashtbl.mem t.current k then needs_full_recompute := true))
    edge_entries;

  List.iter
    (fun (k, v_opt) ->
      match v_opt with
      | Some () -> Hashtbl.replace t.roots k ()
      | None ->
        if Hashtbl.mem t.roots k then (
          Hashtbl.remove t.roots k;
          needs_full_recompute := true))
    init_entries;

  if !needs_full_recompute then (
    let new_current = compute_reachable_from_roots t in

    Hashtbl.iter
      (fun k () ->
        if not (Hashtbl.mem new_current k) then
          output_entries := (k, None) :: !output_entries)
      t.current;

    Hashtbl.iter
      (fun k () ->
        if not (Hashtbl.mem t.current k) then
          output_entries := (k, Some ()) :: !output_entries)
      new_current;

    replace_current_with t new_current)
  else (
    let frontier = Queue.create () in

    init_entries
    |> List.iter (fun (k, v_opt) ->
           match v_opt with
           | Some () when not (Hashtbl.mem t.current k) ->
             Hashtbl.replace t.current k ();
             output_entries := (k, Some ()) :: !output_entries;
             Queue.add k frontier
           | _ -> ());

    while not (Queue.is_empty frontier) do
      let k = Queue.pop frontier in
      match Hashtbl.find_opt t.edge_map k with
      | None -> ()
      | Some successors ->
        List.iter
          (fun succ ->
            if not (Hashtbl.mem t.current succ) then (
              Hashtbl.replace t.current succ ();
              output_entries := (succ, Some ()) :: !output_entries;
              Queue.add succ frontier))
          successors
    done);

  !output_entries
