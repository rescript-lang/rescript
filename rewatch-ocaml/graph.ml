exception Cycle of string list

let canonical_cycle cycle =
  let rec without_last = function
  | [] | [_] -> []
  | value :: rest -> value :: without_last rest
  in
  let nodes = without_last cycle in
  let rec rotations prefix suffix =
    match suffix with
    | [] -> []
    | (head :: rest as rotation) ->
      (rotation @ List.rev prefix) :: rotations (head :: prefix) rest
  in
  match rotations [] nodes with
  | [] -> cycle
  | first :: rest ->
    let best =
      List.fold_left
        (fun best candidate -> if candidate < best then candidate else best)
        first rest
    in
    (match best with head :: _ -> best @ [head] | [] -> cycle)

let shortest_cycle nodes ~name ~deps =
  let by_name = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace by_name (name node) node) nodes;
  let best = ref None in
  let consider cycle =
    let cycle = canonical_cycle cycle in
    match !best with
    | None -> best := Some cycle
    | Some current ->
      let cycle_length = List.length cycle in
      let current_length = List.length current in
      if cycle_length < current_length
         || (cycle_length = current_length && cycle < current)
      then best := Some cycle
  in
  nodes
  |> List.sort (fun left right -> String.compare (name left) (name right))
  |> List.iter (fun start_node ->
       let start = name start_node in
       let queue = Queue.create () in
       let parents = Hashtbl.create (List.length nodes) in
       let distances = Hashtbl.create (List.length nodes) in
       Hashtbl.add distances start 0;
       Queue.add start queue;
       let found = ref false in
       while (not !found) && not (Queue.is_empty queue) do
         let current = Queue.take queue in
         let distance = Hashtbl.find distances current in
         let can_improve =
           match !best with
           | None -> true
           | Some cycle -> distance + 2 <= List.length cycle
         in
         if can_improve then
           match Hashtbl.find_opt by_name current with
           | None -> ()
           | Some node ->
             deps node |> List.sort_uniq String.compare
             |> List.iter (fun dependency ->
                  if dependency = start then (
                    let rec path_to_start acc node_name =
                      if node_name = start then start :: acc
                      else
                        path_to_start (node_name :: acc)
                          (Hashtbl.find parents node_name)
                    in
                    consider (path_to_start [] current @ [start]);
                    found := true)
                  else if
                    Hashtbl.mem by_name dependency
                    && not (Hashtbl.mem distances dependency)
                  then (
                    Hashtbl.add parents dependency current;
                    Hashtbl.add distances dependency (distance + 1);
                    Queue.add dependency queue))
       done);
  !best

let topological_sort nodes ~name ~deps =
  let by_name = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace by_name (name node) node) nodes;
  let state = Hashtbl.create (List.length nodes) in
  let result = ref [] in
  let rec visit stack node =
    let node_name = name node in
    match Hashtbl.find_opt state node_name with
    | Some `Done -> ()
    | Some `Visiting -> raise (Cycle (List.rev (node_name :: stack)))
    | None ->
      Hashtbl.replace state node_name `Visiting;
      List.iter
        (fun dep ->
          match Hashtbl.find_opt by_name dep with
          | None -> ()
          | Some dep_node -> visit (node_name :: stack) dep_node)
        (deps node);
      Hashtbl.replace state node_name `Done;
      result := node :: !result
  in
  try
    List.iter (visit []) nodes;
    List.rev !result
  with Cycle cycle -> (
    match shortest_cycle nodes ~name ~deps with
    | Some shortest -> raise (Cycle shortest)
    | None -> raise (Cycle cycle))
