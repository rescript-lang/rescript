type validation =
  | Replace_duplicates_and_ignore_unknown
  | Reject_invalid of {
      duplicate_node: string -> exn;
      unknown_dependency: node:string -> dependency:string -> exn;
    }

type 'a index = {
  nodes_by_name: (string, 'a) Hashtbl.t;
  dependencies_by_name: (string, string list) Hashtbl.t;
  dependents_by_name: (string, string list) Hashtbl.t;
}

let create_index nodes ~name ~deps ~validation =
  let nodes_by_name = Hashtbl.create (List.length nodes) in
  List.iter
    (fun node ->
      let node_name = name node in
      match Hashtbl.find_opt nodes_by_name node_name with
      | None -> Hashtbl.add nodes_by_name node_name node
      | Some _ -> (
        match validation with
        | Replace_duplicates_and_ignore_unknown ->
          Hashtbl.replace nodes_by_name node_name node
        | Reject_invalid {duplicate_node; unknown_dependency = _} ->
          raise (duplicate_node node_name)))
    nodes;
  let dependencies_by_name = Hashtbl.create (Hashtbl.length nodes_by_name) in
  let dependents_by_name = Hashtbl.create (Hashtbl.length nodes_by_name) in
  Hashtbl.iter
    (fun node_name node ->
      let dependencies =
        deps node
        |> List.sort_uniq String.compare
        |> List.filter (fun dependency ->
            if Hashtbl.mem nodes_by_name dependency then true
            else
              match validation with
              | Replace_duplicates_and_ignore_unknown -> false
              | Reject_invalid {duplicate_node = _; unknown_dependency} ->
                raise (unknown_dependency ~node:node_name ~dependency))
      in
      Hashtbl.add dependencies_by_name node_name dependencies;
      List.iter
        (fun dependency ->
          let dependents =
            Hashtbl.find_opt dependents_by_name dependency
            |> Option.value ~default:[]
          in
          Hashtbl.replace dependents_by_name dependency (node_name :: dependents))
        dependencies)
    nodes_by_name;
  {nodes_by_name; dependencies_by_name; dependents_by_name}

let node_count index = Hashtbl.length index.nodes_by_name
let find_node index name = Hashtbl.find index.nodes_by_name name

let dependencies index name =
  Hashtbl.find_opt index.dependencies_by_name name |> Option.value ~default:[]

let dependents index name =
  Hashtbl.find_opt index.dependents_by_name name |> Option.value ~default:[]

let dependency_count index name = List.length (dependencies index name)

let cycle_blocked_nodes nodes ~name ~deps =
  let index =
    create_index nodes ~name ~deps
      ~validation:Replace_duplicates_and_ignore_unknown
  in
  let pending = Hashtbl.create (node_count index) in
  Hashtbl.iter
    (fun node_name _ ->
      Hashtbl.add pending node_name (dependency_count index node_name))
    index.nodes_by_name;
  let ready = Queue.create () in
  Hashtbl.iter (fun key count -> if count = 0 then Queue.add key ready) pending;
  let removed = Hashtbl.create (node_count index) in
  while not (Queue.is_empty ready) do
    let key = Queue.take ready in
    Hashtbl.replace removed key ();
    dependents index key
    |> List.iter (fun dependent ->
        let count = Hashtbl.find pending dependent - 1 in
        Hashtbl.replace pending dependent count;
        if count = 0 then Queue.add dependent ready)
  done;
  List.filter (fun node -> not (Hashtbl.mem removed (name node))) nodes

let canonical_cycle cycle =
  let rec without_last = function
    | [] | [_] -> []
    | value :: rest -> value :: without_last rest
  in
  let nodes = without_last cycle in
  match nodes with
  | [] -> cycle
  | first :: rest ->
    let smallest = List.fold_left min first rest in
    let rec split prefix = function
      | [] -> nodes
      | head :: tail as suffix ->
        if head = smallest then suffix @ List.rev prefix
        else split (head :: prefix) tail
    in
    let canonical = split [] nodes in
    canonical @ [smallest]

let shortest_cycle_in_index index =
  let names =
    index.nodes_by_name |> Hashtbl.to_seq_keys |> List.of_seq
    |> List.sort String.compare
  in
  let edges = Hashtbl.create (Hashtbl.length index.dependencies_by_name) in
  List.iter
    (fun name ->
      List.iter
        (fun dependency -> Hashtbl.replace edges (name, dependency) ())
        (dependencies index name))
    names;
  match List.find_opt (fun name -> Hashtbl.mem edges (name, name)) names with
  | Some name -> Some [name; name]
  | None -> (
    let two_node_cycle =
      names
      |> List.find_map (fun name ->
          dependencies index name
          |> List.filter (fun dependency -> name < dependency)
          |> List.sort String.compare
          |> List.find_map (fun dependency ->
              if Hashtbl.mem edges (dependency, name) then
                Some [name; dependency; name]
              else None))
    in
    match two_node_cycle with
    | Some _ as cycle -> cycle
    | None ->
      let best = ref None in
      let best_length = ref max_int in
      let consider cycle =
        let cycle = canonical_cycle cycle in
        match !best with
        | None ->
          best := Some cycle;
          best_length := List.length cycle
        | Some current ->
          let cycle_length = List.length cycle in
          if
            cycle_length < !best_length
            || (cycle_length = !best_length && cycle < current)
          then (
            best := Some cycle;
            best_length := cycle_length)
      in
      List.iter
        (fun start ->
          let queue = Queue.create () in
          let parents = Hashtbl.create 16 in
          let distances = Hashtbl.create 16 in
          Hashtbl.add distances start 0;
          Queue.add start queue;
          let found = ref false in
          while (not !found) && not (Queue.is_empty queue) do
            let current = Queue.take queue in
            let distance = Hashtbl.find distances current in
            let can_improve = distance + 2 <= !best_length in
            if can_improve then
              dependencies index current
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
                  else if not (Hashtbl.mem distances dependency) then (
                    Hashtbl.add parents dependency current;
                    Hashtbl.add distances dependency (distance + 1);
                    Queue.add dependency queue))
          done)
        names;
      !best)

let shortest_cycle nodes ~name ~deps =
  create_index nodes ~name ~deps
    ~validation:Replace_duplicates_and_ignore_unknown
  |> shortest_cycle_in_index
