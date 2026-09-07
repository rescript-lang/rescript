exception Cycle of string list

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
  List.iter (visit []) nodes;
  List.rev !result
