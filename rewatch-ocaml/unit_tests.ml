let check condition message = if not condition then failwith message

let () =
  let node name deps = (name, deps) in
  let nodes = [node "C" ["B"]; node "A" []; node "B" ["A"]] in
  let sorted =
    Graph.topological_sort nodes ~name:fst ~deps:snd |> List.map fst
  in
  check (sorted = ["A"; "B"; "C"]) "topological ordering";
  let cycle_detected =
    try
      ignore
        (Graph.topological_sort
           [node "A" ["B"]; node "B" ["A"]]
           ~name:fst ~deps:snd);
      false
    with Graph.Cycle _ -> true
  in
  check cycle_detected "cycle detection"
