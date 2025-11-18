open Graph_lib

let parse file contents = parse_file ~file contents

let expect_equal expected actual label =
  if expected <> actual then
    failwith
      (Printf.sprintf "Expected %s = %s but got %s" label
         (String.concat "," (List.map string_of_int expected))
         (String.concat "," (List.map string_of_int actual)))

let expect_raises label f predicate =
  match f () with
  | exception exn when predicate exn -> ()
  | exception _ ->
      failwith (Printf.sprintf "Expected %s but saw different exception" label)
  | _ -> failwith (Printf.sprintf "Expected %s but no exception was raised" label)

let () =
  let graphs =
    [
      parse "a.graph"
        "node 1 marked\nnode 2\nnode 3\nedge 1 2\nedge 2 3\n";
      parse "b.graph" "node 4\nnode 5\nedge 4 5\n";
      parse "c.graph" "node 6 marked\nnode 7\nedge 6 7\nedge 7 4\n";
      parse "d.graph" "node 8\nnode 9\nedge 8 9\n";
    ]
  in
  let unreachable = unreachable_nodes graphs in
  expect_equal [ 8; 9 ] unreachable "unreachable nodes";
  expect_raises "undefined target"
    (fun () ->
      let bad =
        [
          parse "x.graph" "node 10 marked\n";
          parse "y.graph" "node 11\nedge 11 42\n";
        ]
      in
      ignore (unreachable_nodes bad))
    (function Edge_target_undefined _ -> true | _ -> false);
  expect_raises "duplicate nodes"
    (fun () ->
      let dup =
        [
          parse "a.graph" "node 1 marked\n";
          parse "b.graph" "node 1\n";
        ]
      in
      ignore (unreachable_nodes dup))
    (function Duplicate_node_across_files _ -> true | _ -> false);
  print_endline "Traditional graph tests passed."

