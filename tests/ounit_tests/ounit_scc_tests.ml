let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let tiny_test_cases =
  {|
13
22
 4  2
 2  3
 3  2
 6  0
 0  1
 2  0
11 12
12  9
 9 10
 9 11
 7  9
10 12
11  4
 4  3
 3  5
 6  8
 8  6
 5  4
 0  5
 6  4
 6  9
 7  6
|}

let medium_test_cases =
  {|
50
147
 0  7
 0 34
 1 14
 1 45
 1 21
 1 22
 1 22
 1 49
 2 19
 2 25
 2 33
 3  4
 3 17
 3 27
 3 36
 3 42
 4 17
 4 17
 4 27
 5 43
 6 13
 6 13
 6 28
 6 28
 7 41
 7 44
 8 19
 8 48
 9  9
 9 11
 9 30
 9 46
10  0
10  7
10 28
10 28
10 28
10 29
10 29
10 34
10 41
11 21
11 30
12  9
12 11
12 21
12 21
12 26
13 22
13 23
13 47
14  8
14 21
14 48
15  8
15 34
15 49
16  9
17 20
17 24
17 38
18  6
18 28
18 32
18 42
19 15
19 40
20  3
20 35
20 38
20 46
22  6
23 11
23 21
23 22
24  4
24  5
24 38
24 43
25  2
25 34
26  9
26 12
26 16
27  5
27 24
27 32
27 31
27 42
28 22
28 29
28 39
28 44
29 22
29 49
30 23
30 37
31 18
31 32
32  5
32  6
32 13
32 37
32 47
33  2
33  8
33 19
34  2 
34 19
34 40
35  9
35 37
35 46
36 20
36 42
37  5
37  9
37 35
37 47
37 47
38 35
38 37
38 38
39 18
39 42
40 15
41 28
41 44
42 31
43 37
43 38
44 39
45  8
45 14
45 14
45 15
45 49
46 16
47 23
47 30
48 12
48 21
48 33
48 33
49 34
49 22
49 49
|}
(*
   reference output:
   http://algs4.cs.princeton.edu/42digraph/KosarajuSharirSCC.java.html
*)

let handle_lines tiny_test_cases =
  match Ext_string.split tiny_test_cases '\n' with
  | nodes :: _edges :: rest ->
    let nodes_num = int_of_string nodes in
    let node_array = Array.init nodes_num (fun _ -> Vec_int.empty ()) in
    Ext_list.iter rest (fun x ->
        match Ext_string.split x ' ' with
        | [a; b] ->
          let a, b = (int_of_string a, int_of_string b) in
          Vec_int.push node_array.(a) b
        | _ -> assert false);
    node_array
  | _ -> assert false

let read_file file =
  let in_chan = open_in_bin file in
  let nodes_sum = int_of_string (input_line in_chan) in
  let node_array = Array.init nodes_sum (fun _ -> Vec_int.empty ()) in
  let rec aux () =
    match input_line in_chan with
    | exception End_of_file -> ()
    | x ->
      (match Ext_string.split x ' ' with
      | [a; b] ->
        let a, b = (int_of_string a, int_of_string b) in
        Vec_int.push node_array.(a) b
      | _ -> (* assert false  *) ());
      aux ()
  in
  print_endline "read data into memory";
  aux ();
  fst (Ext_scc.graph_check node_array)
(* 25 *)

let test (input : (string * string list) list) =
  (* string -> int mapping 
  *)
  let tbl = Hash_string.create 32 in
  let idx = ref 0 in
  let add x =
    if not (Hash_string.mem tbl x) then (
      Hash_string.add tbl x !idx;
      incr idx)
  in
  input |> List.iter (fun (x, others) -> List.iter add (x :: others));
  let nodes_num = Hash_string.length tbl in
  let node_array = Array.init nodes_num (fun _ -> Vec_int.empty ()) in
  input
  |> List.iter (fun (x, others) ->
         let idx = Hash_string.find_exn tbl x in
         others
         |> List.iter (fun y ->
                Vec_int.push node_array.(idx) (Hash_string.find_exn tbl y)));
  Ext_scc.graph_check node_array

let test2 (input : (string * string list) list) =
  (* string -> int mapping 
  *)
  let tbl = Hash_string.create 32 in
  let idx = ref 0 in
  let add x =
    if not (Hash_string.mem tbl x) then (
      Hash_string.add tbl x !idx;
      incr idx)
  in
  input |> List.iter (fun (x, others) -> List.iter add (x :: others));
  let nodes_num = Hash_string.length tbl in
  let other_mapping = Array.make nodes_num "" in
  Hash_string.iter tbl (fun k v -> other_mapping.(v) <- k);

  let node_array = Array.init nodes_num (fun _ -> Vec_int.empty ()) in
  input
  |> List.iter (fun (x, others) ->
         let idx = Hash_string.find_exn tbl x in
         others
         |> List.iter (fun y ->
                Vec_int.push node_array.(idx) (Hash_string.find_exn tbl y)));
  let output = Ext_scc.graph node_array in
  output
  |> Int_vec_vec.map_into_array (fun int_vec ->
         Vec_int.map_into_array (fun i -> other_mapping.(i)) int_vec)

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (fst @@ Ext_scc.graph_check (handle_lines tiny_test_cases))
             5 );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (fst @@ Ext_scc.graph_check (handle_lines medium_test_cases))
             10 );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [("a", ["b"; "c"]); ("b", ["c"; "d"]); ("c", ["b"]); ("d", [])])
             (3, [1; 2; 1]) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [
                  ("a", ["b"; "c"]);
                  ("b", ["c"; "d"]);
                  ("c", ["b"]);
                  ("d", []);
                  ("e", []);
                ])
             (4, [1; 1; 2; 1])
         (* {[
              a -> b
              a -> c
              b -> c
              b -> d
              c -> b
              d
              e
              ]}
              {[
              [d ; e ; [b;c] [a] ]
              ]}
           *)
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [
                  ("a", ["b"; "c"]);
                  ("b", ["c"; "d"]);
                  ("c", ["b"]);
                  ("d", ["e"]);
                  ("e", []);
                ])
             (4, [1; 2; 1; 1]) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [
                  ("a", ["b"; "c"]);
                  ("b", ["c"; "d"]);
                  ("c", ["b"]);
                  ("d", ["e"]);
                  ("e", ["c"]);
                ])
             (2, [1; 4]) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [
                  ("a", ["b"; "c"]);
                  ("b", ["c"; "d"]);
                  ("c", ["b"]);
                  ("d", ["e"]);
                  ("e", ["a"]);
                ])
             (1, [5]) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [("a", ["b"]); ("b", ["c"]); ("c", []); ("d", []); ("e", [])])
             (5, [1; 1; 1; 1; 1]) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test
                [
                  ("1", ["0"]);
                  ("0", ["2"]);
                  ("2", ["1"]);
                  ("0", ["3"]);
                  ("3", ["4"]);
                ])
             (3, [3; 1; 1]) );
         (* http://algs4.cs.princeton.edu/42digraph/largeDG.txt *)
         (* __LOC__ >:: begin fun _ -> *)
         (*   OUnit.assert_equal (read_file "largeDG.txt") 25 *)
         (* end *)
         (* ; *)
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test2
                [("a", ["b"; "c"]); ("b", ["c"; "d"]); ("c", ["b"]); ("d", [])])
             [|[|"d"|]; [|"b"; "c"|]; [|"a"|]|] );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (test2
                [
                  ("a", ["b"]);
                  ("b", ["c"]);
                  ("c", ["d"]);
                  ("d", ["e"]);
                  ("e", []);
                ])
             [|[|"e"|]; [|"d"|]; [|"c"|]; [|"b"|]; [|"a"|]|] );
       ]
