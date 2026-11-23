let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let printer_int_list xs =
  Format.asprintf "%a"
    (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space)
    xs

(* [l] without its first [k] elements, as the cells of [l] itself *)
let rec drop k l = if k = 0 then l else drop (k - 1) (List.tl l)

let sharing_lengths = [0; 1; 999; 1000; 1001; 1002; 5000]

(* Indices to change: none, or ones before, at and past the depth (1000) at
   which the sharing maps switch from recursion to a loop *)
let sharing_changes n =
  List.map
    (List.filter (fun i -> i >= 0 && i < n))
    [[]; [0]; [500]; [999]; [1000]; [1001]; [3000]; [n - 1]; [10; 1500; 4000]]

(* Checks a sharing map against [reference] on lists of every length in
   [sharing_lengths] with every set of changes from [sharing_changes]: the
   result, the order in which elements are visited, that an unchanged list is
   returned as it is, and that the tail after the last change is shared. *)
let check_sharing_map ~map_sharing ~reference ~change =
  List.iter
    (fun n ->
      let l = List.init n Fun.id in
      List.iter
        (fun changed ->
          let visited = ref [] in
          let f x =
            visited := x :: !visited;
            if List.mem x changed then change x else [x]
          in
          let mapped = map_sharing l f in
          let case =
            Printf.sprintf "n=%d changed=[%s]" n
              (String.concat ";" (List.map string_of_int changed))
          in
          OUnit.assert_equal ~msg:(case ^ ": result") ~printer:printer_int_list
            (reference l (fun x -> if List.mem x changed then change x else [x]))
            mapped;
          OUnit.assert_equal ~msg:(case ^ ": order") ~printer:printer_int_list l
            (List.rev !visited);
          match List.rev changed with
          | [] ->
            OUnit.assert_bool (case ^ ": unchanged list shared") (mapped == l)
          | last :: _ ->
            let rest = n - last - 1 in
            OUnit.assert_bool
              (case ^ ": unchanged tail shared")
              (drop (List.length mapped - rest) mapped == drop (last + 1) l))
        (sharing_changes n))
    sharing_lengths

let suites =
  __FILE__
  >::: [
         ( "map_sharing" >:: fun _ ->
           check_sharing_map
             ~map_sharing:(fun l f ->
               Ext_list.map_sharing l (fun x -> List.hd (f x)))
             ~reference:(fun l f -> List.map (fun x -> List.hd (f x)) l)
             ~change:(fun x -> [x + 10_000]) );
         ( "map_snd_sharing" >:: fun _ ->
           List.iter
             (fun n ->
               let pairs = List.init n (fun i -> (i, string_of_int i)) in
               OUnit.assert_bool "unchanged list shared"
                 (Ext_list.map_snd_sharing pairs Fun.id == pairs);
               List.iter
                 (fun changed ->
                   let changed = List.filter (fun i -> i < n) changed in
                   let is_changed v = List.mem (int_of_string v) changed in
                   let visited = ref [] in
                   let mapped =
                     Ext_list.map_snd_sharing pairs (fun v ->
                         visited := v :: !visited;
                         if is_changed v then "changed" else v)
                   in
                   OUnit.assert_equal
                     (List.map
                        (fun (k, v) ->
                          (k, if is_changed v then "changed" else v))
                        pairs)
                     mapped;
                   OUnit.assert_equal ~msg:"order" (List.map snd pairs)
                     (List.rev !visited);
                   match List.rev changed with
                   | [] -> OUnit.assert_bool "shared" (mapped == pairs)
                   | last :: _ ->
                     OUnit.assert_bool "unchanged tail shared"
                       (drop (last + 1) mapped == drop (last + 1) pairs))
                 [[500]; [999]; [1000]; [2000]; [500; 2000]])
             [999; 1000; 1001; 3000] );
         ( "concat_map_sharing drops elements" >:: fun _ ->
           check_sharing_map ~map_sharing:Ext_list.concat_map_sharing
             ~reference:(fun l f -> List.concat_map f l)
             ~change:(fun _ -> []) );
         ( "concat_map_sharing replaces elements" >:: fun _ ->
           check_sharing_map ~map_sharing:Ext_list.concat_map_sharing
             ~reference:(fun l f -> List.concat_map f l)
             ~change:(fun x -> [x + 10_000]) );
         ( "concat_map_sharing adds elements" >:: fun _ ->
           check_sharing_map ~map_sharing:Ext_list.concat_map_sharing
             ~reference:(fun l f -> List.concat_map f l)
             ~change:(fun x -> [x; x + 10_000]) );
         ( "map_to_array" >:: fun _ ->
           let ( =~ ) =
             OUnit.assert_equal ~printer:(fun xs ->
                 String.concat "," (List.map string_of_int (Array.to_list xs)))
           in
           let k x y = Ext_list.map_to_array y x in
           k succ [] =~ [||];
           k succ [1] =~ [|2|];
           k succ [1; 2; 3] =~ [|2; 3; 4|];
           k succ [1; 2; 3; 4] =~ [|2; 3; 4; 5|];
           k succ [1; 2; 3; 4; 5] =~ [|2; 3; 4; 5; 6|];
           k succ [1; 2; 3; 4; 5; 6] =~ [|2; 3; 4; 5; 6; 7|];
           k succ [1; 2; 3; 4; 5; 6; 7] =~ [|2; 3; 4; 5; 6; 7; 8|] );
         ( "sort_via_arrayf maps right to left" >:: fun _ ->
           let visited = ref [] in
           let mapped =
             Ext_list.sort_via_arrayf [3; 1; 2] Int.compare (fun value ->
                 visited := value :: !visited;
                 value * 2)
           in
           OUnit.assert_equal [2; 4; 6] mapped;
           OUnit.assert_equal [3; 2; 1] (List.rev !visited);
           OUnit.assert_equal []
             (Ext_list.sort_via_arrayf [] Int.compare (fun _ ->
                  OUnit.assert_failure "empty input must not invoke callback"))
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (Ext_list.stable_group [1; 2; 3; 4; 3] ( = ))
             [[1]; [2]; [4]; [3; 3]] );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal ~printer:printer_int_list in
           let f b _v = if b then 1 else 0 in
           Ext_list.map_last [] f =~ [];
           Ext_list.map_last [0] f =~ [1];
           Ext_list.map_last [0; 0] f =~ [0; 1];
           Ext_list.map_last [0; 0; 0] f =~ [0; 0; 1];
           Ext_list.map_last [0; 0; 0; 0] f =~ [0; 0; 0; 1];
           Ext_list.map_last [0; 0; 0; 0; 0] f =~ [0; 0; 0; 0; 1];
           Ext_list.map_last [0; 0; 0; 0; 0; 0] f =~ [0; 0; 0; 0; 0; 1];
           Ext_list.map_last [0; 0; 0; 0; 0; 0; 0] f =~ [0; 0; 0; 0; 0; 0; 1] );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (Ext_list.map_append [0; 1; 2] ["1"; "2"; "3"] (fun x ->
                  string_of_int x))
             ["0"; "1"; "2"; "1"; "2"; "3"] );
         ( __LOC__ >:: fun _ ->
           let a, b = Ext_list.split_at [1; 2; 3; 4; 5; 6] 3 in
           OUnit.assert_equal (a, b) ([1; 2; 3], [4; 5; 6]);
           OUnit.assert_equal (Ext_list.split_at [1] 1) ([1], []);
           OUnit.assert_equal (Ext_list.split_at [1; 2; 3] 2) ([1; 2], [3]) );
         ( __LOC__ >:: fun _ ->
           let printer (a, b) =
             Format.asprintf "([%a],%d)"
               (Format.pp_print_list Format.pp_print_int)
               a b
           in
           let ( =~ ) = OUnit.assert_equal ~printer in
           Ext_list.split_at_last [1; 2; 3] =~ ([1; 2], 3);
           Ext_list.split_at_last [1; 2; 3; 4; 5; 6; 7; 8]
           =~ ([1; 2; 3; 4; 5; 6; 7], 8);
           Ext_list.split_at_last [1; 2; 3; 4; 5; 6; 7]
           =~ ([1; 2; 3; 4; 5; 6], 7) );
       ]
