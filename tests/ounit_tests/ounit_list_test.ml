let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal
let printer_int_list xs =
  Format.asprintf "%a"
    (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space)
    xs
let suites =
  __FILE__
  >::: [
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
             (Ext_list.flat_map [1; 2] (fun x -> [x; x]))
             [1; 1; 2; 2] );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal ~printer:printer_int_list in
           Ext_list.flat_map [] (fun x -> [succ x]) =~ [];
           Ext_list.flat_map [1] (fun x -> [x; succ x]) =~ [1; 2];
           Ext_list.flat_map [1; 2] (fun x -> [x; succ x]) =~ [1; 2; 2; 3];
           Ext_list.flat_map [1; 2; 3] (fun x -> [x; succ x])
           =~ [1; 2; 2; 3; 3; 4] );
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
