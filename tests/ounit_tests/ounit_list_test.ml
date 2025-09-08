let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal
let printer_int_list xs =
  Format.asprintf "%a"
    (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space)
    xs
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (Ext_list.flat_map [1; 2] (fun x -> [x; x]))
             [1; 1; 2; 2] );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (Ext_list.flat_map_append [1; 2] [3; 4] (fun x -> [x; x]))
             [1; 1; 2; 2; 3; 4] );
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
             (Ext_list.flat_map_append [1; 2] [false; false] (fun x ->
                  if x mod 2 = 0 then [true] else []))
             [true; false; false] );
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
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             (Ext_list.assoc_by_int [(2, "x"); (3, "y"); (1, "z")] 1 None)
             "z" );
         ( __LOC__ >:: fun _ ->
           Ounit_tests_util.assert_raise_any (fun _ ->
               Ext_list.assoc_by_int [(2, "x"); (3, "y"); (1, "z")] 11 None) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal (Ext_list.length_compare [0; 0; 0] 3) `Eq;
           OUnit.assert_equal (Ext_list.length_compare [0; 0; 0] 1) `Gt;
           OUnit.assert_equal (Ext_list.length_compare [0; 0; 0] 4) `Lt;
           OUnit.assert_equal (Ext_list.length_compare [] (-1)) `Gt;
           OUnit.assert_equal (Ext_list.length_compare [] 0) `Eq );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Ext_list.length_larger_than_n [1; 2] [1] 1);
           OUnit.assert_bool __LOC__
             (Ext_list.length_larger_than_n [1; 2] [1; 2] 0);
           OUnit.assert_bool __LOC__ (Ext_list.length_larger_than_n [1; 2] [] 2)
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__ (Ext_list.length_ge [1; 2; 3] 3);
           OUnit.assert_bool __LOC__ (Ext_list.length_ge [] 0);
           OUnit.assert_bool __LOC__ (not (Ext_list.length_ge [] 1)) );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal in

           let f p x = Ext_list.exclude_with_val x p in
           f (fun x -> x = 1) [1; 2; 3] =~ Some [2; 3];
           f (fun x -> x = 4) [1; 2; 3] =~ None;
           f (fun x -> x = 2) [1; 2; 3; 2] =~ Some [1; 3];
           f (fun x -> x = 2) [1; 2; 2; 3; 2] =~ Some [1; 3];
           f (fun x -> x = 2) [2; 2; 2] =~ Some [];
           f (fun x -> x = 3) [2; 2; 2] =~ None );
       ]
