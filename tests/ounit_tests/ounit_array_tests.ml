let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let printer_int_array xs =
  String.concat "," (List.map string_of_int @@ Array.to_list xs)

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           Ext_array.find_and_split [|"a"; "b"; "c"|] Ext_string.equal "--"
           =~ No_split );
         ( __LOC__ >:: fun _ ->
           Ext_array.find_and_split [|"a"; "b"; "c"; "--"|] Ext_string.equal
             "--"
           =~ Split ([|"a"; "b"; "c"|], [||]) );
         ( __LOC__ >:: fun _ ->
           Ext_array.find_and_split
             [|"--"; "a"; "b"; "c"; "--"|]
             Ext_string.equal "--"
           =~ Split ([||], [|"a"; "b"; "c"; "--"|]) );
         ( __LOC__ >:: fun _ ->
           Ext_array.find_and_split
             [|"u"; "g"; "--"; "a"; "b"; "c"; "--"|]
             Ext_string.equal "--"
           =~ Split ([|"u"; "g"|], [|"a"; "b"; "c"; "--"|]) );
         ( __LOC__ >:: fun _ ->
           Ext_array.reverse [|1; 2|] =~ [|2; 1|];
           Ext_array.reverse [||] =~ [||] );
         ( __LOC__ >:: fun _ ->
           let ( =~ ) = OUnit.assert_equal ~printer:printer_int_array in
           let k x y = Ext_array.of_list_map y x in
           k succ [] =~ [||];
           k succ [1] =~ [|2|];
           k succ [1; 2; 3] =~ [|2; 3; 4|];
           k succ [1; 2; 3; 4] =~ [|2; 3; 4; 5|];
           k succ [1; 2; 3; 4; 5] =~ [|2; 3; 4; 5; 6|];
           k succ [1; 2; 3; 4; 5; 6] =~ [|2; 3; 4; 5; 6; 7|];
           k succ [1; 2; 3; 4; 5; 6; 7] =~ [|2; 3; 4; 5; 6; 7; 8|] );
         ( __LOC__ >:: fun _ ->
           Ext_array.to_list_map_acc [|1; 2; 3; 4; 5; 6|] [1; 2; 3] (fun x ->
               if x mod 2 = 0 then Some x else None)
           =~ [2; 4; 6; 1; 2; 3] );
         ( __LOC__ >:: fun _ ->
           Ext_array.to_list_map_acc [|1; 2; 3; 4; 5; 6|] [] (fun x ->
               if x mod 2 = 0 then Some x else None)
           =~ [2; 4; 6] );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Ext_array.for_all2_no_exn [|1; 2; 3|] [|1; 2; 3|] ( = )) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__ (Ext_array.for_all2_no_exn [||] [||] ( = ));
           OUnit.assert_bool __LOC__
             (not @@ Ext_array.for_all2_no_exn [||] [|1|] ( = )) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (not (Ext_array.for_all2_no_exn [|1; 2; 3|] [|1; 2; 33|] ( = ))) );
       ]
