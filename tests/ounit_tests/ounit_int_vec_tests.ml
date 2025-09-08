let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Int_vec_util.mem 3 (Vec_int.of_list [1; 2; 3]));
           OUnit.assert_bool __LOC__
             (not @@ Int_vec_util.mem 0 (Vec_int.of_list [1; 2]));

           let v = Vec_int.make 100 in
           OUnit.assert_bool __LOC__ (not @@ Int_vec_util.mem 0 v);
           Vec_int.push v 0;
           OUnit.assert_bool __LOC__ (Int_vec_util.mem 0 v) );
         ( __LOC__ >:: fun _ ->
           let u = Vec_int.make 100 in
           Vec_int.push u 1;
           OUnit.assert_bool __LOC__ (not @@ Int_vec_util.mem 0 u);
           Vec_int.push u 0;
           OUnit.assert_bool __LOC__ (Int_vec_util.mem 0 u) );
       ]
