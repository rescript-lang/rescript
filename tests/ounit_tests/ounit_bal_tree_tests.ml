let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

module Set_poly = struct
  include Set_int
  let of_sorted_list xs = Array.of_list xs |> of_sorted_array
  let of_array l = Array.fold_left add empty l
end
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Set_poly.invariant
                (Set_poly.of_array (Array.init 1000 (fun n -> n)))) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Set_poly.invariant
                (Set_poly.of_array (Array.init 1000 (fun n -> 1000 - n)))) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Set_poly.invariant
                (Set_poly.of_array (Array.init 1000 (fun _ -> Random.int 1000))))
         );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (Set_poly.invariant
                (Set_poly.of_sorted_list
                   (Array.to_list (Array.init 1000 (fun n -> n))))) );
         ( __LOC__ >:: fun _ ->
           let arr = Array.init 1000 (fun n -> n) in
           let set = Set_poly.of_sorted_array arr in
           OUnit.assert_bool __LOC__ (Set_poly.invariant set);
           OUnit.assert_equal 1000 (Set_poly.cardinal set) );
         ( __LOC__ >:: fun _ ->
           for i = 0 to 200 do
             let arr = Array.init i (fun n -> n) in
             let set = Set_poly.of_sorted_array arr in
             OUnit.assert_bool __LOC__ (Set_poly.invariant set);
             OUnit.assert_equal i (Set_poly.cardinal set)
           done );
         ( __LOC__ >:: fun _ ->
           let arr_size = 200 in
           let arr_sets = Array.make 200 Set_poly.empty in
           for i = 0 to arr_size - 1 do
             let size = Random.int 1000 in
             let arr = Array.init size (fun n -> n) in
             arr_sets.(i) <- Set_poly.of_sorted_array arr
           done;
           let large = Array.fold_left Set_poly.union Set_poly.empty arr_sets in
           OUnit.assert_bool __LOC__ (Set_poly.invariant large) );
         ( __LOC__ >:: fun _ ->
           let arr_size = 1_00_000 in
           let v = ref Set_int.empty in
           for _ = 0 to arr_size - 1 do
             let size = Random.int 0x3FFFFFFF in
             v := Set_int.add !v size
           done;
           OUnit.assert_bool __LOC__ (Set_int.invariant !v) );
       ]
