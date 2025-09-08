let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

(* open Ext_json *)

let v = Vec_int.init 10 (fun i -> i)
let ( =~ ) x y =
  OUnit.assert_equal ~cmp:(Vec_int.equal (fun (x : int) y -> x = y)) x y
let ( =~~ ) x y =
  OUnit.assert_equal
    ~cmp:(Vec_int.equal (fun (x : int) y -> x = y))
    x (Vec_int.of_array y)

let suites =
  __FILE__
  >::: [
         (* idea
            [%loc "inplace filter" ] --> __LOC__ ^ "inplace filter"
            or "inplace filter" [@bs.loc]
         *)
         ( "inplace_filter " ^ __LOC__ >:: fun _ ->
           v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];

           ignore @@ Vec_int.push v 32;
           let capacity = Vec_int.capacity v in
           v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 32|];
           Vec_int.inplace_filter (fun x -> x mod 2 = 0) v;
           v =~~ [|0; 2; 4; 6; 8; 32|];
           Vec_int.inplace_filter (fun x -> x mod 3 = 0) v;
           v =~~ [|0; 6|];
           Vec_int.inplace_filter (fun x -> x mod 3 <> 0) v;
           v =~~ [||];
           OUnit.assert_equal (Vec_int.capacity v) capacity;
           Vec_int.compact v;
           OUnit.assert_equal (Vec_int.capacity v) 0 );
         ( "inplace_filter_from " ^ __LOC__ >:: fun _ ->
           let v = Vec_int.of_array (Array.init 10 (fun i -> i)) in
           v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|];
           Vec_int.push v 96;
           Vec_int.inplace_filter_from 2 (fun x -> x mod 2 = 0) v;
           v =~~ [|0; 1; 2; 4; 6; 8; 96|];
           Vec_int.inplace_filter_from 2 (fun x -> x mod 3 = 0) v;
           v =~~ [|0; 1; 6; 96|];
           Vec_int.inplace_filter (fun x -> x mod 3 <> 0) v;
           v =~~ [|1|];
           Vec_int.compact v;
           OUnit.assert_equal (Vec_int.capacity v) 1 );
         ( "map " ^ __LOC__ >:: fun _ ->
           let v = Vec_int.of_array (Array.init 1000 (fun i -> i)) in
           Vec_int.map succ v =~~ Array.init 1000 succ;
           OUnit.assert_bool __LOC__ (Vec_int.exists (fun x -> x >= 999) v);
           OUnit.assert_bool __LOC__
             (not (Vec_int.exists (fun x -> x > 1000) v));
           OUnit.assert_equal (Vec_int.last v) 999 );
         ( __LOC__ >:: fun _ ->
           let count = 1000 in
           let init_array = Array.init count (fun i -> i) in
           let u = Vec_int.of_array init_array in
           let v =
             Vec_int.inplace_filter_with
               (fun x -> x mod 2 = 0)
               ~cb_no:(fun a b -> Set_int.add b a)
               Set_int.empty u
           in
           let even, odd =
             init_array |> Array.to_list
             |> List.partition (fun x -> x mod 2 = 0)
           in
           OUnit.assert_equal (Set_int.elements v) odd;
           u =~~ Array.of_list even );
         ( "filter" ^ __LOC__ >:: fun _ ->
           let v = Vec_int.of_array [|1; 2; 3; 4; 5; 6|] in
           ( v |> Vec_int.filter (fun x -> x mod 3 = 0) |> fun x ->
             x =~~ [|3; 6|] );
           v =~~ [|1; 2; 3; 4; 5; 6|];
           Vec_int.pop v;
           v =~~ [|1; 2; 3; 4; 5|];
           let count = ref 0 in
           let len = Vec_int.length v in
           while not (Vec_int.is_empty v) do
             Vec_int.pop v;
             incr count
           done;
           OUnit.assert_equal len !count );
         ( __LOC__ >:: fun _ ->
           let count = 100 in
           let v = Vec_int.of_array (Array.init count (fun i -> i)) in
           OUnit.assert_bool __LOC__
             (try
                Vec_int.delete v count;
                false
              with _ -> true);
           for i = count - 1 downto 10 do
             Vec_int.delete v i
           done;
           v =~~ [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9|] );
         ( "sub" ^ __LOC__ >:: fun _ ->
           let v = Vec_int.make 5 in
           OUnit.assert_bool __LOC__
             (try
                ignore @@ Vec_int.sub v 0 2;
                false
              with Invalid_argument _ -> true);
           Vec_int.push v 1;
           OUnit.assert_bool __LOC__
             (try
                ignore @@ Vec_int.sub v 0 2;
                false
              with Invalid_argument _ -> true);
           Vec_int.push v 2;
           Vec_int.sub v 0 2 =~~ [|1; 2|] );
         ( "reserve" ^ __LOC__ >:: fun _ ->
           let v = Vec_int.empty () in
           Vec_int.reserve v 1000;
           for i = 0 to 900 do
             Vec_int.push v i
           done;
           OUnit.assert_equal (Vec_int.length v) 901;
           OUnit.assert_equal (Vec_int.capacity v) 1000 );
         ( "capacity" ^ __LOC__ >:: fun _ ->
           let v = Vec_int.of_array [|3|] in
           Vec_int.reserve v 10;
           v =~~ [|3|];
           Vec_int.push v 1;
           Vec_int.push v 2;
           Vec_int.push v 5;
           v =~~ [|3; 1; 2; 5|];
           OUnit.assert_equal (Vec_int.capacity v) 10;
           for i = 0 to 5 do
             Vec_int.push v i
           done;
           v =~~ [|3; 1; 2; 5; 0; 1; 2; 3; 4; 5|];
           Vec_int.push v 100;
           v =~~ [|3; 1; 2; 5; 0; 1; 2; 3; 4; 5; 100|];
           OUnit.assert_equal (Vec_int.capacity v) 20 );
         ( __LOC__ >:: fun _ ->
           let empty = Vec_int.empty () in
           Vec_int.push empty 3;
           empty =~~ [|3|] );
         ( __LOC__ >:: fun _ ->
           let lst = [1; 2; 3; 4] in
           let v = Vec_int.of_list lst in
           OUnit.assert_equal
             (Vec_int.map_into_list (fun x -> x + 1) v)
             (Ext_list.map lst (fun x -> x + 1)) );
         ( __LOC__ >:: fun _ ->
           let v = Vec_int.make 4 in
           Vec_int.push v 1;
           Vec_int.push v 2;
           Vec_int.reverse_in_place v;
           v =~~ [|2; 1|] );
       ]
