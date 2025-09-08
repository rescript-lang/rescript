let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

module Set_poly = struct
  include Set_int
  let of_sorted_list xs = Array.of_list xs |> of_sorted_array
  let of_array l = Ext_array.fold_left l empty add
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

type ident = {stamp: int; name: string; mutable flags: int}

module Set_ident = Set.Make (struct
  type t = ident
  let compare = Stdlib.compare
end)

let compare_ident x y =
  let a = compare (x.stamp : int) y.stamp in
  if a <> 0 then a
  else
    let b = compare (x.name : string) y.name in
    if b <> 0 then b else compare (x.flags : int) y.flags

let rec add (tree : _ Set_gen.t) x =
  match tree with
  | Empty -> Set_gen.singleton x
  | Leaf v ->
    let c = compare_ident x v in
    if c = 0 then tree
    else if c < 0 then Set_gen.unsafe_two_elements x v
    else Set_gen.unsafe_two_elements v x
  | Node {l; v; r} as t ->
    let c = compare_ident x v in
    if c = 0 then t
    else if c < 0 then Set_gen.bal (add l x) v r
    else Set_gen.bal l v (add r x)

let rec mem (tree : _ Set_gen.t) x =
  match tree with
  | Empty -> false
  | Leaf v -> compare_ident x v = 0
  | Node {l; v; r} ->
    let c = compare_ident x v in
    c = 0 || mem (if c < 0 then l else r) x

module Ident_set2 = Set.Make (struct
  type t = ident
  let compare = compare_ident
end)

let bench () =
  let times = 1_000_000 in
  Ounit_tests_util.time "functor set" (fun _ ->
      let v = ref Set_ident.empty in
      for i = 0 to times do
        v := Set_ident.add {stamp = i; name = "name"; flags = -1} !v
      done;
      for i = 0 to times do
        ignore @@ Set_ident.mem {stamp = i; name = "name"; flags = -1} !v
      done);
  Ounit_tests_util.time "functor set (specialized)" (fun _ ->
      let v = ref Ident_set2.empty in
      for i = 0 to times do
        v := Ident_set2.add {stamp = i; name = "name"; flags = -1} !v
      done;
      for i = 0 to times do
        ignore @@ Ident_set2.mem {stamp = i; name = "name"; flags = -1} !v
      done);

  Ounit_tests_util.time "poly set" (fun _ ->
      let module Set_poly = Set_ident in
      let v = ref Set_poly.empty in
      for i = 0 to times do
        v := Set_poly.add {stamp = i; name = "name"; flags = -1} !v
      done;
      for i = 0 to times do
        ignore @@ Set_poly.mem {stamp = i; name = "name"; flags = -1} !v
      done);
  Ounit_tests_util.time "poly set (specialized)" (fun _ ->
      let v = ref Set_gen.empty in
      for i = 0 to times do
        v := add !v {stamp = i; name = "name"; flags = -1}
      done;
      for i = 0 to times do
        ignore @@ mem !v {stamp = i; name = "name"; flags = -1}
      done)
