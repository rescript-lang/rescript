let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

let count = 2_000_000

let bench () =
  Ounit_tests_util.time "int hash set" (fun _ ->
      let v = Hash_set_int.create 2_000_000 in
      for i = 0 to count do
        Hash_set_int.add v i
      done;
      for _ = 0 to 3 do
        for i = 0 to count do
          assert (Hash_set_int.mem v i)
        done
      done);
  Ounit_tests_util.time "int hash set" (fun _ ->
      let v = Hash_set_poly.create 2_000_000 in
      for i = 0 to count do
        Hash_set_poly.add v i
      done;
      for _ = 0 to 3 do
        for i = 0 to count do
          assert (Hash_set_poly.mem v i)
        done
      done)

type id = {stamp: int; name: string; mutable flags: int} (* = Ident.t *)
let hash id = Bs_hash_stubs.hash_stamp_and_name id.stamp id.name
let suites =
  __FILE__
  >::: [
         (__LOC__ >:: fun _ -> Bs_hash_stubs.hash_int 0 =~ Hashtbl.hash 0);
         ( __LOC__ >:: fun _ ->
           Bs_hash_stubs.hash_int max_int =~ Hashtbl.hash max_int );
         ( __LOC__ >:: fun _ ->
           Bs_hash_stubs.hash_int max_int =~ Hashtbl.hash max_int );
         ( __LOC__ >:: fun _ ->
           Bs_hash_stubs.hash_string
             "The quick brown fox jumps over the lazy dog"
           =~ Hashtbl.hash "The quick brown fox jumps over the lazy dog" );
         ( __LOC__ >:: fun _ ->
           Array.init 100 (fun i -> String.make i 'a')
           |> Array.iter (fun x ->
                  Bs_hash_stubs.hash_string x =~ Hashtbl.hash x) );
         ( __LOC__ >:: fun _ ->
           (* only stamp matters here *)
           hash {stamp = 1; name = "xx"; flags = 0}
           =~ Bs_hash_stubs.hash_small_int 1;
           hash {stamp = 11; name = "xx"; flags = 0}
           =~ Bs_hash_stubs.hash_small_int 11 );
         ( __LOC__ >:: fun _ ->
           (* only string matters here *)
           hash {stamp = 0; name = "Pervasives"; flags = 0}
           =~ Bs_hash_stubs.hash_string "Pervasives";
           hash {stamp = 0; name = "UU"; flags = 0}
           =~ Bs_hash_stubs.hash_string "UU" );
         ( __LOC__ >:: fun _ ->
           let v = Array.init 20 (fun i -> i) in
           let u = Array.init 30 (fun i -> 0 - i) in
           Bs_hash_stubs.int_unsafe_blit v 0 u 10 20;
           OUnit.assert_equal u
             (Array.init 30 (fun i -> if i < 10 then -i else i - 10)) );
       ]
