let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal

type id = {stamp: int; name: string; mutable flags: int} (* = Ident.t *)
let hash id = Ext_platform_primitives.hash_stamp_and_name id.stamp id.name
let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           Ext_platform_primitives.hash_int 0 =~ Hashtbl.hash 0 );
         ( __LOC__ >:: fun _ ->
           Ext_platform_primitives.hash_int max_int =~ Hashtbl.hash max_int );
         ( __LOC__ >:: fun _ ->
           Ext_platform_primitives.hash_int max_int =~ Hashtbl.hash max_int );
         ( __LOC__ >:: fun _ ->
           Ext_platform_primitives.hash_string
             "The quick brown fox jumps over the lazy dog"
           =~ Hashtbl.hash "The quick brown fox jumps over the lazy dog" );
         ( __LOC__ >:: fun _ ->
           Array.init 100 (fun i -> String.make i 'a')
           |> Array.iter (fun x ->
               Ext_platform_primitives.hash_string x =~ Hashtbl.hash x) );
         ( __LOC__ >:: fun _ ->
           (* only string matters here *)
           hash {stamp = 0; name = "Pervasives"; flags = 0}
           =~ Ext_platform_primitives.hash_string "Pervasives";
           hash {stamp = 0; name = "UU"; flags = 0}
           =~ Ext_platform_primitives.hash_string "UU" );
         ( __LOC__ >:: fun _ ->
           let v = Array.init 20 (fun i -> i) in
           let u = Array.init 30 (fun i -> 0 - i) in
           Ext_platform_primitives.int_unsafe_blit v 0 u 10 20;
           OUnit.assert_equal u
             (Array.init 30 (fun i -> if i < 10 then -i else i - 10)) );
       ]
