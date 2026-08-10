let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump

module String_hash = Hash.Make (struct
  type t = string

  let equal = Ext_string.equal
  let hash = Ext_platform_primitives.hash_string
end)

let suites =
  __FILE__
  >::: [
         (* __LOC__ >:: begin fun _ ->  *)
         (*   let h = String_hash.create 0 in  *)
         (*   let accu key = *)
         (*     String_hash.replace_or_init h key   succ 1 in  *)
         (*   let count = 1000 in  *)
         (*   for i = 0 to count - 1 do      *)
         (*     Array.iter accu  [|"a";"b";"c";"d";"e";"f"|]     *)
         (*   done; *)
         (*   String_hash.length h =~ 6; *)
         (*   String_hash.iter (fun _ v -> v =~ count ) h *)
         (* end; *)
         ( "add semantics " >:: fun _ ->
           let h = String_hash.create 0 in
           let count = 1000 in
           for _ = 0 to 1 do
             for i = 0 to count - 1 do
               String_hash.add h (string_of_int i) i
             done
           done;
           String_hash.length h =~ 2 * count );
         ( "replace semantics" >:: fun _ ->
           let h = String_hash.create 0 in
           let count = 1000 in
           for _ = 0 to 1 do
             for i = 0 to count - 1 do
               String_hash.replace h (string_of_int i) i
             done
           done;
           String_hash.length h =~ count );
         ( __LOC__ >:: fun _ ->
           let h = String_hash.create 0 in
           let count = 10 in
           for i = 0 to count - 1 do
             String_hash.replace h (string_of_int i) i
           done;
           let xs = String_hash.to_list h (fun k _ -> k) in
           let ys = List.sort compare xs in
           ys =~ ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"] );
         ( "ordered local identifiers" >:: fun _ ->
           let table = Ordered_hash_map_local_ident.create 1 in
           let identifiers =
             Array.init 100 (fun stamp ->
                 ({stamp = stamp + 1; name = "value"; flags = 0} : Ident.t))
           in
           Array.iteri
             (fun value ident ->
               Ordered_hash_map_local_ident.add table ident value)
             identifiers;
           Array.iteri
             (fun value ident ->
               OUnit.assert_equal value
                 (Ordered_hash_map_local_ident.rank table ident);
               OUnit.assert_equal value
                 (Ordered_hash_map_local_ident.find_value table ident))
             identifiers;
           OUnit.assert_equal 100 (Ordered_hash_map_local_ident.length table);
           OUnit.assert_equal identifiers
             (Ordered_hash_map_local_ident.to_sorted_array table) );
       ]
