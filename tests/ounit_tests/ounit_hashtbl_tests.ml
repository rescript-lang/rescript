let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) = OUnit.assert_equal ~printer:Ext_obj.dump

let suites =
  __FILE__
  >::: [
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
