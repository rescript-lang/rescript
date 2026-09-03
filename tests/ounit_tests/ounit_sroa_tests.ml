open OUnit

let loc = Location.none

let read block index =
  Lambda.prim
    ~primitive:(Pfield (index, Fld_tuple))
    ~args:[Lambda.var block]
    loc

let write block index value =
  Lambda.prim
    ~primitive:(Psetfield (index, Fld_record_set (string_of_int index)))
    ~args:[Lambda.var block; value]
    loc

let pair_info =
  Lambda.Blk_record
    {
      fields = [|("left", false); ("right", false)|];
      mutable_flag = Asttypes.Mutable;
    }

let suites =
  __FILE__
  >::: [
         ( "replaces multiple fields in initializer order" >:: fun _ ->
           let block = Ident.create "pair" in
           let initializers =
             [
               Lambda.const (Lambda.const_int 10);
               Lambda.const (Lambda.const_int 20);
             ]
           in
           let body =
             Lambda.seq
               (write block 1 (Lambda.const (Lambda.const_int 30)))
               (read block 0)
           in
           match
             Lam_pass_sroa.replace ~block ~info:pair_info ~initializers body
           with
           | Some
               (Llet
                  ( Variable,
                    field0,
                    Lconst (Const_int 10l),
                    Llet
                      ( Variable,
                        field1,
                        Lconst (Const_int 20l),
                        Lsequence (Lassign (assigned, Lconst _), Lvar returned)
                      ) )) ->
             assert_equal "pair_left" (Ident.name field0);
             assert_equal "pair_right" (Ident.name field1);
             assert_bool "field one gets its own binding"
               (Ident.same field1 assigned);
             assert_bool "the read uses field zero" (Ident.same field0 returned)
           | _ -> assert_failure "expected two scalar bindings" );
         ( "replaces fields captured by a closure" >:: fun _ ->
           let block = Ident.create "pair" in
           let closure =
             Lambda.function_ ~loc ~attr:Lambda.default_function_attribute
               ~params:[]
               ~body:(write block 1 (read block 0))
           in
           match
             Lam_pass_sroa.replace ~block ~info:pair_info
               ~initializers:
                 [
                   Lambda.const (Lambda.const_int 10);
                   Lambda.const (Lambda.const_int 20);
                 ]
               closure
           with
           | Some
               (Llet
                  ( Variable,
                    field0,
                    _,
                    Llet
                      ( Variable,
                        field1,
                        _,
                        Lfunction {body = Lassign (assigned, Lvar returned)} )
                  )) ->
             assert_bool "the closure writes field one"
               (Ident.same field1 assigned);
             assert_bool "the closure reads field zero"
               (Ident.same field0 returned)
           | _ -> assert_failure "expected scalar closure captures" );
         ( "preserves unrelated closure subtrees" >:: fun _ ->
           let block = Ident.create "pair" in
           let unrelated =
             Lambda.function_ ~loc ~attr:Lambda.default_function_attribute
               ~params:[] ~body:Lambda.lambda_unit
           in
           let body = Lambda.seq (read block 0) unrelated in
           match
             Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
               ~initializers:[Lambda.const (Lambda.const_int 10)]
               body
           with
           | Some
               (Llet (Variable, _, _, Lsequence (Lvar _, preserved_unrelated)))
             ->
             assert_bool "the unrelated subtree is physically shared"
               (preserved_unrelated == unrelated)
           | _ -> assert_failure "expected a scalar read followed by a closure"
         );
         ( "rejects a whole-block use" >:: fun _ ->
           let block = Ident.create "pair" in
           assert_equal None
             (Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
                ~initializers:[Lambda.const (Lambda.const_int 10)]
                (Lambda.var block)) );
         ( "rejects an out-of-bounds field" >:: fun _ ->
           let block = Ident.create "pair" in
           assert_equal None
             (Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
                ~initializers:[Lambda.const (Lambda.const_int 10)]
                (read block 1)) );
       ]
