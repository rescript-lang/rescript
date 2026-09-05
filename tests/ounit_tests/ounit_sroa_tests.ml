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

let debugger = Lambda.prim ~primitive:Pdebugger ~args:[] loc

let bigint_power =
  Lambda.prim ~primitive:Ppowbigint
    ~args:
      [Lambda.var (Ident.create "base"); Lambda.var (Ident.create "exponent")]
    loc

let count_debuggers lam =
  let count = ref 0 in
  let rec loop (lam : Lambda.t) =
    (match lam with
    | Lprim {primitive = Pdebugger} -> incr count
    | _ -> ());
    ignore
      (Lambda.shallow_exists
         (fun child ->
           loop child;
           false)
         lam)
  in
  loop lam;
  !count

let contains_bigint_power lam =
  let rec loop (lam : Lambda.t) =
    match lam with
    | Lprim {primitive = Ppowbigint} -> true
    | _ -> Lambda.shallow_exists loop lam
  in
  loop lam

let contains_storage lam =
  let rec loop (lam : Lambda.t) =
    match lam with
    | Llet _ | Lassign _ -> true
    | _ -> Lambda.shallow_exists loop lam
  in
  loop lam

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
             Lambda.seq (write block 1 (read block 0)) (read block 1)
           in
           match
             Lam_pass_sroa.replace ~block ~info:pair_info ~initializers body
           with
           | Some
               (Llet
                  ( Alias,
                    field0,
                    Lconst (Const_int 10l),
                    Llet
                      ( Variable,
                        field1,
                        Lconst (Const_int 20l),
                        Lsequence (Lassign (assigned, Lvar read), Lvar returned)
                      ) )) ->
             assert_equal "pair_left" (Ident.name field0);
             assert_equal "pair_right" (Ident.name field1);
             assert_bool "field one gets its own binding"
               (Ident.same field1 assigned);
             assert_bool "the assignment reads field zero"
               (Ident.same field0 read);
             assert_bool "the result reads field one"
               (Ident.same field1 returned)
           | _ -> assert_failure "expected two scalar bindings" );
         ( "replaces fields captured by a closure" >:: fun _ ->
           let block = Ident.create "pair" in
           let closure =
             Lambda.function_ ~loc ~attr:Lambda.default_function_attribute
               ~params:[]
               ~body:(Lambda.seq (write block 1 (read block 0)) (read block 1))
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
                  ( Alias,
                    field0,
                    _,
                    Llet
                      ( Variable,
                        field1,
                        _,
                        Lfunction
                          {
                            body =
                              Lsequence
                                (Lassign (assigned, Lvar read), Lvar returned);
                          } ) )) ->
             assert_bool "the closure writes field one"
               (Ident.same field1 assigned);
             assert_bool "the closure reads field zero" (Ident.same field0 read);
             assert_bool "the closure reads field one"
               (Ident.same field1 returned)
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
           | Some (Llet (Alias, _, _, Lsequence (Lvar _, preserved_unrelated)))
             ->
             assert_bool "the unrelated subtree is physically shared"
               (preserved_unrelated == unrelated)
           | _ -> assert_failure "expected a scalar read followed by a closure"
         );
         ( "keeps effectful read-only fields strict" >:: fun _ ->
           let block = Ident.create "cell" in
           let body = Lambda.seq (read block 0) (read block 0) in
           match
             Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
               ~initializers:[debugger] body
           with
           | Some
               (Llet
                  ( Strict,
                    field,
                    Lprim {primitive = Pdebugger},
                    Lsequence (Lvar first, Lvar second) )) ->
             assert_bool "both reads use the strict scalar"
               (Ident.same field first && Ident.same field second)
           | _ -> assert_failure "expected one strict scalar binding" );
         ( "removes write-only storage but preserves effects" >:: fun _ ->
           let block = Ident.create "pair" in
           let body =
             Lambda.seq (write block 0 debugger) (write block 1 debugger)
           in
           match
             Lam_pass_sroa.replace ~block ~info:pair_info
               ~initializers:[Lambda.const (Lambda.const_int 10); debugger]
               body
           with
           | Some replacement ->
             assert_equal 3 (count_debuggers replacement);
             assert_bool "write-only fields have no scalar storage"
               (not (contains_storage replacement))
           | None -> assert_failure "expected write-only fields to scalarize" );
         ( "keeps a throwing write value as an effect" >:: fun _ ->
           let block = Ident.create "cell" in
           match
             Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
               ~initializers:[Lambda.const (Lambda.Const_bigint (true, "0"))]
               (write block 0 bigint_power)
           with
           | Some replacement ->
             assert_bool "the value that may raise is still evaluated"
               (contains_bigint_power replacement)
           | None -> assert_failure "expected the write-only field to scalarize"
         );
         ( "removes write-only storage captured by a closure" >:: fun _ ->
           let block = Ident.create "cell" in
           let closure =
             Lambda.function_ ~loc ~attr:Lambda.default_function_attribute
               ~params:[] ~body:(write block 0 debugger)
           in
           match
             Lam_pass_sroa.replace ~block ~info:Lambda.ref_tag_info
               ~initializers:[debugger] closure
           with
           | Some replacement ->
             assert_equal 2 (count_debuggers replacement);
             assert_bool "the closure captures no scalar storage"
               (not (contains_storage replacement))
           | None -> assert_failure "expected the closure write to scalarize" );
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
