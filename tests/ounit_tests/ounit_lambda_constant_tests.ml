open OUnit

let ( =~ ) = OUnit.assert_equal

let runtime name : Variant_runtime.tagged_block =
  {tag = {name; literal = None}; tag_name = None}

let constructor name =
  Lambda.Blk_constructor {name; num_nonconst = 1; runtime = runtime name}

let literal name value : Variant_runtime.constructor_case =
  Constant {name; literal = Some value}

let block name shape : Variant_runtime.constructor_case =
  Block (Untagged {tag = {name; literal = None}; block_type = shape})

let action s = Lambda.const (Const_string s)

let variant_switch ~constructors ~cases ~default : Lambda.lambda_switch =
  let layout =
    Variant_runtime.make_layout
      ~configuration:{unboxed = true; tag_name = None}
      (Array.of_list constructors)
  in
  let consts, blocks =
    List.partition
      (fun (case, _) ->
        match case with
        | Variant_runtime.Constant _ -> true
        | Block _ -> false)
      cases
  in
  let keys =
    List.map (fun (case, result) ->
        (Lambda.Switch_constructor case, action result))
  in
  {
    sw_consts_full = List.length consts = Variant_runtime.num_constants layout;
    sw_consts = keys consts;
    sw_blocks_full = List.length blocks = Variant_runtime.num_blocks layout;
    sw_blocks = keys blocks;
    sw_failaction = Option.map action default;
    sw_dispatch = Switch_variant (Variant_runtime.matching_facts layout);
  }

let assert_fold sw value expected =
  action expected =~ Lambda.switch (Lambda.const value) sw

let assert_deferred sw value =
  match Lambda.switch (Lambda.const value) sw with
  | Lswitch _ -> ()
  | _ -> assert_failure "unknown runtime representation must retain dispatch"

let switch_tests =
  let primary = literal "Primary" (String "primary") in
  let color = block "Color" StringType in
  let color_switch =
    variant_switch ~constructors:[primary; color]
      ~cases:[(primary, "literal"); (color, "payload")]
      ~default:None
  in
  [
    ( "inline records remain objects" >:: fun _ ->
      let record = block "Record" ObjectType in
      let runtime =
        match record with
        | Block runtime -> runtime
        | Constant _ -> assert false
      in
      let tag =
        Lambda.blk_record_inlined
          [|("x", false)|]
          "Record" 1 ~runtime Asttypes.Immutable
      in
      let sw =
        variant_switch ~constructors:[primary; record]
          ~cases:[(primary, "literal"); (record, "record")]
          ~default:None
      in
      assert_fold sw (Const_block (tag, [Const_int 1l])) "record" );
    ( "literal values precede payload shapes" >:: fun _ ->
      assert_fold color_switch (Const_string "primary") "literal";
      assert_fold color_switch (Const_string "blue") "payload";
      assert_fold color_switch
        (Const_constructor {name = "Alias"; literal = Some (String "primary")})
        "literal" );
    ( "declaration literals missing from match select default" >:: fun _ ->
      let sw =
        variant_switch ~constructors:[primary; color]
          ~cases:[(color, "payload")]
          ~default:(Some "default")
      in
      assert_fold sw (Const_string "primary") "default";
      assert_fold sw (Const_string "blue") "payload" );
    ( "absent payload arm selects default" >:: fun _ ->
      let sw =
        variant_switch ~constructors:[primary; color]
          ~cases:[(primary, "literal")]
          ~default:(Some "default")
      in
      assert_fold sw (Const_string "blue") "default" );
    ( "ints and floats use JavaScript numeric equality" >:: fun _ ->
      let one = literal "One" (Int 1) in
      let number = block "Number" FloatType in
      let sw =
        variant_switch ~constructors:[one; number]
          ~cases:[(one, "literal"); (number, "number")]
          ~default:None
      in
      assert_fold sw (Const_float "1.") "literal";
      assert_fold sw (Const_int 1l) "literal";
      assert_fold sw (Const_float "2.5") "number" );
    ( "integer tag emission truncates to 32 bits" >:: fun _ ->
      let one = literal "One" (Int 4294967297) in
      let number = block "Number" FloatType in
      let sw =
        variant_switch ~constructors:[one; number]
          ~cases:[(one, "literal"); (number, "number")]
          ~default:None
      in
      assert_fold sw (Const_int 1l) "literal";
      assert_fold sw (Const_float "1.") "literal" );
    ( "array and object constants have distinct runtime shapes" >:: fun _ ->
      let array = block "Array" (InstanceType Array) in
      let obj = block "Object" ObjectType in
      let sw =
        variant_switch ~constructors:[array; obj]
          ~cases:[(array, "array"); (obj, "object")]
          ~default:None
      in
      assert_fold sw (Const_block (Blk_tuple, [Const_int 1l])) "array";
      assert_fold sw
        (Const_block
           ( Lambda.blk_record [|("0", false)|] Asttypes.Immutable,
             [Const_int 1l] ))
        "array";
      assert_fold sw
        (Const_block
           ( Lambda.blk_record [|("x", false)|] Asttypes.Immutable,
             [Const_int 1l] ))
        "object" );
    ( "empty list is zero at runtime" >:: fun _ ->
      let zero = literal "Zero" (Int 0) in
      let values = block "Values" UnknownType in
      let sw =
        variant_switch ~constructors:[zero; values]
          ~cases:[(zero, "zero"); (values, "values")]
          ~default:None
      in
      assert_fold sw (Const_constructor {name = "[]"; literal = None}) "zero" );
    ( "unknown values do not select default" >:: fun _ ->
      let one = literal "One" (BigInt "1") in
      let value = block "Value" UnknownType in
      let sw =
        variant_switch ~constructors:[one; value]
          ~cases:[(one, "one")]
          ~default:(Some "default")
      in
      List.iter (assert_deferred sw)
        [
          Const_bigint (false, "1");
          Const_constructor {name = "DecimalTen"; literal = Some (BigInt "1_0")};
          Const_char 65;
          Const_some (Const_int 1l);
        ] );
    ( "tagged switch still folds by constructor" >:: fun _ ->
      let runtime = runtime "Color" in
      let case = Variant_runtime.Block (Tagged runtime) in
      let sw =
        variant_switch ~constructors:[primary; case]
          ~cases:[(primary, "literal"); (case, "payload")]
          ~default:None
      in
      assert_fold sw
        (Const_block (constructor "Color", [Const_string "primary"]))
        "payload";
      let sw =
        {
          sw with
          sw_blocks = [];
          sw_blocks_full = false;
          sw_failaction = Some (action "default");
        }
      in
      assert_fold sw
        (Const_block (constructor "Color", [Const_string "primary"]))
        "default" );
  ]

let suites =
  __FILE__
  >::: switch_tests
       @ [
           ( "regexp literals lower to raw JavaScript" >:: fun _ ->
             let expression = Ast_helper.Exp.regexp {|a\/b\d|} "ig" in
             let lowered =
               Bs_builtin_ppx.mapper.expr Bs_builtin_ppx.mapper expression
             in
             let raw =
               match lowered.Parsetree.pexp_desc with
               | Pexp_constraint
                   ( raw,
                     {
                       ptyp_desc =
                         Ptyp_constr
                           ( {txt = Longident.Ldot (Lident "Stdlib_RegExp", "t")},
                             [] );
                     } ) ->
                 raw
               | _ -> OUnit.assert_failure "expected the regexp type constraint"
             in
             let structure =
               [
                 Ast_helper.Str.value Asttypes.Nonrecursive
                   [
                     Ast_helper.Vb.mk
                       (Ast_helper.Pat.var (Location.mknoloc "regexp"))
                       raw;
                   ];
               ]
             in
             let typed, _, _ =
               Typemod.type_structure Env.initial_safe_string structure
                 Location.none
             in
             let implementation =
               Translmod.transl_implementation "RegexpTest"
                 (typed, Typedtree.Tcoerce_none)
             in
             let sources = ref [] in
             let rec collect lambda =
               (match lambda with
               | Lambda.Lprim {primitive = Praw_js_code {code}; args = []} ->
                 sources := code :: !sources
               | _ -> ());
               Lambda_traverse.iter collect lambda
             in
             collect implementation.lambda;
             OUnit.assert_equal ~printer:Ext_obj.dump [{|/a\/b\d/ig|}] !sources
           );
           ( "typed string constants" >:: fun _ ->
             Lambda.const_string "value" =~ Lambda.Const_string "value" );
           ( "compiler-generated strings normalize malformed bytes" >:: fun _ ->
             let constant = Lambda.const_string "a\xffé" in
             constant =~ Lambda.Const_string "aÿé";
             match
               Lambda.prim ~primitive:Lambda.Pstringlength
                 ~args:[Lambda.const constant]
                 Location.none
             with
             | Lambda.Lconst (Lambda.Const_int length) -> 3l =~ length
             | _ -> OUnit.assert_failure "expected a folded string length" );
         ]
