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

let object_discriminant field_name values =
  Variant_runtime.ObjectDiscriminant
    {
      field_name;
      domain =
        {
          literals = List.map (fun value -> Variant_runtime.String value) values;
          shapes = [];
        };
      excluded = {literals = []; shapes = []};
    }

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
    ( "pattern scopes refresh together when Lambda is copied" >:: fun _ ->
      let scope = Lambda.fresh_pattern_scope ~unit_name:"Imported" in
      let base = Ident.create "payload" in
      let field =
        Lambda.Fld_record_pattern {field = Lambda.fld_record "kind"; scope}
      in
      let shape = object_discriminant "kind" ["a"; "b"] in
      let case = block "Many" shape in
      let original =
        variant_switch ~constructors:[case]
          ~cases:[(case, "many")]
          ~default:None
      in
      let facts =
        match original.sw_dispatch with
        | Lambda.Switch_variant facts -> facts
        | Switch_direct | Switch_variant_matching _ -> assert false
      in
      let projection =
        Lambda.prim
          ~primitive:(Pfield (0, field))
          ~args:[Lambda.var base]
          Location.none
      in
      let lambda =
        Lambda.switch (Lambda.var base)
          {
            original with
            sw_dispatch = Switch_variant_matching (facts, scope);
            sw_blocks = [(Switch_constructor case, projection)];
          }
      in
      let copy_scope () =
        match Lam_bounded_vars.rewrite (Hash_ident.create 1) lambda with
        | Lambda.Lswitch
            ( _,
              {
                sw_dispatch = Switch_variant_matching (_, fresh);
                sw_blocks =
                  [
                    ( _,
                      Lprim
                        {
                          primitive =
                            Pfield
                              (_, Fld_record_pattern {scope = projected; field});
                        } );
                  ];
              } ) ->
          assert_bool "copied switch and projection share their fresh identity"
            (fresh = projected);
          assert_bool "copy has a new identity" (fresh <> scope);
          Some "kind" =~ Lambda.str_of_field_info field;
          fresh
        | _ -> assert_failure "expected scoped switch and projection"
      in
      assert_bool "different inlined copies have distinct scopes"
        (copy_scope () <> copy_scope ()) );
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
    ( "object discrimination uses emitted strings and runtime field names"
    >:: fun _ ->
      let first =
        block "First" (object_discriminant "wire-kind" ["first"; "alias"])
      in
      let second =
        block "Second" (object_discriminant "wire-kind" ["second"])
      in
      let sw =
        variant_switch ~constructors:[first; second]
          ~cases:[(first, "first"); (second, "second")]
          ~default:None
      in
      let record value : Lambda.structured_constant =
        Const_block
          ( Lambda.blk_record [|("wire-kind", false)|] Asttypes.Immutable,
            [value] )
      in
      assert_fold sw (record (Const_string "second")) "second";
      assert_fold sw (record (Const_polyvar "alias")) "first";
      assert_fold sw
        (record
           (Const_constructor
              {name = "SourceName"; literal = Some (String "first")}))
        "first";
      let runtime =
        match first with
        | Block runtime -> runtime
        | Constant _ -> assert false
      in
      let inlined =
        Lambda.blk_record_inlined
          [|("wire-kind", false)|]
          "First" 2 ~runtime Asttypes.Immutable
      in
      (* The source constructor says First, but only the runtime string counts. *)
      assert_fold sw (Const_block (inlined, [Const_string "second"])) "second";
      let partial =
        variant_switch ~constructors:[first; second]
          ~cases:[(first, "first")]
          ~default:(Some "default")
      in
      assert_fold partial (record (Const_string "second")) "default";
      assert_deferred partial (record (Const_string "unknown"));
      assert_deferred partial
        (Const_block
           ( Lambda.blk_record [|("other", false)|] Asttypes.Immutable,
             [Const_string "first"] ));
      assert_deferred partial (record (Const_char 65));
      assert_deferred partial
        (Const_block (Blk_module ["wire-kind"], [Const_string "first"])) );
    ( "field domains classify emitted primitive values" >:: fun _ ->
      let discriminator name literals shapes =
        block name
          (ObjectDiscriminant
             {
               field_name = "kind";
               domain = Variant_domain.normalize {literals; shapes};
               excluded = {literals = []; shapes = []};
             })
      in
      let numbers = discriminator "Numbers" [Int 1; Float "2.5"] [] in
      let booleans = discriminator "Boolean" [] [BooleanType] in
      let nil = discriminator "Nil" [Null; Undefined] [] in
      let bigints = discriminator "Bigints" [BigInt "0x10"] [] in
      let sw =
        variant_switch
          ~constructors:[numbers; booleans; nil; bigints]
          ~cases:
            [
              (numbers, "number");
              (booleans, "boolean");
              (nil, "nil");
              (bigints, "bigint");
            ]
          ~default:None
      in
      let record value =
        Lambda.Const_block
          (Lambda.blk_record [|("kind", false)|] Asttypes.Immutable, [value])
      in
      assert_fold sw (record (Const_float "1.")) "number";
      assert_fold sw (record (Const_float "2.5")) "number";
      assert_fold sw (record Const_js_true) "boolean";
      assert_fold sw (record Const_js_false) "boolean";
      assert_fold sw (record Const_js_null) "nil";
      assert_fold sw (record (Const_js_undefined {is_unit = false})) "nil";
      assert_fold sw (record (Const_bigint (true, "16"))) "bigint";
      assert_fold sw
        (record
           (Const_constructor {name = "Hex"; literal = Some (BigInt "0x10")}))
        "bigint" );
    ( "field literal exclusions survive partial matches" >:: fun _ ->
      let exact =
        block "Exact"
          (ObjectDiscriminant
             {
               field_name = "kind";
               domain = Variant_domain.singleton_literal (String "exact");
               excluded = {literals = []; shapes = []};
             })
      in
      let broad =
        block "Broad"
          (ObjectDiscriminant
             {
               field_name = "kind";
               domain = Variant_domain.singleton_shape StringType;
               excluded = Variant_domain.singleton_literal (String "exact");
             })
      in
      let sw =
        variant_switch ~constructors:[exact; broad]
          ~cases:[(broad, "broad")]
          ~default:(Some "default")
      in
      let record value =
        Lambda.Const_block
          ( Lambda.blk_record [|("kind", false)|] Asttypes.Immutable,
            [Const_string value] )
      in
      assert_fold sw (record "exact") "default";
      assert_fold sw (record "other") "broad" );
    ( "numeric field exclusions use emitted equality" >:: fun _ ->
      let make name domain excluded =
        block name (ObjectDiscriminant {field_name = "kind"; domain; excluded})
      in
      let exact_domain = Variant_domain.singleton_literal (Int 1) in
      let exact = make "Exact" exact_domain {literals = []; shapes = []} in
      let broad =
        make "Broad" (Variant_domain.singleton_shape FloatType) exact_domain
      in
      let sw =
        variant_switch ~constructors:[exact; broad]
          ~cases:[(broad, "broad")]
          ~default:(Some "default")
      in
      let record value =
        Lambda.Const_block
          (Lambda.blk_record [|("kind", false)|] Asttypes.Immutable, [value])
      in
      assert_fold sw (record (Const_float "1.")) "default";
      assert_fold sw (record (Const_int 2l)) "broad";
      assert_fold sw (record (Const_float "nan")) "broad";
      assert_fold sw (record (Const_float "infinity")) "broad" );
    ( "field object domains exclude arrays and accept unions" >:: fun _ ->
      let make name shapes =
        block name
          (ObjectDiscriminant
             {
               field_name = "kind";
               domain = Variant_domain.normalize {literals = []; shapes};
               excluded = {literals = []; shapes = []};
             })
      in
      let object_case = make "Object" [ObjectType] in
      let array_case = make "Array" [InstanceType Array] in
      let sw =
        variant_switch ~constructors:[object_case; array_case]
          ~cases:[(object_case, "object"); (array_case, "array")]
          ~default:None
      in
      let record value =
        Lambda.Const_block
          (Lambda.blk_record [|("kind", false)|] Asttypes.Immutable, [value])
      in
      let object_value =
        Lambda.Const_block
          ( Lambda.blk_record [|("value", false)|] Asttypes.Immutable,
            [Const_int 1l] )
      in
      let array_value = Lambda.Const_block (Blk_tuple, [Const_int 1l]) in
      assert_fold sw (record object_value) "object";
      assert_fold sw (record array_value) "array";
      let union = make "Union" [ObjectType; InstanceType Array] in
      let sw =
        variant_switch ~constructors:[union]
          ~cases:[(union, "union")]
          ~default:None
      in
      assert_fold sw (record object_value) "union";
      assert_fold sw (record array_value) "union" );
    ( "object discrimination respects declared array cases" >:: fun _ ->
      let array = block "Array" (InstanceType Array) in
      let first = block "First" (object_discriminant "0" ["first"]) in
      let second = block "Second" (object_discriminant "0" ["second"]) in
      let value : Lambda.structured_constant =
        Const_block
          ( Lambda.blk_record [|("0", false)|] Asttypes.Immutable,
            [Const_string "first"] )
      in
      let sw =
        variant_switch ~constructors:[array; first; second]
          ~cases:[(array, "array"); (first, "first"); (second, "second")]
          ~default:None
      in
      assert_fold sw value "array";
      let sw =
        variant_switch ~constructors:[first; second]
          ~cases:[(first, "first"); (second, "second")]
          ~default:None
      in
      assert_fold sw value "first" );
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
           ( "record field folds resolve emitted names across layouts"
           >:: fun _ ->
             let fields = [|("count", false); ("wire-text", false)|] in
             let record = Lambda.blk_record fields Asttypes.Immutable in
             let inlined =
               Lambda.blk_record_inlined fields "Broad" 2
                 ~runtime:
                   (Untagged
                      {
                        tag = {name = "Broad"; literal = None};
                        block_type = ObjectType;
                      })
                 Asttypes.Immutable
             in
             List.iter
               (fun tag ->
                 let value = Lambda.const (Const_int 10l) in
                 let contents = [value; Lambda.const (Const_string "text")] in
                 let id = Ident.create "record" in
                 let fallback () =
                   OUnit.assert_failure "named field was not resolved"
                 in
                 let table = Hash_ident.create 1 in
                 Hash_ident.add table id
                   (Lam_util.kind_of_lambda_block tag contents);
                 List.iter
                   (fun info ->
                     Some 0 =~ Lambda.field_position tag 1 info;
                     value
                     =~ Lam_util.field_flatten_get fallback id 1 info table)
                   [Lambda.fld_record "count"; Lambda.fld_record_inline "count"];
                 Hash_ident.replace table id
                   (Constant
                      (Const_block (tag, [Const_int 10l; Const_string "text"])));
                 value
                 =~ Lam_util.field_flatten_get fallback id 1
                      (Lambda.fld_record_inline "count")
                      table;
                 let expression =
                   Js_exp_make.make_block tag
                     [Js_exp_make.int 10l; Js_exp_make.str "text"]
                     Immutable
                 in
                 Js_exp_make.int 10l
                 =~ Js_exp_make.record_access expression "count" 1l;
                 Js_exp_make.str "text"
                 =~ Js_exp_make.record_access expression "wire-text" 0l;
                 None
                 =~ Lambda.field_position tag 0 (Lambda.fld_record "missing"))
               [record; inlined] );
           ( "named exception fields retain their identity-slot offset"
           >:: fun _ ->
             let tag =
               Lambda.blk_record_ext [|"count"; "text"|] Asttypes.Immutable
             in
             Some 1
             =~ Lambda.field_position tag 2
                  (Lambda.fld_record_extension "count");
             Some 2
             =~ Lambda.field_position tag 1 (Lambda.fld_record_extension "text");
             let expression =
               Js_exp_make.make_block tag
                 [
                   Js_exp_make.str "Exception";
                   Js_exp_make.int 10l;
                   Js_exp_make.str "text";
                 ]
                 Immutable
             in
             Js_exp_make.int 10l
             =~ Js_exp_make.extension_access expression (Some "count") 2l;
             Js_exp_make.str "Exception"
             =~ Js_exp_make.extension_access expression None 0l );
           ( "scalar replacement resolves reordered named reads and writes"
           >:: fun _ ->
             let block = Ident.create "record" in
             let info =
               Lambda.blk_record
                 [|("count", false); ("text", false)|]
                 Asttypes.Mutable
             in
             let body =
               Lambda.seq
                 (Lambda.prim
                    ~primitive:(Psetfield (1, Fld_record_inline_set "count"))
                    ~args:[Lambda.var block; Lambda.const (Const_int 20l)]
                    Location.none)
                 (Lambda.prim
                    ~primitive:(Pfield (1, Fld_record_inline {name = "count"}))
                    ~args:[Lambda.var block]
                    Location.none)
             in
             match
               Lam_pass_sroa.replace ~block ~info
                 ~initializers:
                   [
                     Lambda.const (Const_int 10l);
                     Lambda.const (Const_string "text");
                   ]
                 body
             with
             | Some
                 (Llet
                    ( Variable,
                      field,
                      Lconst (Const_int 10l),
                      Lsequence
                        (Lassign (written, Lconst (Const_int 20l)), Lvar read)
                    )) ->
               assert_bool "read and write must use the count field"
                 (Ident.same field written && Ident.same field read)
             | _ ->
               assert_failure
                 "expected scalar storage for the named count field" );
           ( "record array layout" >:: fun _ ->
             List.iter
               (fun (fields, expected) ->
                 OUnit.assert_equal expected
                   (Lambda.record_fields_are_array fields))
               [
                 ([||], false);
                 ([|("0", false)|], true);
                 ([|("0", true); ("1", false)|], true);
                 ([|("1", false)|], false);
                 ([|("0", false); ("2", false)|], false);
                 ([|("00", false)|], false);
                 ([|("name", false)|], false);
               ] );
           ( "regexp literals lower to raw JavaScript" >:: fun _ ->
             let expression = Ast_helper.Exp.regexp {|a\/b\d|} "ig" in
             let wire =
               Ast_mapper_to0.default_mapper.expr Ast_mapper_to0.default_mapper
                 expression
             in
             let payload_loc = {Location.none with loc_ghost = false} in
             let payload_attr =
               (Location.mknoloc "test.payload", Parsetree0.PStr [])
             in
             let legacy ?(delimiter = Some "js") ?(attrs = []) () =
               Ast_helper0.Exp.extension
                 ( Location.mknoloc "re",
                   Parsetree0.PStr
                     [
                       Ast_helper0.Str.eval
                         (Ast_helper0.Exp.constant ~loc:payload_loc ~attrs
                            (Parsetree0.Pconst_string ({|/a\/b\d/ig|}, delimiter)));
                     ] )
             in
             List.iter
               (fun wire ->
                 let expression =
                   Ast_mapper_from0.default_mapper.expr
                     Ast_mapper_from0.default_mapper wire
                 in
                 (match expression.Parsetree.pexp_desc with
                 | Pexp_regexp {pattern; flags} ->
                   OUnit.assert_equal {|a\/b\d|} pattern;
                   OUnit.assert_equal "ig" flags
                 | _ ->
                   OUnit.assert_failure
                     "AST0 regexp must leave the bridge as a literal");
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
                               ( {
                                   txt =
                                     Longident.Ldot (Lident "Stdlib_RegExp", "t");
                                 },
                                 [] );
                         } ) ->
                     raw
                   | _ ->
                     OUnit.assert_failure "expected the regexp type constraint"
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
                 in
                 let implementation =
                   Translmod.transl_implementation "RegexpTest"
                     (typed, Typedtree.Tcoerce_none)
                 in
                 let sources = ref [] in
                 let rec collect lambda =
                   (match lambda with
                   | Lambda.Lprim {primitive = Praw_js_code {code}; args = []}
                     ->
                     sources := code :: !sources
                   | _ -> ());
                   Lambda_traverse.iter collect lambda
                 in
                 collect implementation.lambda;
                 OUnit.assert_equal ~printer:Ext_obj.dump [{|/a\/b\d/ig|}]
                   !sources)
               [
                 wire;
                 legacy ();
                 legacy ~attrs:[payload_attr] ();
                 legacy ~delimiter:None ();
               ] );
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
