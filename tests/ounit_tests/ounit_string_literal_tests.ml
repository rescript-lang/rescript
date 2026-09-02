let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let located_string txt = Location.mknoloc txt

let assert_decoded ~encoded ~expected =
  OUnit.assert_equal ~printer:Ext_obj.dump (Some expected)
    (String_literal.decode_js_escapes encoded)

let assert_template_decoded ~encoded ~expected =
  OUnit.assert_equal ~printer:Ext_obj.dump (Some expected)
    (String_literal.decode_js_template_escapes encoded)

let assert_encoded ~semantic ~expected =
  let encoded = String_literal.encode_js_string semantic in
  OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected encoded;
  assert_decoded ~encoded ~expected:semantic

let assert_invalid_backquoted_pattern encoded =
  let source = "let f = value => switch value { | `" ^ encoded ^ "` => 1 }" in
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res" ~source
  in
  OUnit.assert_bool "expected an invalid string escape" result.invalid

let assert_invalid_backquoted_pattern_after_diagnostic () =
  let source =
    {|
let invalidBigint = 0x1n
let f = value => switch value { | `\uD800` => 1 }
|}
  in
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res" ~source
  in
  OUnit.assert_equal ~printer:string_of_int 2 (List.length result.diagnostics)

let assert_invalid_tagged_template_pattern tag =
  let source =
    "let f = value => switch value { | " ^ tag ^ "`literal` => 1 }"
  in
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res" ~source
  in
  OUnit.assert_bool "expected a tagged template pattern error" result.invalid

let assert_invalid_string encoded =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:("let value = \"" ^ encoded ^ "\"")
  in
  OUnit.assert_bool "expected an invalid string escape" result.invalid

let assert_invalid_template_expression source =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:("let value = `" ^ source ^ "`")
  in
  OUnit.assert_bool "expected an invalid template escape" result.invalid

let assert_parsed_string ~source ~expected_semantic =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:("let value = \"" ^ source ^ "\"")
  in
  match result.parsetree with
  | [
   {
     pstr_desc =
       Pstr_value
         (_, [{pvb_expr = {pexp_desc = Pexp_constant (Pconst_string actual)}}]);
   };
  ] ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") source actual.source;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic
      actual.semantic
  | _ -> OUnit.assert_failure "expected a parsed string literal"

let assert_parsed_char ~for_printer ~source ~expected_semantic =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer
      ~display_filename:"StringLiteralTest.res"
      ~source:("let value = '" ^ source ^ "'")
  in
  match result.parsetree with
  | [
   {
     pstr_desc =
       Pstr_value
         ( _,
           [
             {
               pvb_expr =
                 {
                   pexp_desc =
                     Pexp_constant
                       (Pconst_char {source = actual_source; semantic});
                 };
             };
           ] );
   };
  ] ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") source actual_source;
    OUnit.assert_equal ~printer:string_of_int expected_semantic semantic
  | _ -> OUnit.assert_failure "expected a parsed character literal"

let assert_parsed_template_literal ~source =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:("let value = `" ^ source ^ "`")
  in
  match result.parsetree with
  | [
   {
     pstr_desc =
       Pstr_value
         ( _,
           [
             {
               pvb_expr =
                 {
                   pexp_desc =
                     Pexp_template {source_segments = [actual]; values = []};
                   pexp_attributes = [];
                 };
             };
           ] );
   };
  ] ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") source actual.txt
  | _ -> OUnit.assert_failure "expected an explicit template expression"

let assert_parsed_template_pattern ~source ~expected_semantic =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:("let f = value => switch value { | `" ^ source ^ "` => 1 }")
  in
  let actual = ref None in
  let mapper =
    {
      Ast_mapper.default_mapper with
      pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
          | Ppat_constant (Pconst_string {source; semantic}) ->
            actual := Some (source, semantic, pattern.ppat_attributes)
          | _ -> ());
          Ast_mapper.default_mapper.pat self pattern);
    }
  in
  ignore (mapper.structure mapper result.parsetree);
  match !actual with
  | Some (actual_source, semantic, []) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S")
      (String_literal.encode_js_string expected_semantic)
      actual_source;
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected_semantic semantic
  | _ -> OUnit.assert_failure "expected a normalized string pattern"

let assert_parsed_template () =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:"let value = `head\\n${item}tail`"
  in
  match result.parsetree with
  | [
   {
     pstr_desc =
       Pstr_value
         ( _,
           [
             {
               pvb_expr =
                 {
                   pexp_desc =
                     Pexp_template
                       {
                         source_segments;
                         values =
                           [
                             {
                               pexp_desc =
                                 Pexp_ident {txt = Longident.Lident "item"};
                             };
                           ];
                       };
                 };
             };
           ] );
   };
  ] ->
    OUnit.assert_equal ~printer:Ext_obj.dump [{|head\n|}; "tail"]
      (List.map (fun {Location.txt} -> txt) source_segments)
  | _ -> OUnit.assert_failure "expected an explicit parsed template"

let assert_invalid_json_interpolation () =
  let result =
    Res_driver.parse_implementation_from_source ~for_printer:false
      ~display_filename:"StringLiteralTest.res"
      ~source:{|let value = json`head${item}tail`|}
  in
  OUnit.assert_bool "expected JSON interpolation to be rejected" result.invalid

let assert_int_equal expected actual =
  OUnit.assert_equal ~printer:string_of_int expected actual

let assert_code_point_at string index expected =
  OUnit.assert_equal ~printer:Ext_obj.dump expected
    (String_literal.code_point_at_utf16_index string index)

let semantic_string s = Lambda.const (Lambda.Const_string s)

let lam_int i = Lambda.const (Lambda.Const_int (Int32.of_int i))

let assert_lam_int expected = function
  | Lambda.Lconst (Lambda.Const_int i) ->
    OUnit.assert_equal ~printer:Int32.to_string (Int32.of_int expected) i
  | _ -> OUnit.assert_failure "expected a folded Lambda integer"

let assert_lam_char expected = function
  | Lambda.Lconst (Lambda.Const_char actual) -> assert_int_equal expected actual
  | _ -> OUnit.assert_failure "expected a folded Lambda character"

let typed_string s =
  match Typecore.constant (Ast_helper.Const.string s) with
  | Ok constant -> constant
  | Error _ -> OUnit.assert_failure "expected a typed string constant"

let assert_typed_template ~source_segments ~expected_semantics =
  let value = Ast_helper.Exp.constant (Ast_helper.Const.string "value") in
  let expression =
    Ast_helper.Exp.template (List.map located_string source_segments) [value]
  in
  let typed =
    Typecore.type_exp Env.initial_safe_string expression
      ~context:(Some Error_message_utils.StringConcat)
  in
  begin match typed.exp_desc with
  | Texp_template
      {segments; values = [{exp_desc = Texp_constant (Const_string "value")}]}
    ->
    OUnit.assert_equal ~printer:Ext_obj.dump source_segments
      (List.map (fun ({source} : Asttypes.template_segment) -> source) segments);
    OUnit.assert_equal ~printer:Ext_obj.dump expected_semantics
      (List.map
         (fun ({semantic} : Asttypes.template_segment) -> semantic)
         segments)
  | _ -> OUnit.assert_failure "expected an explicit typed template"
  end;
  match Translcore.transl_exp typed with
  | Lprim
      {primitive = Ptemplate segments; args = [Lconst (Const_string "value")]}
    ->
    OUnit.assert_equal ~printer:Ext_obj.dump source_segments
      (List.map (fun ({source} : Asttypes.template_segment) -> source) segments);
    OUnit.assert_equal ~printer:Ext_obj.dump expected_semantics
      (List.map
         (fun ({semantic} : Asttypes.template_segment) -> semantic)
         segments)
  | _ -> OUnit.assert_failure "expected an explicit Lambda template"

let convert_typed_constant constant = Lambda.const_of_typed constant

let assert_js_string ~expected constant =
  match (Lam_compile_const.translate constant).J.expression_desc with
  | Str actual ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> OUnit.assert_failure "expected a JavaScript string expression"

let assert_external_js_string ~expected constant =
  match (Lam_compile_const.translate_arg_cst constant).J.expression_desc with
  | Str actual ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> OUnit.assert_failure "expected a JavaScript string expression"

let assert_external_json_literal ~expected constant =
  match (Lam_compile_const.translate_arg_cst constant).J.expression_desc with
  | Json_literal actual ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> OUnit.assert_failure "expected a JavaScript JSON literal expression"

let inline_string semantic =
  match Ast_external_mk.inline_string semantic with
  | Prim_inline_const constant -> constant
  | _ -> OUnit.assert_failure "expected an inline constant"

let assert_js_global ~expected (expression : J.expression) =
  match expression.expression_desc with
  | Var (Id ident) ->
    OUnit.assert_bool "expected a JavaScript global" (Ext_ident.is_js ident);
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected ident.name
  | _ -> OUnit.assert_failure "expected a JavaScript global reference"

let string_payload constant =
  Parsetree.PStr [Ast_helper.Str.eval (Ast_helper.Exp.constant constant)]

let template_payload source =
  Parsetree.PStr
    [Ast_helper.Str.eval (Ast_helper.Exp.template [located_string source] [])]

let suites =
  __FILE__
  >::: [
         ( "plain text" >:: fun _ ->
           assert_decoded ~encoded:"plain" ~expected:"plain" );
         ( "named escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\b\f\n\r\t\v\0|}
             ~expected:"\b\012\n\r\t\011\000" );
         ( "semantic strings get canonical source spelling" >:: fun _ ->
           assert_encoded ~semantic:"\b\012\n\r\t\011\000\"\\😀"
             ~expected:{|\b\f\n\r\t\v\x00\"\\😀|} );
         ( "escaped punctuation and non-escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\\\"\'\ \$\`\a|} ~expected:{|\"' $`a|};
           assert_decoded ~encoded:"\\é" ~expected:"é" );
         ( "hex escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\x61\xE9|} ~expected:"aé" );
         ( "unicode escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\u0061\u20AC|} ~expected:"a€";
           assert_decoded ~encoded:{|\u{1f600}|} ~expected:"😀";
           assert_decoded ~encoded:{|\uD83D\uDE00|} ~expected:"😀" );
         ( "malformed UTF-8 is rejected" >:: fun _ ->
           List.iter
             (fun encoded ->
               OUnit.assert_equal ~printer:Ext_obj.dump None
                 (String_literal.decode_js_escapes encoded);
               OUnit.assert_equal ~printer:Ext_obj.dump None
                 (String_literal.decode_js_template_escapes encoded))
             ["\xc0\x80"; "\xed\xa0\x80"; "\xf4\x90\x80\x80"] );
         ( "line continuations" >:: fun _ ->
           assert_decoded ~encoded:"a\\\nb" ~expected:"ab";
           assert_decoded ~encoded:"a\\\rb" ~expected:"ab";
           assert_decoded ~encoded:"a\\\r\nb" ~expected:"ab";
           List.iter
             (fun codepoint ->
               let separator = Ext_utf8.encode_codepoint codepoint in
               let encoded = "a\\" ^ separator ^ "b" in
               assert_decoded ~encoded ~expected:"ab";
               assert_template_decoded ~encoded ~expected:"ab")
             [0x2028; 0x2029] );
         ( "template line endings use JavaScript normalization" >:: fun _ ->
           assert_decoded ~encoded:"a\r\nb" ~expected:"a\r\nb";
           assert_template_decoded ~encoded:"a\rb" ~expected:"a\nb";
           assert_template_decoded ~encoded:"a\r\nb" ~expected:"a\nb";
           assert_template_decoded ~encoded:"a\\r\\nb" ~expected:"a\r\nb" );
         ( "templates reject legacy octal and decimal escapes" >:: fun _ ->
           assert_template_decoded ~encoded:{|a\0b|} ~expected:"a\000b";
           List.iter
             (fun encoded ->
               OUnit.assert_equal ~printer:Ext_obj.dump None
                 (String_literal.decode_js_template_escapes encoded))
             [{|a\1b|}; {|a\01b|}; {|a\8b|}] );
         ( "ordinary literals become semantic strings" >:: fun _ ->
           assert_parsed_string ~source:{|\x61\n\uD83D\uDE00|}
             ~expected_semantic:"a\n😀" );
         ( "template expressions preserve source spelling" >:: fun _ ->
           let encoded = {|\x61|} in
           assert_parsed_template_literal ~source:encoded;
           assert_parsed_template_pattern ~source:encoded ~expected_semantic:"a";
           let expression =
             Ast_helper.Exp.template [located_string encoded] []
           in
           match
             (Bs_builtin_ppx.mapper.expr Bs_builtin_ppx.mapper expression)
               .pexp_desc
           with
           | Pexp_template {source_segments = [actual]; values = []} ->
             OUnit.assert_equal ~printer:(Printf.sprintf "%S") encoded
               actual.txt
           | _ -> OUnit.assert_failure "expected a template expression" );
         ( "tagged templates are rejected in patterns" >:: fun _ ->
           assert_invalid_tagged_template_pattern "json";
           assert_invalid_tagged_template_pattern "js" );
         ( "interpolated templates have an explicit parser representation"
         >:: fun _ ->
           assert_parsed_template ();
           assert_invalid_json_interpolation () );
         ( "template expression escapes are parser diagnostics" >:: fun _ ->
           List.iter assert_invalid_template_expression
             [{|bad \xZZ escape|}; {|a\1b|}; {|a\01b|}; {|a\8b|}] );
         ( "interpolated templates have an explicit typed representation"
         >:: fun _ ->
           assert_typed_template ~source_segments:[{|head\n|}; {|\u0061|}]
             ~expected_semantics:["head\n"; "a"];
           assert_typed_template ~source_segments:["head\r\n"; "tail\r"]
             ~expected_semantics:["head\n"; "tail\n"] );
         ( "constant templates remain nonexpansive" >:: fun _ ->
           let expression =
             Ast_helper.Exp.template [located_string "literal"] []
           in
           let typed =
             Typecore.type_exp Env.initial_safe_string expression ~context:None
           in
           OUnit.assert_bool
             "an interpolation-free template should generalize like a string \
              constant"
             (Typecore.is_nonexpansive typed) );
         ( "invalid encoded values are rejected" >:: fun _ ->
           List.iter
             (fun encoded ->
               OUnit.assert_equal ~printer:Ext_obj.dump None
                 (String_literal.decode_js_escapes encoded))
             [
               {|trailing\|};
               {|\x6|};
               {|\xGG|};
               {|\u061|};
               {|\u{}|};
               {|\u{110000}|};
               {|\uD800|};
               {|\uDC00|};
               {|\uD800\u0041|};
               {|\uDC00\uD800|};
               "\128";
               "\195";
               "\195A";
               "\\\195";
             ] );
         ( "scanner rejects invalid braced Unicode escapes" >:: fun _ ->
           assert_invalid_string {|\u{}|};
           assert_invalid_string {|\u{110000}|} );
         ( "backquoted patterns reject lone surrogate escapes" >:: fun _ ->
           assert_invalid_backquoted_pattern {|\uD800|};
           assert_invalid_backquoted_pattern {|\uDC00|} );
         ( "invalid backquoted pattern after an earlier diagnostic" >:: fun _ ->
           assert_invalid_backquoted_pattern_after_diagnostic () );
         ( "character literals retain source and semantic forms" >:: fun _ ->
           assert_parsed_char ~for_printer:false ~source:{|\u{61}|}
             ~expected_semantic:0x61;
           assert_parsed_char ~for_printer:true ~source:{|\u{61}|}
             ~expected_semantic:0x61;
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") {e|\x00|e}
             (String_literal.encode_char_source 0x00);
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") "😀"
             (String_literal.encode_char_source 0x1f600);
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") {|\u{D800}|}
             (String_literal.encode_char_source 0xd800) );
         ( "character patterns are not tagged templates" >:: fun _ ->
           let pattern =
             Ast_helper.Pat.constant
               (Parsetree.Pconst_char {source = "a"; semantic = 0x61})
           in
           let transformed =
             Bs_builtin_ppx.mapper.pat Bs_builtin_ppx.mapper pattern
           in
           OUnit.assert_equal ~printer:Ext_obj.dump pattern.ppat_desc
             transformed.ppat_desc );
         ( "typed constants contain only semantic strings" >:: fun _ ->
           let semantic = typed_string "a\n😀" in
           OUnit.assert_equal ~printer:Ext_obj.dump
             (Asttypes.Const_string "a\n😀") semantic;
           OUnit.assert_equal ~printer:Ext_obj.dump
             (Ast_helper.Const.string "a\n😀")
             (Untypeast.constant semantic);
           OUnit.assert_equal ~printer:Ext_obj.dump
             (Error Typecore.Json_literal_outside_external)
             (Typecore.constant (Parsetree.Pconst_json {|{"answer":42}|})) );
         ( "constant backquoted attribute strings become semantic" >:: fun _ ->
           OUnit.assert_equal ~printer:Ext_obj.dump (Some "a\n😀")
             (Ast_payload.semantic_string_of_payload
                (template_payload {|\x61\n\uD83D\uDE00|}));
           OUnit.assert_equal ~printer:Ext_obj.dump (Some "a\n😀")
             (Builtin_attributes.deprecated_of_attrs
                [
                  ( Location.mkloc "deprecated" Location.none,
                    template_payload {|\x61\n\uD83D\uDE00|} );
                ]);
           OUnit.assert_equal ~printer:Ext_obj.dump None
             (Ast_payload.semantic_string_of_payload
                (string_payload (Pconst_json {|{"answer":42}|})));
           match
             Ast_payload.semantic_string_of_payload
               (template_payload {|\uD800|})
           with
           | _ -> OUnit.assert_failure "expected an invalid string escape"
           | exception Location.Error _ -> () );
         ( "error extensions accept backquoted messages" >:: fun _ ->
           let extension =
             ( Location.mknoloc "error",
               template_payload {|message\nwith context|} )
           in
           let error = Builtin_attributes.error_of_extension extension in
           OUnit.assert_equal ~printer:(Printf.sprintf "%S")
             "message\nwith context" error.msg;
           let extension_with_highlight =
             ( Location.mknoloc "error",
               Parsetree.PStr
                 [
                   Ast_helper.Str.eval
                     (Ast_helper.Exp.template
                        [located_string {|plain\nmessage|}]
                        []);
                   Ast_helper.Str.eval
                     (Ast_helper.Exp.template
                        [located_string {|highlighted\nmessage|}]
                        []);
                 ] )
           in
           let error =
             Builtin_attributes.error_of_extension extension_with_highlight
           in
           OUnit.assert_equal ~printer:(Printf.sprintf "%S")
             "highlighted\nmessage" error.if_highlight );
         ( "external string constants have explicit representations" >:: fun _ ->
           OUnit.assert_equal ~printer:Ext_obj.dump
             (External_ffi_types.Const_string "a\n😀") (inline_string "a\n😀");
           assert_external_js_string ~expected:{|\x61|}
             (External_arg_spec.cst_string {|\x61|});
           assert_external_json_literal ~expected:{|{"answer":42}|}
             (External_arg_spec.cst_json {|{"answer":42}|});
           let json = Js_exp_make.json_literal {| {answer: 42} |} in
           OUnit.assert_bool "expected JSON literals to be side-effect free"
             (Js_analyzer.no_side_effect_expression json);
           OUnit.assert_bool
             "expected allocating JSON literals not to duplicate"
             (not (Js_analyzer.is_okay_to_duplicate json));
           OUnit.assert_bool "expected JSON literals not to compare as strings"
             (not
                (Js_analyzer.eq_expression json
                   (Js_exp_make.json_literal {| {answer: 42} |})));
           (match (Js_exp_make.typeof json).expression_desc with
           | Typeof argument ->
             OUnit.assert_bool "expected typeof to preserve the JSON expression"
               (json == argument)
           | _ -> OUnit.assert_failure "expected a runtime typeof expression");
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") {| {answer: 42} |}
             (Js_dump.string_of_expression json) );
         ( "Lambda constants contain semantic strings" >:: fun _ ->
           let semantic =
             convert_typed_constant (Asttypes.Const_string "a\n😀")
           in
           OUnit.assert_equal ~printer:Ext_obj.dump (Lambda.Const_string "a\n😀")
             semantic );
         ( "JavaScript IR distinguishes strings and template literals"
         >:: fun _ ->
           assert_js_string ~expected:"a\n😀" (Lambda.Const_string "a\n😀");
           let semantic = Js_exp_make.str "a" in
           let template = Js_exp_make.template_literal ~semantic:"a" {|\x61|} in
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") {|`\x61`|}
             (Js_dump.string_of_expression template);
           (match (Js_exp_make.string_length template).expression_desc with
           | Number (Int {i = 1l}) -> ()
           | _ ->
             OUnit.assert_failure
               "expected template literal length to use its semantic value");
           (match
              (Js_exp_make.string_append semantic template).expression_desc
            with
           | String_append _ -> ()
           | _ ->
             OUnit.assert_failure
               "expected semantic strings and template segments not to fold");
           (match
              (Js_exp_make.string_append template
                 (Js_exp_make.template_literal ~semantic:"b" "b"))
                .expression_desc
            with
           | String_append _ -> ()
           | _ ->
             OUnit.assert_failure
               "expected template literals not to merge their source text");
           let tagged =
             Js_exp_make.tagged_template
               (Js_exp_make.js_global "tag")
               [{|a\n|}; " b"]
               [Js_exp_make.small_int 1]
           in
           (match tagged.expression_desc with
           | Tagged_template (_, segments, [_]) ->
             OUnit.assert_equal ~printer:Ext_obj.dump [{|a\n|}; " b"] segments
           | _ ->
             OUnit.assert_failure
               "expected tagged templates to own their encoded segments");
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") {|tag`a\n${1} b`|}
             (Js_dump.string_of_expression tagged) );
         ( "template line-ending semantics drive constant folding" >:: fun _ ->
           let source = "a\r\nb" in
           let semantic =
             match String_literal.decode_js_template_escapes source with
             | Some semantic -> semantic
             | None -> OUnit.assert_failure "expected a valid template segment"
           in
           let template = Js_exp_make.template_literal ~semantic source in
           OUnit.assert_equal ~printer:(Printf.sprintf "%S") "`a\r\nb`"
             (Js_dump.string_of_expression template);
           (match (Js_exp_make.string_length template).expression_desc with
           | Number (Int {i = 3l}) -> ()
           | _ ->
             OUnit.assert_failure
               "expected template length to use normalized line endings");
           match
             (Js_exp_make.string_equal template (Js_exp_make.str "a\nb"))
               .expression_desc
           with
           | Bool true -> ()
           | _ ->
             OUnit.assert_failure
               "expected template equality to use normalized line endings" );
         ( "interpolated templates remain explicit in JavaScript IR" >:: fun _ ->
           let segments : Asttypes.template_segment list =
             [
               {source = {|head\n\${literal}\`|}; semantic = "head\n${literal}`"};
               {source = "tail"; semantic = "tail"};
             ]
           in
           let value =
             Js_exp_make.string_append
               (Js_exp_make.js_global "left")
               (Js_exp_make.js_global "right")
           in
           let template = Js_exp_make.interpolated_template segments [value] in
           (match template.expression_desc with
           | Interpolated_template {segments = actual_segments; values = [_]} ->
             OUnit.assert_equal ~printer:Ext_obj.dump segments actual_segments
           | _ ->
             OUnit.assert_failure
               "expected an explicit JavaScript IR interpolation");
           OUnit.assert_equal ~printer:(Printf.sprintf "%S")
             {|`head\n\${literal}\`${left + right}tail`|}
             (Js_dump.string_of_expression template) );
         ( "JavaScript references are not encoded as strings" >:: fun _ ->
           let value = Js_exp_make.var (Ext_ident.create "value") in
           (match (Js_exp_make.is_array value).expression_desc with
           | Call
               ( {expression_desc = Static_index (array, "isArray", None)},
                 [argument],
                 _ ) ->
             assert_js_global ~expected:"Array" array;
             OUnit.assert_bool "expected the original argument"
               (Js_analyzer.eq_expression value argument)
           | _ -> OUnit.assert_failure "expected an Array.isArray call");
           (match
              Js_exp_make.and_
                (Js_exp_make.is_array value)
                (Js_exp_make.triple_equal value (Js_exp_make.str "literal"))
            with
           | {expression_desc = Bool false} -> ()
           | _ ->
             OUnit.assert_failure
               "expected Array.isArray simplification to remain active");
           let open Ast_untagged_variants.Dynamic_checks in
           let date = Variant_runtime.Instance.Date in
           assert_js_global ~expected:"Date"
             (Js_exp_make.emit_check
                (TagType (Variant_runtime.Untagged (InstanceType date))));
           match Js_exp_make.emit_check (IsInstanceOf (date, Expr value)) with
           | {expression_desc = Bin (InstanceOf, argument, constructor)} ->
             OUnit.assert_bool "expected the original argument"
               (Js_analyzer.eq_expression value argument);
             assert_js_global ~expected:"Date" constructor
           | _ -> OUnit.assert_failure "expected an instanceof expression" );
         ( "semantic string equality folds directly" >:: fun _ ->
           let assert_equal_result expected left right =
             match (Js_exp_make.string_equal left right).expression_desc with
             | Bool actual -> OUnit.assert_equal expected actual
             | _ -> OUnit.assert_failure "expected folded string equality"
           in
           assert_equal_result true
             (Js_exp_make.str "a\n😀")
             (Js_exp_make.str "a\n😀");
           assert_equal_result false (Js_exp_make.str "a") (Js_exp_make.str "é");
           assert_equal_result true (Js_exp_make.str "a")
             (Js_exp_make.template_literal ~semantic:"a" {|\x61|}) );
         ( "UTF-16 length" >:: fun _ ->
           assert_int_equal 0 (String_literal.utf16_length "");
           assert_int_equal 3 (String_literal.utf16_length "abc");
           assert_int_equal 1 (String_literal.utf16_length "é");
           assert_int_equal 2 (String_literal.utf16_length "😀");
           assert_int_equal 4 (String_literal.utf16_length "a😀b") );
         ( "codePointAt with UTF-16 indices" >:: fun _ ->
           assert_code_point_at "a😀b" (-1) None;
           assert_code_point_at "a😀b" 0 (Some 0x61);
           assert_code_point_at "a😀b" 1 (Some 0x1f600);
           assert_code_point_at "a😀b" 2 (Some 0xde00);
           assert_code_point_at "a😀b" 3 (Some 0x62);
           assert_code_point_at "a😀b" 4 None );
         ( "Lambda string length uses UTF-16 units" >:: fun _ ->
           Lambda.prim ~primitive:Lambda.Pstringlength
             ~args:[semantic_string "a😀b"]
             Location.none
           |> assert_lam_int 4 );
         ( "Lambda string indexing uses codePointAt semantics" >:: fun _ ->
           Lambda.prim ~primitive:Lambda.Pstringrefs
             ~args:[semantic_string "a😀b"; lam_int 1]
             Location.none
           |> assert_lam_char 0x1f600;
           Lambda.prim ~primitive:Lambda.Pstringrefu
             ~args:[semantic_string "a😀b"; lam_int 2]
             Location.none
           |> assert_lam_char 0xde00 );
         ( "JS string length uses UTF-16 units" >:: fun _ ->
           match
             (Js_exp_make.string_length (Js_exp_make.str "a😀b")).expression_desc
           with
           | J.Number (Js_op.Int {i}) ->
             OUnit.assert_equal ~printer:Int32.to_string 4l i
           | _ -> OUnit.assert_failure "expected a folded JavaScript integer" );
       ]
