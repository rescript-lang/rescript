let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let assert_decoded ~encoded ~expected =
  OUnit.assert_equal ~printer:Ext_obj.dump (Some expected)
    (String_literal.decode_js_escapes encoded)

let assert_invalid_backquoted_pattern encoded =
  let template_attribute =
    (Location.mknoloc "res.template", Parsetree.PStr [])
  in
  let pattern =
    Ast_helper.Pat.constant ~attrs:[template_attribute]
      (Parsetree.Pconst_string (encoded, Some "js"))
  in
  match Ast_utf8_string_interp.transform_pat pattern encoded "js" with
  | _ -> OUnit.assert_failure "expected an invalid string escape"
  | exception Location.Error _ -> ()

let assert_invalid_tagged_pattern tag contents =
  let pattern =
    Ast_helper.Pat.constant (Parsetree.Pconst_string (contents, Some tag))
  in
  match Ast_utf8_string_interp.transform_pat pattern contents tag with
  | _ -> OUnit.assert_failure "expected a tagged pattern error"
  | exception Location.Error _ -> ()

let assert_transformed_expression ?(delim = "js") ~encoded ~expected () =
  let expression =
    Ast_helper.Exp.constant (Parsetree.Pconst_string (encoded, Some delim))
  in
  match
    (Ast_utf8_string_interp.transform_exp expression encoded delim).pexp_desc
  with
  | Pexp_constant (Pconst_string (actual, None)) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> OUnit.assert_failure "expected a semantic string expression"

let assert_transformed_pattern ?(delim = "js") ~encoded ~expected () =
  let pattern =
    Ast_helper.Pat.constant (Parsetree.Pconst_string (encoded, Some delim))
  in
  match
    (Ast_utf8_string_interp.transform_pat pattern encoded delim).ppat_desc
  with
  | Ppat_constant (Pconst_string (actual, None)) ->
    OUnit.assert_equal ~printer:(Printf.sprintf "%S") expected actual
  | _ -> OUnit.assert_failure "expected a semantic string pattern"

let suites =
  __FILE__
  >::: [
         ( "plain text" >:: fun _ ->
           assert_decoded ~encoded:"plain" ~expected:"plain" );
         ( "named escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\b\f\n\r\t\v\0|}
             ~expected:"\b\012\n\r\t\011\000" );
         ( "escaped punctuation and non-escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\\\"\'\ \$\`\a|} ~expected:{|\"' $`a|} );
         ( "hex escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\x61\xE9|} ~expected:"aé" );
         ( "unicode escapes" >:: fun _ ->
           assert_decoded ~encoded:{|\u0061\u20AC|} ~expected:"a€";
           assert_decoded ~encoded:{|\u{1f600}|} ~expected:"😀";
           assert_decoded ~encoded:{|\uD83D\uDE00|} ~expected:"😀" );
         ( "line continuations" >:: fun _ ->
           assert_decoded ~encoded:"a\\\nb" ~expected:"ab";
           assert_decoded ~encoded:"a\\\rb" ~expected:"ab";
           assert_decoded ~encoded:"a\\\r\nb" ~expected:"ab" );
         ( "ordinary literals become semantic strings" >:: fun _ ->
           assert_transformed_expression ~encoded:{|\x61\n\uD83D\uDE00|}
             ~expected:"a\n😀" ();
           assert_transformed_pattern ~encoded:{|\x61\n\uD83D\uDE00|}
             ~expected:"a\n😀" () );
         ( "template literals remain raw" >:: fun _ ->
           let encoded = {|\x61|} in
           let template_attribute =
             (Location.mknoloc "res.template", Parsetree.PStr [])
           in
           let expression =
             Ast_helper.Exp.constant ~attrs:[template_attribute]
               (Parsetree.Pconst_string (encoded, Some "js"))
           in
           match
             (Ast_utf8_string_interp.transform_exp expression encoded "js")
               .pexp_desc
           with
           | Pexp_constant (Pconst_string (actual, Some "bq")) ->
             OUnit.assert_equal ~printer:(Printf.sprintf "%S") encoded actual
           | _ -> OUnit.assert_failure "expected a raw template segment" );
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
             ] );
         ( "backquoted patterns reject lone surrogate escapes" >:: fun _ ->
           assert_invalid_backquoted_pattern {|\uD800|};
           assert_invalid_backquoted_pattern {|\uDC00|} );
         ( "patterns reject tagged template literals" >:: fun _ ->
           (* A tagged pattern cannot invoke its tag. Treating its raw contents as
              a string made json`\x61` collide with the ordinary "\\x61"
              pattern during string-switch sorting. *)
           assert_invalid_tagged_pattern "json" {|\x61|} );
         ( "printer char patterns are not tagged templates" >:: fun _ ->
           let pattern =
             Ast_helper.Pat.constant
               (Parsetree.Pconst_string ("a", Some "INTERNAL_RES_CHAR_CONTENTS"))
           in
           let transformed =
             Ast_utf8_string_interp.transform_pat pattern "a"
               "INTERNAL_RES_CHAR_CONTENTS"
           in
           OUnit.assert_equal ~printer:Ext_obj.dump pattern.ppat_desc
             transformed.ppat_desc );
       ]
