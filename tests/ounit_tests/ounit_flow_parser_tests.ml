let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let assert_classification expected source =
  OUnit.assert_equal expected (Classify_function.classify source)

let parse_expression source =
  let open Parser_flow in
  let env = Parser_env.init_env None source in
  do_parse env Parse.expression false

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           assert_classification
             (Js_raw_info.Js_function {arity = 2; arrow = false})
             "function (x, y) { return x + y; }" );
         ( __LOC__ >:: fun _ ->
           assert_classification
             (Js_raw_info.Js_function {arity = 2; arrow = true})
             "(x, y) => x + y" );
         ( __LOC__ >:: fun _ ->
           assert_classification
             (Js_raw_info.Js_literal {comment = None})
             "{x: [1, -2, null, undefined]}" );
         ( __LOC__ >:: fun _ ->
           assert_classification
             (Js_raw_info.Js_literal {comment = Some "/* keep */"})
             "/* keep */ 1" );
         ( __LOC__ >:: fun _ ->
           assert_classification Js_raw_info.Js_exp_unknown "value + 1";
           assert_classification Js_raw_info.Js_exp_unknown "{...value}" );
         ( __LOC__ >:: fun _ ->
           let (_, expression), errors = parse_expression "/rescript/gi" in
           OUnit.assert_equal [] errors;
           match expression with
           | Flow_ast.Expression.RegExpLiteral _ -> ()
           | _ -> OUnit.assert_failure "expected a regular expression literal"
         );
         ( __LOC__ >:: fun _ ->
           let _, errors = parse_expression "1 +" in
           match errors with
           | ({Loc.start; _end}, _) :: _ ->
             OUnit.assert_equal 1 start.line;
             OUnit.assert_equal 1 _end.line;
             OUnit.assert_bool __LOC__ (_end.column >= start.column)
           | [] -> OUnit.assert_failure "expected a parser error" );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal Js_raw_info.Js_stmt_comment
             (Classify_function.classify_stmt "// only a comment");
           OUnit.assert_equal Js_raw_info.Js_stmt_unknown
             (Classify_function.classify_stmt "console.log('hello')") );
       ]
