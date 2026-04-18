let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let pure_iterable = Js_exp_make.var (Ident.create "iterable")
let empty_body = []

let for_of_statement =
  {
    J.statement_desc =
      ForOf (None, Ident.create "_for_of", pure_iterable, empty_body);
    comment = None;
  }

let for_await_of_statement =
  {
    J.statement_desc =
      ForAwaitOf (None, Ident.create "_for_await_of", pure_iterable, empty_body);
    comment = None;
  }

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (not (Js_analyzer.no_side_effect_statement for_of_statement)) );
         ( __LOC__ >:: fun _ ->
           OUnit.assert_bool __LOC__
             (not (Js_analyzer.no_side_effect_statement for_await_of_statement))
         );
       ]
