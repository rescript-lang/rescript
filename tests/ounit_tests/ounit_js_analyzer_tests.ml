let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let pure_iterable = Js_exp_make.var (Ident.create "iterable")
let empty_body = []

let record_rest_expression source field =
  Js_exp_make.record_rest
    [{J.record_rest_label = "name"; record_rest_ident = Some field}]
    (Js_exp_make.var source)

let record_rest_expression_without_idents source =
  Js_exp_make.record_rest
    [{J.record_rest_label = "name"; record_rest_ident = None}]
    (Js_exp_make.var source)

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

let record_rest_statement ~source ~field ~rest =
  Js_stmt_make.define_variable ~kind:Lam_compat.Strict rest
    (record_rest_expression source field)

let function_expression param body =
  {
    J.expression_desc =
      Fun
        {
          is_method = false;
          params = [param];
          body;
          env = Js_fun_env.make 1;
          return_unit = false;
          async = false;
          directive = None;
        };
    comment = None;
  }

let transform_expression expression =
  let fn = Ident.create "fn" in
  let program =
    Js_pass_record_rest.program
      {
        J.block =
          [Js_stmt_make.define_variable ~kind:Lam_compat.Strict fn expression];
        exports = [];
        export_set = Set_ident.empty;
      }
  in
  match program.block with
  | [
   {
     statement_desc =
       Variable {value = Some ({expression_desc = Fun _; _} as expression); _};
     _;
   };
  ] ->
    expression
  | _ -> OUnit.assert_failure __LOC__

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
         ( __LOC__ >:: fun _ ->
           let source = Ident.create "source" in
           let field = Ident.create "name" in
           OUnit.assert_bool __LOC__
             (not
                (Js_analyzer.no_side_effect_expression
                   (record_rest_expression source field))) );
         ( __LOC__ >:: fun _ ->
           let source = Ident.create "source" in
           let field = Ident.create "name" in
           OUnit.assert_bool __LOC__
             (not
                (Js_analyzer.eq_expression
                   (record_rest_expression source field)
                   (record_rest_expression source field))) );
         ( __LOC__ >:: fun _ ->
           let source = Ident.create "source" in
           let field = Ident.create "name" in
           let rest = Ident.create "rest" in
           let free =
             Js_analyzer.free_variables_of_statement
               (record_rest_statement ~source ~field ~rest)
           in
           OUnit.assert_bool __LOC__ (Set_ident.mem free source);
           OUnit.assert_bool __LOC__ (not (Set_ident.mem free field));
           OUnit.assert_bool __LOC__ (not (Set_ident.mem free rest)) );
         ( __LOC__ >:: fun _ ->
           let param = Ident.create "param" in
           let transformed =
             transform_expression
               (function_expression param
                  [
                    Js_stmt_make.return_stmt
                      (record_rest_expression_without_idents param);
                  ])
           in
           match transformed.expression_desc with
           | Fun
               {
                 params = [transformed_param];
                 body =
                   [
                     {
                       statement_desc =
                         Variable
                           {
                             ident = rest;
                             value =
                               Some
                                 {
                                   expression_desc =
                                     Record_rest
                                       ( [
                                           {
                                             record_rest_label = "name";
                                             record_rest_ident = Some ignored;
                                           };
                                         ],
                                         {expression_desc = Var (Id source); _}
                                       );
                                   _;
                                 };
                             _;
                           };
                       _;
                     };
                     {
                       statement_desc =
                         Return {expression_desc = Var (Id returned); _};
                       _;
                     };
                   ];
                 _;
               } ->
             OUnit.assert_bool __LOC__ (Ident.same param transformed_param);
             OUnit.assert_equal "__unused0" (Ident.name ignored);
             OUnit.assert_equal "rest" (Ident.name rest);
             OUnit.assert_bool __LOC__ (Ident.same param source);
             OUnit.assert_bool __LOC__ (Ident.same rest returned)
           | _ -> OUnit.assert_failure __LOC__ );
         ( __LOC__ >:: fun _ ->
           let rest = Ident.create "rest" in
           let program =
             Js_pass_record_rest.program
               {
                 J.block =
                   [
                     Js_stmt_make.define_variable ~kind:Lam_compat.Strict rest
                       (Js_exp_make.record_rest
                          [
                            {
                              record_rest_label = "name";
                              record_rest_ident = None;
                            };
                          ]
                          {expression_desc = Object (None, []); comment = None});
                   ];
                 exports = [];
                 export_set = Set_ident.empty;
               }
           in
           match program.block with
           | [
            {
              statement_desc =
                Variable
                  {
                    value =
                      Some
                        {
                          expression_desc =
                            Record_rest
                              ( [
                                  {
                                    record_rest_label = "name";
                                    record_rest_ident = Some ignored;
                                  };
                                ],
                                _ );
                          _;
                        };
                    _;
                  };
              _;
            };
           ] ->
             OUnit.assert_equal "__unused0" (Ident.name ignored)
           | _ -> OUnit.assert_failure __LOC__ );
       ]
