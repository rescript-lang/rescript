let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let lambda_for_template_literal () =
  let prev_dont_write_files = !Clflags.dont_write_files in
  let prev_assume_no_mli = !Clflags.assume_no_mli in
  Clflags.dont_write_files := true;
  Clflags.assume_no_mli := Clflags.Mli_non_exists;
  Res_compmisc.init_path ();
  let module_name = "TemplateLiteralTest" in
  let env = Res_compmisc.initial_env ~modulename:module_name () in
  Env.set_unit_name module_name;
  let template_expr =
    Ast_helper.Exp.template
      {
        tag = None;
        prefix = None;
        strings = ["Hello "; ""];
        expressions =
          [Ast_helper.Exp.constant (Parsetree.Pconst_string ("world", None))];
      }
  in
  let binding =
    Ast_helper.Vb.mk
      (Ast_helper.Pat.var (Location.mknoloc "greeting"))
      template_expr
  in
  let structure = [Ast_helper.Str.value Asttypes.Nonrecursive [binding]] in
  let typed, coercion, _, _ =
    Typemod.type_implementation_more "TemplateLiteralTest.res" module_name
      module_name env structure
  in
  let lambda, _exports =
    Translmod.transl_implementation module_name (typed, coercion)
  in
  Clflags.dont_write_files := prev_dont_write_files;
  Clflags.assume_no_mli := prev_assume_no_mli;
  lambda

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           let lambda = lambda_for_template_literal () in
           let printed = Format.asprintf "%a" Printlambda.lambda lambda in
           OUnit.assert_bool "expected string.template in lambda"
             (Ext_string.contain_substring printed "string.template") );
       ]
