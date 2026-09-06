let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let without_locations structure =
  let mapper =
    {Ast_mapper.default_mapper with location = (fun _ _ -> Location.none)}
  in
  mapper.structure mapper structure

let assert_recovery ~source ~expected =
  let parse source =
    Res_driver.parse_implementation_from_source ~display_filename:"Recovery.res"
      ~source
  in
  let recovered = parse source in
  let valid = parse expected in
  OUnit.assert_bool source recovered.invalid;
  OUnit.assert_bool expected (not valid.invalid);
  OUnit.assert_equal ~msg:source 1 (List.length recovered.diagnostics);
  OUnit.assert_equal ~msg:source
    ~printer:(Format.asprintf "%a" Printast.implementation)
    (without_locations valid.parsetree)
    (without_locations recovered.parsetree)

let suites =
  __FILE__
  >::: [
         ( "unexpected groups retain list elements and their order" >:: fun _ ->
           List.iter
             (fun (opening, closing) ->
               List.iter
                 (fun (before, element, after) ->
                   assert_recovery
                     ~source:(before ^ opening ^ element ^ closing ^ after)
                     ~expected:(before ^ element ^ after))
                 [
                   ( "type t = {",
                     "first: int, second: string",
                     ", last: bool}\nlet after = 1" );
                   ("let {", "first, second", ", last} = value\nlet after = 1");
                   ("type t<", "'a, 'b", ", 'c> = ('a, 'b, 'c)\nlet after = 1");
                 ])
             [("(", ")"); ("[", "]"); ("{", "}"); ("<", ">")] );
         ( "a mismatched closer remains available to its owner" >:: fun _ ->
           List.iter
             (fun opening ->
               assert_recovery
                 ~source:
                   ("type t = {" ^ opening
                  ^ "first: int, second: string}\nlet after = 1")
                 ~expected:
                   "type t = {first: int, second: string}\nlet after = 1")
             ["("; "["; "<"; "(["; "[<"] );
         ( "an unowned closer does not end the list" >:: fun _ ->
           List.iter
             (fun closer ->
               assert_recovery
                 ~source:
                   ("type t = {" ^ closer
                  ^ "first: int, second: string}\nlet after = 1")
                 ~expected:
                   "type t = {first: int, second: string}\nlet after = 1")
             [")"; "]"; ">"] );
         ( "invalid type argument delimiters report the opening" >:: fun _ ->
           List.iter
             (fun (source, expected, hint) ->
               assert_recovery
                 ~source:(source ^ "\nlet after = 1")
                 ~expected:(expected ^ "\nlet after = 1");
               let result =
                 Res_driver.parse_implementation_from_source
                   ~display_filename:"Recovery.res" ~source
               in
               match result.diagnostics with
               | [diagnostic] ->
                 OUnit.assert_equal (String.index source '(')
                   (Res_diagnostics.get_start_pos diagnostic).pos_cnum;
                 OUnit.assert_equal ~printer:Fun.id
                   ("Type parameters require angle brackets:\n  " ^ hint)
                   (Res_diagnostics.explain diagnostic)
               | _ -> OUnit.assert_failure "expected one delimiter diagnostic")
             [
               ("type t = option(int)", "type t = option<int>", "option<int>");
               ( "type t<'a> = Nullable.t('a)",
                 "type t<'a> = Nullable.t<'a>",
                 "Nullable.t<'a>" );
               ( "type t<'a> = private Belt.Map.t('a)",
                 "type t<'a> = private Belt.Map.t<'a>",
                 "Belt.Map.t<'a>" );
               ( "type t = option(<node<int>>)",
                 "type t = option<node<int>>",
                 "option<node<int>>" );
               ( "type t = pair<option(<node<int>>), string>",
                 "type t = pair<option<node<int>>, string>",
                 "option<node<int>>" );
               ( "type t = pair(option(<node<int>>), string)",
                 "type t = pair<option<node<int>>, string>",
                 "pair<option<node<int>>, string>" );
               ( "type t = pair(int, string)",
                 "type t = pair<int, string>",
                 "pair<int, string>" );
               ("type t = option(<int)", "type t = option<int>", "option<int>");
             ] );
         ( "type argument hints survive speculative parsing" >:: fun _ ->
           let p =
             Res_parser.make "type t<'a> = Nullable.t('a)" "Recovery.res"
           in
           let inspect p =
             ignore (Res_core.parse_implementation p);
             List.map Res_diagnostics.explain p.diagnostics
           in
           let expected =
             ["Type parameters require angle brackets:\n  Nullable.t<'a>"]
           in
           OUnit.assert_equal expected (Res_parser.lookahead p inspect);
           OUnit.assert_equal [] p.diagnostics;
           OUnit.assert_equal expected (inspect p) );
         ( "recovery ends before the next declaration" >:: fun _ ->
           assert_recovery ~source:"type t = option<<int>\nlet after = 1"
             ~expected:"type t = option<int>\nlet after = 1";
           let source =
             "type a = option(<int>)\ntype b = option(<string>)\nlet after = 1"
           in
           let result =
             Res_driver.parse_implementation_from_source
               ~display_filename:"Recovery.res" ~source
           in
           OUnit.assert_equal 2 (List.length result.diagnostics);
           OUnit.assert_equal 3 (List.length result.parsetree) );
       ]
