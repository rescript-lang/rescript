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
       ]
