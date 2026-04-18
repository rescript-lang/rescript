let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_equal = OUnit.assert_equal
let assert_failure = OUnit.assert_failure

let parse_structure source =
  Res_driver.parse_implementation_from_source ~for_printer:false
    ~display_filename:"JsxLocTest.res" ~source
  |> fun result -> result.parsetree

let roundtrip_structure source =
  parse_structure source
  |> Ml_binary.to_ast0 Ml_binary.Ml
  |> Ml_binary.ast0_to_structure

let get_jsx_props structure =
  let from_expr = function
    | {
        Parsetree.pexp_desc =
          Pexp_jsx_element (Jsx_unary_element {jsx_unary_element_props = props});
        _;
      } ->
      Some props
    | {
        Parsetree.pexp_desc =
          Pexp_jsx_element
            (Jsx_container_element {jsx_container_element_props = props});
        _;
      } ->
      Some props
    | _ -> None
  in
  let from_item = function
    | {Parsetree.pstr_desc = Pstr_value (_, bindings); _} ->
      bindings
      |> List.find_map (fun {Parsetree.pvb_expr; _} -> from_expr pvb_expr)
    | {pstr_desc = Pstr_eval (expr, _); _} -> from_expr expr
    | _ -> None
  in
  match List.find_map from_item structure with
  | Some props -> props
  | None ->
    assert_failure
      (Format.asprintf "Expected a JSX unary element binding in:@.%a"
         Pprintast.structure structure)

let get_roundtripped_props source =
  let original = parse_structure source |> get_jsx_props in
  let roundtripped = roundtrip_structure source |> get_jsx_props in
  (original, roundtripped)

let assert_same_loc expected actual =
  let to_tuple (loc : Location.t) =
    ( loc.loc_start.pos_lnum,
      loc.loc_start.pos_bol,
      loc.loc_start.pos_cnum,
      loc.loc_end.pos_lnum,
      loc.loc_end.pos_bol,
      loc.loc_end.pos_cnum )
  in
  assert_equal
    ~printer:(fun loc ->
      let sl, sb, sc, el, eb, ec = to_tuple loc in
      Printf.sprintf "(%d,%d,%d)-(%d,%d,%d)" sl sb sc el eb ec)
    expected actual

let test_jsx_prop_value_loc_roundtrip _ =
  let source = {|
let _ = <Comp foo=bar />
|} in
  let original, roundtripped = get_roundtripped_props source in
  match (original, roundtripped) with
  | [Parsetree.JSXPropValue (original_name, _, _)], [JSXPropValue (name, _, _)]
    ->
    assert_same_loc original_name.loc name.loc
  | _ -> assert_failure "Expected one JSX prop value"

let test_jsx_spread_loc_roundtrip _ =
  let source = {|
let _ = <Comp {...props} foo=bar />
|} in
  let original, roundtripped = get_roundtripped_props source in
  match (original, roundtripped) with
  | ( Parsetree.JSXPropSpreading (original_loc, _) :: _,
      JSXPropSpreading (loc, _) :: _ ) ->
    assert_same_loc original_loc loc
  | _ -> assert_failure "Expected a leading JSX spread prop"

let suites =
  __FILE__
  >::: [
         "prop_value_roundtrip" >:: test_jsx_prop_value_loc_roundtrip;
         "spread_roundtrip" >:: test_jsx_spread_loc_roundtrip;
       ]
