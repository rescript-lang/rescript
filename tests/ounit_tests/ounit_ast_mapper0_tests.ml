let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_failure = OUnit.assert_failure

let loc = Location.none

let attr name payload = ({Location.txt = name; loc}, payload)

let has_attr name attrs =
  List.exists (fun ({Location.txt}, _) -> txt = name) attrs

let record_pat0 attrs =
  Ast_helper0.Pat.record ~loc ~attrs
    [
      ( Location.mknoloc (Longident.Lident "name"),
        Ast_helper0.Pat.var ~loc (Location.mknoloc "name") );
    ]
    Asttypes.Open

let map_pat0 pat =
  Ast_mapper_from0.default_mapper.pat Ast_mapper_from0.default_mapper pat

let test_public_record_rest_attr_is_not_internal _ =
  let pat =
    map_pat0 (record_pat0 [attr "res.record_rest" (Parsetree0.PStr [])])
  in
  match pat.ppat_desc with
  | Parsetree.Ppat_record (_, _, None) ->
    OUnit.assert_bool "public res.record_rest attribute was not preserved"
      (has_attr "res.record_rest" pat.ppat_attributes)
  | Parsetree.Ppat_record (_, _, Some _) ->
    assert_failure "public res.record_rest attribute was decoded as record rest"
  | _ -> assert_failure "Expected a record pattern"

let test_malformed_internal_record_rest_attr_fails _ =
  OUnit.assert_raises (Failure "Malformed internal _res.record_rest attribute")
    (fun () ->
      ignore
        (map_pat0 (record_pat0 [attr "_res.record_rest" (Parsetree0.PStr [])])))

let test_record_rest_roundtrips_through_ast0 _ =
  let rest =
    Some
      {
        Parsetree.rest_loc = loc;
        rest_name = Location.mknoloc "rest";
        rest_type = None;
      }
  in
  let pat =
    Ast_helper.Pat.record ~loc ?rest
      [
        {
          Parsetree.lid = Location.mknoloc (Longident.Lident "name");
          x = Ast_helper.Pat.var ~loc (Location.mknoloc "name");
          opt = false;
        };
      ]
      Asttypes.Open
  in
  let pat0 =
    Ast_mapper_to0.default_mapper.pat Ast_mapper_to0.default_mapper pat
  in
  let pat = map_pat0 pat0 in
  match pat.ppat_desc with
  | Parsetree.Ppat_record
      (_, _, Some {rest_name = {txt = "rest"; _}; rest_type = None; _}) ->
    ()
  | _ -> assert_failure "Expected record rest after ast0 roundtrip"

let suites =
  __FILE__
  >::: [
         "public_record_rest_attr_is_not_internal"
         >:: test_public_record_rest_attr_is_not_internal;
         "malformed_internal_record_rest_attr_fails"
         >:: test_malformed_internal_record_rest_attr_fails;
         "record_rest_roundtrips_through_ast0"
         >:: test_record_rest_roundtrips_through_ast0;
       ]
