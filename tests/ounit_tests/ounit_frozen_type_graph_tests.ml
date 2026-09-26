let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_bool = OUnit.assert_bool

let freeze roots =
  match Frozen_type_graph.freeze roots with
  | Ok image -> image
  | Error reason -> OUnit.assert_failure reason

let test_root_sharing_and_instantiation _ =
  let variable = Btype.newgenty (Types.Tvar (Some "a")) in
  let scheme =
    Btype.newgenty
      (Types.Tarrow ([Types.{lbl = Asttypes.Nolabel; typ = variable}], variable))
  in
  let image = freeze [scheme; scheme] in
  OUnit.assert_equal 2 (Frozen_type_graph.node_count image);
  match Frozen_type_graph.thaw image with
  | [first; second] -> (
    assert_bool "two roots keep the same scheme identity" (first == second);
    let first_use = Ctype.instance Env.empty first in
    let second_use = Ctype.instance Env.empty first in
    assert_bool "each use has its own mutable inference graph"
      (first_use != second_use);
    match (first_use.desc, second_use.desc, first.desc) with
    | ( Types.Tarrow ([{typ = first_arg}], first_result),
        Types.Tarrow ([{typ = second_arg}], second_result),
        Types.Tarrow ([{typ = scheme_arg}], scheme_result) ) ->
      assert_bool "an instance preserves type-variable identity"
        (first_arg == first_result && second_arg == second_result);
      assert_bool "separate uses do not share inference variables"
        (first_arg != second_arg);
      assert_bool "instantiation did not change the thawed scheme"
        (scheme_arg == scheme_result && scheme_arg != first_arg)
    | _ -> OUnit.assert_failure "expected three function types")
  | _ -> OUnit.assert_failure "expected two roots"

let field_cell ty =
  match ty.Types.desc with
  | Types.Tfield {mutability; _} -> mutability
  | _ -> OUnit.assert_failure "expected an object field"

let test_mutability_classes_are_local _ =
  let cell = ref (Types.Mutability_value Asttypes.Immutable) in
  let rest = Btype.newgenty (Types.Tvar None) in
  let field name =
    Btype.newgenty
      (Types.Tfield {name; mutability = cell; typ = Predef.type_int (); rest})
  in
  let image = freeze [field "x"; field "y"] in
  let first = Frozen_type_graph.thaw image in
  let second = Frozen_type_graph.thaw image in
  match (first, second) with
  | [a; b], [c; d] ->
    let a_cell = field_cell a in
    assert_bool "a thaw preserves aliasing inside the image"
      (a_cell == field_cell b);
    assert_bool "separate thaws own separate mutability classes"
      (a_cell != field_cell c && field_cell c == field_cell d);
    a_cell := Types.Mutability_value Asttypes.Mutable;
    assert_bool "a thaw cannot mutate the shared image or another thaw"
      (Btype.mutability_repr (field_cell c) = Asttypes.Immutable
      && Btype.mutability_repr cell = Asttypes.Immutable)
  | _ -> OUnit.assert_failure "expected two fields per thaw"

let test_poly_quantifiers_instantiate_independently _ =
  let universal = Btype.newgenty (Types.Tunivar (Some "a")) in
  let body =
    Btype.newgenty
      (Types.Tarrow
         ([Types.{lbl = Asttypes.Nolabel; typ = universal}], universal))
  in
  let image = freeze [Btype.newgenty (Types.Tpoly (body, [universal]))] in
  let scheme = Frozen_type_graph.thaw_root image 0 in
  match scheme.Types.desc with
  | Types.Tpoly (body, [universal]) -> (
    let first_variables, first =
      Ctype.instance_poly ~fixed:false [universal] body
    in
    let second_variables, second =
      Ctype.instance_poly ~fixed:false [universal] body
    in
    let first_variable, second_variable =
      match (first_variables, second_variables) with
      | [first_variable], [second_variable] -> (first_variable, second_variable)
      | _ -> OUnit.assert_failure "expected one instance variable per use"
    in
    assert_bool "quantified variables are fresh for each use"
      (first_variable != second_variable);
    match (first.desc, second.desc, body.desc) with
    | ( Types.Tarrow ([{typ = first_arg}], first_result),
        Types.Tarrow ([{typ = second_arg}], second_result),
        Types.Tarrow ([{typ = original_arg}], original_result) ) ->
      assert_bool "each body uses its own quantified variable"
        (first_arg == first_result
        && first_arg == first_variable
        && second_arg == second_result
        && second_arg == second_variable);
      assert_bool "the frozen scheme's thaw stays quantified"
        (original_arg == universal && original_result == universal)
    | _ -> OUnit.assert_failure "expected two instantiated functions")
  | _ -> OUnit.assert_failure "expected a quantified function"

let test_cycles_and_identifier_sharing _ =
  let recursive = Btype.newgenty (Types.Tvar None) in
  recursive.desc <- Types.Ttuple [recursive];
  let ident = Ident.create "local" in
  let first =
    Btype.newgenty (Types.Tconstr (Path.Pident ident, [], ref Types.Mnil))
  in
  let second =
    Btype.newgenty (Types.Tconstr (Path.Pident ident, [], ref Types.Mnil))
  in
  let image = freeze [recursive; first; second] in
  let thaw () = Frozen_type_graph.thaw image in
  let left = Domain.spawn thaw in
  let right = Domain.spawn thaw in
  match (Domain.join left, Domain.join right) with
  | [cycle_a; a; b], [cycle_b; c; d] ->
    (match (cycle_a.desc, cycle_b.desc) with
    | Types.Ttuple [back_a], Types.Ttuple [back_b] ->
      assert_bool "cycles preserve their own identity"
        (back_a == cycle_a && back_b == cycle_b && cycle_a != cycle_b)
    | _ -> OUnit.assert_failure "expected recursive tuples");
    let path_ident ty =
      match ty.Types.desc with
      | Types.Tconstr (Path.Pident id, _, _) -> id
      | _ -> OUnit.assert_failure "expected a constructor path"
    in
    assert_bool "identifiers share within a thaw"
      (path_ident a == path_ident b && path_ident c == path_ident d);
    assert_bool "identifiers belong to one thaw" (path_ident a != path_ident c)
  | _ -> OUnit.assert_failure "expected three roots per thaw"

let test_variant_row_references_are_local _ =
  let row_reference = ref None in
  let field = Types.Reither (true, [], false, row_reference) in
  let source =
    Btype.newgenty
      (Types.Tvariant
         {
           row_fields = [("A", field); ("B", field)];
           row_more = Btype.newgenty Types.Tnil;
           row_closed = true;
           row_fixed = false;
           row_name = None;
         })
  in
  let image = freeze [source] in
  let row_refs ty =
    match ty.Types.desc with
    | Types.Tvariant
        {
          row_fields =
            [
              ("A", Types.Reither (_, _, _, a));
              ("B", Types.Reither (_, _, _, b));
            ];
          _;
        } ->
      (a, b)
    | _ -> OUnit.assert_failure "expected two variant rows"
  in
  match (Frozen_type_graph.thaw image, Frozen_type_graph.thaw image) with
  | [first], [second] ->
    let a, b = row_refs first in
    let c, d = row_refs second in
    assert_bool "row references share within an image" (a == b && c == d);
    assert_bool "row references are request-local" (a != c);
    a := Some Types.Rabsent;
    assert_bool "a row update does not change the image or another thaw"
      (!c = None && !row_reference = None)
  | _ -> OUnit.assert_failure "expected one variant per thaw"

let test_rejects_transient_state _ =
  let variable = Btype.newgenty (Types.Tvar None) in
  let marked = Btype.newgenty (Types.Tsubst variable) in
  let memo = ref (Types.Mlink (ref Types.Mnil)) in
  let abbreviated =
    Btype.newgenty
      (Types.Tconstr (Path.Pident (Ident.create_persistent "Alias"), [], memo))
  in
  assert_bool "copy marks cannot enter the immutable image"
    (Result.is_error (Frozen_type_graph.freeze [marked]));
  assert_bool "abbreviation memo state cannot enter the immutable image"
    (Result.is_error (Frozen_type_graph.freeze [abbreviated]))

let test_selective_thaw _ =
  let unused = Btype.newgenty (Types.Ttuple [Predef.type_int ()]) in
  let chosen = Btype.newgenty (Types.Tvar (Some "chosen")) in
  let image = freeze [unused; chosen] in
  OUnit.assert_equal 2 (Frozen_type_graph.root_count image);
  let first = Frozen_type_graph.thaw_root image 1 in
  let second = Frozen_type_graph.thaw_root image 1 in
  assert_bool "one root can be materialized without sharing mutable nodes"
    (first != second && first != chosen);
  first.desc <- Types.Tvar (Some "changed");
  match (second.desc, chosen.desc) with
  | Types.Tvar (Some "chosen"), Types.Tvar (Some "chosen") -> ()
  | _ -> OUnit.assert_failure "selective thaw changed another graph"

let test_registered_identifier_matches_path _ =
  let binder = Ident.create "t" in
  let root =
    Btype.newgenty (Types.Tconstr (Path.Pident binder, [], ref Types.Mnil))
  in
  let image = Frozen_type_graph.freeze ~identifiers:[binder] [root] in
  let image =
    match image with
    | Ok image -> image
    | Error reason -> OUnit.assert_failure reason
  in
  let view = Frozen_type_graph.create_view image in
  let materialized_binder = Frozen_type_graph.identifier_at view 0 in
  match (Frozen_type_graph.type_at view 0).Types.desc with
  | Types.Tconstr (Path.Pident path_ident, _, _) ->
    assert_bool "signature binder and type path use the same local object"
      (materialized_binder == path_ident && materialized_binder != binder)
  | _ -> OUnit.assert_failure "expected a constructor path"

let suites =
  __FILE__
  >::: [
         "root_sharing_and_instantiation" >:: test_root_sharing_and_instantiation;
         "mutability_classes_are_local" >:: test_mutability_classes_are_local;
         "poly_quantifiers_instantiate_independently"
         >:: test_poly_quantifiers_instantiate_independently;
         "cycles_and_identifier_sharing" >:: test_cycles_and_identifier_sharing;
         "variant_row_references_are_local"
         >:: test_variant_row_references_are_local;
         "rejects_transient_state" >:: test_rejects_transient_state;
         "selective_thaw" >:: test_selective_thaw;
         "registered_identifier_matches_path"
         >:: test_registered_identifier_matches_path;
       ]
