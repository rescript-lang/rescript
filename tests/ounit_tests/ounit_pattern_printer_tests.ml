let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let loc = Location.none

let rest_type =
  Btype.newgenty
    (Types.Tconstr (Path.Pident (Ident.create "restConfig"), [], ref Types.Mnil))

let record_type =
  Btype.newgenty
    (Types.Tconstr (Path.Pident (Ident.create "config"), [], ref Types.Mnil))

let record_rest_pattern =
  let rest = Ident.create "_rest" in
  {
    Typedtree.pat_desc =
      Tpat_record
        ( [],
          Asttypes.Closed,
          Some
            {
              rest_ident = rest;
              rest_name = Location.mknoloc (Ident.name rest);
              rest_type;
              excluded_runtime_labels = [];
            } );
    pat_loc = loc;
    pat_extra = [];
    pat_type = rest_type;
    pat_env = Env.empty;
    pat_attributes = [];
  }

let int_pattern n =
  {
    record_rest_pattern with
    Typedtree.pat_desc = Tpat_constant (Const_int n);
    pat_type = Predef.type_int;
  }

let count_label =
  let label =
    {
      Types.lbl_name = "count";
      lbl_res = record_type;
      lbl_arg = Predef.type_int;
      lbl_mut = Asttypes.Immutable;
      lbl_optional = false;
      lbl_pos = 0;
      lbl_all = [||];
      lbl_repres = Record_regular;
      lbl_private = Asttypes.Public;
      lbl_loc = loc;
      lbl_attributes = [];
    }
  in
  label.lbl_all <- [|label|];
  label

let record_with_rest_pattern =
  {
    record_rest_pattern with
    Typedtree.pat_desc =
      Tpat_record
        ( [
            ( Location.mknoloc (Longident.Lident "count"),
              count_label,
              int_pattern 1,
              false );
          ],
          Asttypes.Closed,
          match record_rest_pattern.pat_desc with
          | Tpat_record (_, _, rest) -> rest
          | _ -> assert false );
    pat_type = record_type;
  }

let dummy_expr =
  {
    Typedtree.exp_desc = Texp_constant (Const_int 0);
    exp_loc = loc;
    exp_extra = [];
    exp_type = Predef.type_int;
    exp_env = Env.empty;
    exp_attributes = [];
  }

let assert_parmatch_conv_keeps_record_rest _ =
  let converted_pattern = ref None in
  let pred _ _ pattern =
    converted_pattern := Some pattern;
    None
  in
  ignore
    (Parmatch.check_partial_gadt pred loc
       [{c_lhs = record_with_rest_pattern; c_guard = None; c_rhs = dummy_expr}]);
  match !converted_pattern with
  | Some {Parsetree.ppat_desc = Ppat_record (_, _, Some rest)} ->
    OUnit.assert_equal ~printer:(fun x -> x) "_rest" rest.rest_name.txt;
    OUnit.assert_bool __LOC__ (Option.is_some rest.rest_type)
  | Some pattern ->
    OUnit.assert_failure
      (Format.asprintf "Expected record rest, got %a" Pprintast.pattern pattern)
  | None -> OUnit.assert_failure "Expected exhaustiveness predicate to run"

let suites =
  __FILE__
  >::: [
         ( __LOC__ >:: fun _ ->
           OUnit.assert_equal
             ~printer:(fun x -> x)
             "{..._rest}"
             (Pattern_printer.print_pattern record_rest_pattern) );
         __LOC__ >:: assert_parmatch_conv_keeps_record_rest;
       ]
