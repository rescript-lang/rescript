let suites =
  OUnit.( >::: ) __FILE__
    [
      Ounit_vec_test.suites;
      Ounit_json_tests.suites;
      Ounit_array_tests.suites;
      Ounit_scc_tests.suites;
      Ounit_list_test.suites;
      Ounit_hash_set_tests.suites;
      Ounit_bal_tree_tests.suites;
      Ounit_hash_stubs_test.suites;
      Ounit_map_tests.suites;
      Ounit_hashtbl_tests.suites;
      Ounit_string_tests.suites;
      Ounit_string_literal_tests.suites;
      Ounit_int_vec_tests.suites;
      Ounit_ident_mask_tests.suites;
      Ounit_lid_of_path_tests.suites;
      Ounit_utf8_test.suites;
      Ounit_util_tests.suites;
      Ounit_rec_check_tests.suites;
      Ounit_lambda_constant_tests.suites;
      Ounit_deep_flatten_tests.suites;
      Ounit_exits_tests.suites;
      Ounit_sroa_tests.suites;
      Ounit_ast_mapper0_tests.suites;
      Ounit_object_mutability_tests.suites;
      Ounit_pattern_printer_tests.suites;
      Ounit_js_analyzer_tests.suites;
      Ounit_flow_parser_tests.suites;
      Ounit_jsx_loc_tests.suites;
      Ounit_analysis_config_tests.suites;
      Ounit_analysis_references_tests.suites;
      Ounit_ffi_inclusion_tests.suites;
      Ounit_gentype_tests.suites;
    ]

let _ = OUnit.run_test_tt_main suites
