let suites =
  OUnit.( >::: ) __FILE__
    [
      Ounit_vec_test.suites;
      Ounit_json_tests.suites;
      Ounit_ext_json_tests.suites;
      Ounit_path_tests.suites;
      Ounit_array_tests.suites;
      Ounit_scc_tests.suites;
      Ounit_list_test.suites;
      Ounit_hash_set_tests.suites;
      Ounit_bal_tree_tests.suites;
      Ounit_hash_stubs_test.suites;
      Ounit_map_tests.suites;
      Ounit_hashtbl_tests.suites;
      Ounit_string_tests.suites;
      Ounit_int_vec_tests.suites;
      Ounit_ident_mask_tests.suites;
      Ounit_utf8_test.suites;
      Ounit_unicode_tests.suites;
      Ounit_bsb_regex_tests.suites;
      Ounit_bsb_pkg_tests.suites;
      Ounit_util_tests.suites;
    ]

let _ = OUnit.run_test_tt_main suites
