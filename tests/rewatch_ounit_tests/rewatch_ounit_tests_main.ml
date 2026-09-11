open OUnit2

let () =
  run_test_tt_main
    ("rewatch"
    >::: [
           Build_artifacts_tests.tests;
           Build_state_tests.tests;
           Clean_tests.tests;
           Cli_tests.tests;
           Compile_assets_tests.tests;
           Compiler_args_tests.tests;
           Compiler_info_tests.tests;
           Compiler_scheduler_tests.tests;
           Config_tests.tests;
           File_util_tests.tests;
           Format_tests.tests;
           Native_watcher_tests.tests;
           Output_tests.tests;
           Package_metadata_tests.tests;
           Project_context_tests.tests;
           Source_tests.tests;
           Toolchain_tests.tests;
           Unit_tests.tests;
           Warning_state_tests.tests;
           Watcher_tests.tests;
         ])
