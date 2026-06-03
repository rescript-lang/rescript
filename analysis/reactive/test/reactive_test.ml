(** Main test driver for Reactive tests *)

let () =
  Printf.printf "\n====== Reactive Collection Tests ======\n";
  Flat_map_test.run_all ();
  Join_test.run_all ();
  Union_test.run_all ();
  Fixpoint_basic_test.run_all ();
  Fixpoint_incremental_test.run_all ();
  Batch_test.run_all ();
  Integration_test.run_all ();
  Glitch_free_test.run_all ();
  Printf.printf "\nAll tests passed!\n"
