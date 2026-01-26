(** Main test driver for Reactive tests *)

let () =
  Printf.printf "\n====== Reactive Collection Tests ======\n";
  FlatMapTest.run_all ();
  JoinTest.run_all ();
  UnionTest.run_all ();
  FixpointBasicTest.run_all ();
  FixpointIncrementalTest.run_all ();
  BatchTest.run_all ();
  IntegrationTest.run_all ();
  GlitchFreeTest.run_all ();
  Printf.printf "\nAll tests passed!\n"
