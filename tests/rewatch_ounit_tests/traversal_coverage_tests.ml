open OUnit2

let admission_sequence _context =
  let visited = Hashtbl.create 1 in
  assert_equal Traversal_coverage.Visit_current
    (Traversal_coverage.admit visited "directory" ~recursive:false);
  assert_equal Traversal_coverage.Skip
    (Traversal_coverage.admit visited "directory" ~recursive:false);
  assert_equal Traversal_coverage.Visit_descendants
    (Traversal_coverage.admit visited "directory" ~recursive:true);
  assert_equal Traversal_coverage.Skip
    (Traversal_coverage.admit visited "directory" ~recursive:true)

let recursive_first_visit _context =
  let visited = Hashtbl.create 1 in
  let admission =
    Traversal_coverage.admit visited "directory" ~recursive:true
  in
  assert_equal Traversal_coverage.Visit_current_and_descendants admission;
  assert_bool "the current directory is visited"
    (Traversal_coverage.visits_current admission);
  assert_bool "descendants are visited"
    (Traversal_coverage.visits_descendants admission)

let tests =
  "traversal_coverage_tests"
  >::: [
         "shallow coverage can be upgraded" >:: admission_sequence;
         "a recursive first visit covers the complete tree"
         >:: recursive_first_visit;
       ]
