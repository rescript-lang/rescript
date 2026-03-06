(** Join combinator tests *)

open Reactive
open TestHelpers

let test_join () =
  reset ();
  Printf.printf "=== Test: join (reactive lookup/join) ===\n";

  (* Left collection: exception refs (path -> loc_from) *)
  let left, emit_left = Source.create ~name:"left" () in

  (* Right collection: decl index (path -> decl_pos) *)
  let right, emit_right = Source.create ~name:"right" () in

  (* Join: for each (path, loc_from) in left, look up path in right *)
  let joined =
    Join.create ~name:"joined" left right
      ~key_of:(fun path _loc_from -> path)
      ~f:(fun _path loc_from decl_pos_mb emit ->
        if ReactiveMaybe.is_some decl_pos_mb then
          emit (ReactiveMaybe.unsafe_get decl_pos_mb) loc_from)
      ()
  in

  (* Initially empty *)
  assert (length joined = 0);

  (* Add declaration at path "A" with pos 100 *)
  emit_set emit_right "A" 100;
  Printf.printf "After right set (A, 100): joined=%d\n" (length joined);
  assert (length joined = 0);

  (* No left entries yet *)

  (* Add exception ref at path "A" from loc 1 *)
  emit_set emit_left "A" 1;
  Printf.printf "After left set (A, 1): joined=%d\n" (length joined);
  assert (length joined = 1);
  assert (get_opt joined 100 = Some 1);

  (* decl_pos 100 -> loc_from 1 *)

  (* Add another exception ref at path "B" (no matching decl) *)
  emit_set emit_left "B" 2;
  Printf.printf "After left set (B, 2): joined=%d (B has no decl)\n"
    (length joined);
  assert (length joined = 1);

  (* Add declaration for path "B" *)
  emit_set emit_right "B" 200;
  Printf.printf "After right set (B, 200): joined=%d\n" (length joined);
  assert (length joined = 2);
  assert (get_opt joined 200 = Some 2);

  (* Update right: change B's decl_pos *)
  emit_set emit_right "B" 201;
  Printf.printf "After right set (B, 201): joined=%d\n" (length joined);
  assert (length joined = 2);
  assert (get_opt joined 200 = None);
  (* Old key gone *)
  assert (get_opt joined 201 = Some 2);

  (* New key has the value *)

  (* remove left entry A *)
  emit_remove emit_left "A";
  Printf.printf "After left Remove(A): joined=%d\n" (length joined);
  assert (length joined = 1);
  assert (get_opt joined 100 = None);

  Printf.printf "PASSED\n\n"

let test_join_with_merge () =
  reset ();
  Printf.printf "=== Test: join with merge ===\n";

  (* Multiple left entries can map to same right key *)
  let left, emit_left = Source.create ~name:"left" () in
  let right, emit_right = Source.create ~name:"right" () in

  (* Join with merge: all entries produce to key 0 *)
  let joined =
    Join.create ~name:"joined" left right
      ~key_of:(fun _id path -> path) (* Look up by path *)
      ~f:(fun _id _path value_mb emit ->
        if ReactiveMaybe.is_some value_mb then
          emit 0 (ReactiveMaybe.unsafe_get value_mb))
      ~merge:( + ) (* Sum values *)
      ()
  in

  emit_set emit_right "X" 10;
  emit_set emit_left 1 "X";
  emit_set emit_left 2 "X";

  Printf.printf "Two entries looking up X (value 10): sum=%d\n"
    (get_opt joined 0 |> Option.value ~default:0);
  assert (get_opt joined 0 = Some 20);

  (* 10 + 10 *)
  emit_set emit_right "X" 5;
  Printf.printf "After right changes to 5: sum=%d\n"
    (get_opt joined 0 |> Option.value ~default:0);
  assert (get_opt joined 0 = Some 10);

  (* 5 + 5 *)
  emit_remove emit_left 1;
  Printf.printf "After removing one left entry: sum=%d\n"
    (get_opt joined 0 |> Option.value ~default:0);
  assert (get_opt joined 0 = Some 5);

  (* Only one left *)
  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Join Tests ======\n\n";
  test_join ();
  test_join_with_merge ()
