(** Tests for glitch-free semantics with the accumulate-then-propagate scheduler *)

open Reactive
open TestHelpers

type file_data = {refs: (string * string) list; decl_positions: string list}
(** Type for file data *)

type full_file_data = {
  value_refs: (string * string) list;
  exception_refs: (string * string) list;
  full_decls: string list;
}
(** Type for full file data *)

(** Track all deltas received *)
let track_deltas c =
  let received = ref [] in
  c.subscribe (fun wave ->
      let rev_entries = ref [] in
      ReactiveWave.iter wave (fun k mv ->
          let k = Stable.unsafe_to_value k in
          let mv = Stable.unsafe_to_value mv in
          rev_entries := (k, mv) :: !rev_entries);
      received := List.rev !rev_entries :: !received);
  received

(** Count adds and removes *)
let count_delta = function
  | entries ->
    List.fold_left
      (fun (a, r) (_, mv) ->
        if Maybe.is_some mv then (a + 1, r) else (a, r + 1))
      (0, 0) entries

let sum_deltas deltas =
  List.fold_left
    (fun (ta, tr) d ->
      let a, r = count_delta d in
      (ta + a, tr + r))
    (0, 0) deltas

(** Test: Same source anti-join - no removals expected *)
let test_same_source_anti_join () =
  reset ();
  Printf.printf "=== Test: same source anti-join ===\n";

  let src, emit = Source.create ~name:"source" () in

  let refs =
    FlatMap.create ~name:"refs" src
      ~f:(fun _file (data : file_data) emit ->
        List.iter (fun (k, v) -> emit k v) data.refs)
      ()
  in

  let decls =
    FlatMap.create ~name:"decls" src
      ~f:(fun _file (data : file_data) emit ->
        List.iter (fun pos -> emit pos ()) data.decl_positions)
      ()
  in

  let external_refs =
    Join.create ~name:"external_refs" refs decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_mb emit ->
        if not (Maybe.is_some decl_mb) then emit posTo ())
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  emit_sets emit
    [
      ("file1", {refs = [("A", "X"); ("B", "Y")]; decl_positions = ["A"; "B"]});
      ("file2", {refs = [("C", "Z")]; decl_positions = []});
    ];

  let adds, removes = sum_deltas !deltas in
  Printf.printf "adds=%d, removes=%d, len=%d\n" adds removes
    (length external_refs);

  assert (removes = 0);
  assert (length external_refs = 1);
  Printf.printf "PASSED\n\n"

(** Test: Multi-level union - the problematic case for glitch-free *)
let test_multi_level_union () =
  reset ();
  Printf.printf "=== Test: multi-level union ===\n";

  let src, emit = Source.create ~name:"source" () in

  (* refs1: level 1 *)
  let refs1 =
    FlatMap.create ~name:"refs1" src
      ~f:(fun _file (data : file_data) emit ->
        List.iter
          (fun (k, v) -> if String.length k > 0 && k.[0] = 'D' then emit k v)
          data.refs)
      ()
  in

  (* intermediate: level 1 *)
  let intermediate =
    FlatMap.create ~name:"intermediate" src
      ~f:(fun _file (data : file_data) emit ->
        List.iter
          (fun (k, v) -> if String.length k > 0 && k.[0] = 'I' then emit k v)
          data.refs)
      ()
  in

  (* refs2: level 2 *)
  let refs2 =
    FlatMap.create ~name:"refs2" intermediate ~f:(fun k v emit -> emit k v) ()
  in

  (* decls: level 1 *)
  let decls =
    FlatMap.create ~name:"decls" src
      ~f:(fun _file (data : file_data) emit ->
        List.iter (fun pos -> emit pos ()) data.decl_positions)
      ()
  in

  (* all_refs: union at level 3 *)
  let all_refs = Union.create ~name:"all_refs" refs1 refs2 () in

  (* external_refs: join at level 4 *)
  let external_refs =
    Join.create ~name:"external_refs" all_refs decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_mb emit ->
        if not (Maybe.is_some decl_mb) then emit posTo ())
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  emit_set emit "file1"
    {refs = [("D1", "X"); ("I1", "Y")]; decl_positions = ["D1"]};

  let adds, removes = sum_deltas !deltas in
  Printf.printf "adds=%d, removes=%d, len=%d\n" adds removes
    (length external_refs);

  assert (removes = 0);
  assert (length external_refs = 1);
  Printf.printf "PASSED\n\n"

(** Test: Real pipeline simulation - mimics ReactiveLiveness *)
let test_real_pipeline_simulation () =
  reset ();
  Printf.printf "=== Test: real pipeline simulation ===\n";

  let src, emit = Source.create ~name:"source" () in

  (* decls: level 1 *)
  let decls =
    FlatMap.create ~name:"decls" src
      ~f:(fun _file (data : full_file_data) emit ->
        List.iter (fun pos -> emit pos ()) data.full_decls)
      ()
  in

  (* merged_value_refs: level 1 *)
  let merged_value_refs =
    FlatMap.create ~name:"merged_value_refs" src
      ~f:(fun _file (data : full_file_data) emit ->
        List.iter (fun (k, v) -> emit k v) data.value_refs)
      ()
  in

  (* exception_refs_raw: level 1 *)
  let exception_refs_raw =
    FlatMap.create ~name:"exception_refs_raw" src
      ~f:(fun _file (data : full_file_data) emit ->
        List.iter (fun (k, v) -> emit k v) data.exception_refs)
      ()
  in

  (* exception_decls: level 2 *)
  let exception_decls =
    FlatMap.create ~name:"exception_decls" decls
      ~f:(fun pos () emit ->
        if String.length pos > 0 && pos.[0] = 'E' then emit pos ())
      ()
  in

  (* resolved_exception_refs: join at level 3 *)
  let resolved_exception_refs =
    Join.create ~name:"resolved_exception_refs" exception_refs_raw
      exception_decls
      ~key_of:(fun path _loc -> path)
      ~f:(fun path loc decl_mb emit ->
        if Maybe.is_some decl_mb then emit path loc)
      ()
  in

  (* resolved_refs_from: level 4 *)
  let resolved_refs_from =
    FlatMap.create ~name:"resolved_refs_from" resolved_exception_refs
      ~f:(fun posTo posFrom emit -> emit posFrom posTo)
      ()
  in

  (* value_refs_from: union at level 5 *)
  let value_refs_from =
    Union.create ~name:"value_refs_from" merged_value_refs resolved_refs_from ()
  in

  (* external_value_refs: join at level 6 *)
  let external_value_refs =
    Join.create ~name:"external_value_refs" value_refs_from decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_mb emit ->
        if not (Maybe.is_some decl_mb) then emit posTo ())
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_value_refs in

  emit_set emit "file1"
    {
      value_refs = [("A", "X")];
      exception_refs = [("E1", "Y")];
      full_decls = ["A"; "E1"];
    };

  let _adds, removes = sum_deltas !deltas in
  Printf.printf "removes=%d, len=%d\n" removes (length external_value_refs);

  assert (removes = 0);
  Printf.printf "PASSED\n\n"

(** Test: Separate sources - removals are expected here *)
let test_separate_sources () =
  reset ();
  Printf.printf "=== Test: separate sources (removals expected) ===\n";

  let refs_src, emit_refs = Source.create ~name:"refs_source" () in
  let decls_src, emit_decls = Source.create ~name:"decls_source" () in

  let external_refs =
    Join.create ~name:"external_refs" refs_src decls_src
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_mb emit ->
        if not (Maybe.is_some decl_mb) then emit posTo ())
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  (* Refs arrive first *)
  emit_sets emit_refs [("A", "X"); ("B", "Y"); ("C", "Z")];

  let adds1, _ = sum_deltas !deltas in
  Printf.printf "After refs: adds=%d, len=%d\n" adds1 (length external_refs);

  (* Decls arrive second - causes removals *)
  emit_sets emit_decls [("A", ()); ("B", ())];

  let adds2, removes2 = sum_deltas !deltas in
  Printf.printf "After decls: adds=%d, removes=%d, len=%d\n" adds2 removes2
    (length external_refs);

  (* With separate sources, removals are expected and correct *)
  assert (removes2 = 2);
  (* X and Y removed *)
  assert (length external_refs = 1);
  (* Only Z remains *)
  Printf.printf "PASSED\n\n"

let run_all () =
  Printf.printf "\n====== Glitch-Free Tests ======\n\n";
  test_same_source_anti_join ();
  test_multi_level_union ();
  test_real_pipeline_simulation ();
  test_separate_sources ()
