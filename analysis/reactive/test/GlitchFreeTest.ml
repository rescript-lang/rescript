(** Tests for glitch-free semantics with the accumulate-then-propagate scheduler *)

open Reactive

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
  c.subscribe (fun d -> received := d :: !received);
  received

(** Count adds and removes *)
let count_delta = function
  | Set _ -> (1, 0)
  | Remove _ -> (0, 1)
  | Batch entries ->
    List.fold_left
      (fun (a, r) (_, v_opt) ->
        match v_opt with
        | Some _ -> (a + 1, r)
        | None -> (a, r + 1))
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

  let src, emit = source ~name:"source" () in

  let refs =
    flatMap ~name:"refs" src ~f:(fun _file (data : file_data) -> data.refs) ()
  in

  let decls =
    flatMap ~name:"decls" src
      ~f:(fun _file (data : file_data) ->
        List.map (fun pos -> (pos, ())) data.decl_positions)
      ()
  in

  let external_refs =
    join ~name:"external_refs" refs decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_opt ->
        match decl_opt with
        | Some () -> []
        | None -> [(posTo, ())])
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  emit
    (Batch
       [
         set "file1"
           {refs = [("A", "X"); ("B", "Y")]; decl_positions = ["A"; "B"]};
         set "file2" {refs = [("C", "Z")]; decl_positions = []};
       ]);

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

  let src, emit = source ~name:"source" () in

  (* refs1: level 1 *)
  let refs1 =
    flatMap ~name:"refs1" src
      ~f:(fun _file (data : file_data) ->
        List.filter (fun (k, _) -> String.length k > 0 && k.[0] = 'D') data.refs)
      ()
  in

  (* intermediate: level 1 *)
  let intermediate =
    flatMap ~name:"intermediate" src
      ~f:(fun _file (data : file_data) ->
        List.filter (fun (k, _) -> String.length k > 0 && k.[0] = 'I') data.refs)
      ()
  in

  (* refs2: level 2 *)
  let refs2 = flatMap ~name:"refs2" intermediate ~f:(fun k v -> [(k, v)]) () in

  (* decls: level 1 *)
  let decls =
    flatMap ~name:"decls" src
      ~f:(fun _file (data : file_data) ->
        List.map (fun pos -> (pos, ())) data.decl_positions)
      ()
  in

  (* all_refs: union at level 3 *)
  let all_refs = union ~name:"all_refs" refs1 refs2 () in

  (* external_refs: join at level 4 *)
  let external_refs =
    join ~name:"external_refs" all_refs decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_opt ->
        match decl_opt with
        | Some () -> []
        | None -> [(posTo, ())])
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  emit
    (Batch
       [
         set "file1" {refs = [("D1", "X"); ("I1", "Y")]; decl_positions = ["D1"]};
       ]);

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

  let src, emit = source ~name:"source" () in

  (* decls: level 1 *)
  let decls =
    flatMap ~name:"decls" src
      ~f:(fun _file (data : full_file_data) ->
        List.map (fun pos -> (pos, ())) data.full_decls)
      ()
  in

  (* merged_value_refs: level 1 *)
  let merged_value_refs =
    flatMap ~name:"merged_value_refs" src
      ~f:(fun _file (data : full_file_data) -> data.value_refs)
      ()
  in

  (* exception_refs_raw: level 1 *)
  let exception_refs_raw =
    flatMap ~name:"exception_refs_raw" src
      ~f:(fun _file (data : full_file_data) -> data.exception_refs)
      ()
  in

  (* exception_decls: level 2 *)
  let exception_decls =
    flatMap ~name:"exception_decls" decls
      ~f:(fun pos () ->
        if String.length pos > 0 && pos.[0] = 'E' then [(pos, ())] else [])
      ()
  in

  (* resolved_exception_refs: join at level 3 *)
  let resolved_exception_refs =
    join ~name:"resolved_exception_refs" exception_refs_raw exception_decls
      ~key_of:(fun path _loc -> path)
      ~f:(fun path loc decl_opt ->
        match decl_opt with
        | Some () -> [(path, loc)]
        | None -> [])
      ()
  in

  (* resolved_refs_from: level 4 *)
  let resolved_refs_from =
    flatMap ~name:"resolved_refs_from" resolved_exception_refs
      ~f:(fun posTo posFrom -> [(posFrom, posTo)])
      ()
  in

  (* value_refs_from: union at level 5 *)
  let value_refs_from =
    union ~name:"value_refs_from" merged_value_refs resolved_refs_from ()
  in

  (* external_value_refs: join at level 6 *)
  let external_value_refs =
    join ~name:"external_value_refs" value_refs_from decls
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_opt ->
        match decl_opt with
        | Some () -> []
        | None -> [(posTo, ())])
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_value_refs in

  emit
    (Batch
       [
         set "file1"
           {
             value_refs = [("A", "X")];
             exception_refs = [("E1", "Y")];
             full_decls = ["A"; "E1"];
           };
       ]);

  let _adds, removes = sum_deltas !deltas in
  Printf.printf "removes=%d, len=%d\n" removes (length external_value_refs);

  assert (removes = 0);
  Printf.printf "PASSED\n\n"

(** Test: Separate sources - removals are expected here *)
let test_separate_sources () =
  reset ();
  Printf.printf "=== Test: separate sources (removals expected) ===\n";

  let refs_src, emit_refs = source ~name:"refs_source" () in
  let decls_src, emit_decls = source ~name:"decls_source" () in

  let external_refs =
    join ~name:"external_refs" refs_src decls_src
      ~key_of:(fun posFrom _posTo -> posFrom)
      ~f:(fun _posFrom posTo decl_opt ->
        match decl_opt with
        | Some () -> []
        | None -> [(posTo, ())])
      ~merge:(fun () () -> ())
      ()
  in

  let deltas = track_deltas external_refs in

  (* Refs arrive first *)
  emit_refs (Batch [set "A" "X"; set "B" "Y"; set "C" "Z"]);

  let adds1, _ = sum_deltas !deltas in
  Printf.printf "After refs: adds=%d, len=%d\n" adds1 (length external_refs);

  (* Decls arrive second - causes removals *)
  emit_decls (Batch [set "A" (); set "B" ()]);

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
