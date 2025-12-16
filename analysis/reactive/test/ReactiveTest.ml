(** Tests for Reactive collections *)

open Reactive

(** {1 Helper functions} *)

let read_lines path =
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !lines

let write_lines path lines =
  let oc = open_out path in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines;
  close_out oc

(** {1 Tests} *)

let test_flatmap_basic () =
  Printf.printf "=== Test: flatMap basic ===\n";

  (* Create a simple source collection *)
  let data : (int, string) Hashtbl.t = Hashtbl.create 16 in
  let subscribers : ((int, string) delta -> unit) list ref = ref [] in

  let source : (int, string) t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f data);
      get = (fun k -> Hashtbl.find_opt data k);
      length = (fun () -> Hashtbl.length data);
    }
  in

  let emit delta =
    apply_delta data delta;
    List.iter (fun h -> h delta) !subscribers
  in

  (* Create derived collection via flatMap *)
  let derived =
    flatMap source
      ~f:(fun key value ->
        [(key * 10, value); ((key * 10) + 1, value); ((key * 10) + 2, value)])
      ()
  in

  (* Add entry -> derived should have 3 entries *)
  emit (Set (1, "a"));
  Printf.printf "After Set(1, 'a'): derived has %d entries\n" (length derived);
  assert (length derived = 3);
  assert (get derived 10 = Some "a");
  assert (get derived 11 = Some "a");
  assert (get derived 12 = Some "a");

  (* Add another entry *)
  emit (Set (2, "b"));
  Printf.printf "After Set(2, 'b'): derived has %d entries\n" (length derived);
  assert (length derived = 6);

  (* Update entry *)
  emit (Set (1, "A"));
  Printf.printf "After Set(1, 'A'): derived has %d entries\n" (length derived);
  assert (get derived 10 = Some "A");
  assert (length derived = 6);

  (* Remove entry *)
  emit (Remove 1);
  Printf.printf "After Remove(1): derived has %d entries\n" (length derived);
  assert (length derived = 3);
  assert (get derived 10 = None);
  assert (get derived 20 = Some "b");

  Printf.printf "PASSED\n\n"

module IntSet = Set.Make (Int)

let test_flatmap_with_merge () =
  Printf.printf "=== Test: flatMap with merge ===\n";

  let data : (int, IntSet.t) Hashtbl.t = Hashtbl.create 16 in
  let subscribers : ((int, IntSet.t) delta -> unit) list ref = ref [] in

  let source : (int, IntSet.t) t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f data);
      get = (fun k -> Hashtbl.find_opt data k);
      length = (fun () -> Hashtbl.length data);
    }
  in

  let emit delta =
    apply_delta data delta;
    List.iter (fun h -> h delta) !subscribers
  in

  (* Create derived with merge *)
  let derived =
    flatMap source
      ~f:(fun _key values -> [(0, values)]) (* all contribute to key 0 *)
      ~merge:IntSet.union ()
  in

  (* Source 1 contributes {1, 2} *)
  emit (Set (1, IntSet.of_list [1; 2]));
  let v = get derived 0 |> Option.get in
  Printf.printf "After source 1: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2]));

  (* Source 2 contributes {3, 4} -> should merge *)
  emit (Set (2, IntSet.of_list [3; 4]));
  let v = get derived 0 |> Option.get in
  Printf.printf "After source 2: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2; 3; 4]));

  (* Remove source 1 *)
  emit (Remove 1);
  let v = get derived 0 |> Option.get in
  Printf.printf "After remove 1: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [3; 4]));

  Printf.printf "PASSED\n\n"

let test_composition () =
  Printf.printf "=== Test: composition (flatMap chain) ===\n";

  (* Source: file -> list of items *)
  let data : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let subscribers : ((string, string list) delta -> unit) list ref = ref [] in

  let source : (string, string list) t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f data);
      get = (fun k -> Hashtbl.find_opt data k);
      length = (fun () -> Hashtbl.length data);
    }
  in

  let emit delta =
    apply_delta data delta;
    List.iter (fun h -> h delta) !subscribers
  in

  (* First flatMap: file -> items *)
  let items =
    flatMap source
      ~f:(fun path items ->
        List.mapi (fun i item -> (Printf.sprintf "%s:%d" path i, item)) items)
      ()
  in

  (* Second flatMap: item -> chars *)
  let chars =
    flatMap items
      ~f:(fun key value ->
        String.to_seq value
        |> Seq.mapi (fun i c -> (Printf.sprintf "%s:%d" key i, c))
        |> List.of_seq)
      ()
  in

  (* Add file with 2 items *)
  emit (Set ("file1", ["ab"; "cd"]));
  Printf.printf "After file1: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 2);
  assert (length chars = 4);

  (* Add another file *)
  emit (Set ("file2", ["xyz"]));
  Printf.printf "After file2: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 3);
  assert (length chars = 7);

  (* Update file1 *)
  emit (Set ("file1", ["a"]));
  Printf.printf "After update file1: items=%d, chars=%d\n" (length items)
    (length chars);
  assert (length items = 2);
  (* 1 from file1 + 1 from file2 *)
  assert (length chars = 4);

  (* 1 from file1 + 3 from file2 *)
  Printf.printf "PASSED\n\n"

let test_flatmap_on_existing_data () =
  Printf.printf "=== Test: flatMap on collection with existing data ===\n";

  (* Create source with data already in it *)
  let data : (int, string) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.add data 1 "a";
  Hashtbl.add data 2 "b";

  let subscribers : ((int, string) delta -> unit) list ref = ref [] in

  let source : (int, string) t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f data);
      get = (fun k -> Hashtbl.find_opt data k);
      length = (fun () -> Hashtbl.length data);
    }
  in

  Printf.printf "Source has %d entries before flatMap\n" (length source);

  (* Create flatMap AFTER source has data *)
  let derived = flatMap source ~f:(fun k v -> [(k * 10, v)]) () in

  (* Check derived has existing data *)
  Printf.printf "Derived has %d entries (expected 2)\n" (length derived);
  assert (length derived = 2);
  assert (get derived 10 = Some "a");
  assert (get derived 20 = Some "b");

  Printf.printf "PASSED\n\n"

module StringMap = Map.Make (String)

let test_file_collection () =
  Printf.printf "=== Test: ReactiveFileCollection + composition ===\n";

  (* Create temp files with words *)
  let temp_dir = Filename.get_temp_dir_name () in
  let file_a = Filename.concat temp_dir "reactive_test_a.txt" in
  let file_b = Filename.concat temp_dir "reactive_test_b.txt" in

  (* file_a: hello(2), world(1) *)
  write_lines file_a ["hello world"; "hello"];
  (* file_b: hello(1), foo(1) *)
  write_lines file_b ["hello foo"];

  (* Create file collection: file -> word count map *)
  let files =
    ReactiveFileCollection.create ~read_file:read_lines
      ~process:(fun _path lines ->
        (* Count words within this file *)
        let counts = ref StringMap.empty in
        lines
        |> List.iter (fun line ->
               String.split_on_char ' ' line
               |> List.iter (fun word ->
                      let c =
                        StringMap.find_opt word !counts
                        |> Option.value ~default:0
                      in
                      counts := StringMap.add word (c + 1) !counts));
        !counts)
  in

  (* First flatMap: aggregate word counts across files with merge *)
  let word_counts =
    Reactive.flatMap
      (ReactiveFileCollection.to_collection files)
      ~f:(fun _path counts -> StringMap.bindings counts)
        (* Each file contributes its word counts *)
      ~merge:( + ) (* Sum counts from multiple files *)
      ()
  in

  (* Second flatMap: filter to words with count >= 2 *)
  let frequent_words =
    Reactive.flatMap word_counts
      ~f:(fun word count -> if count >= 2 then [(word, count)] else [])
      ()
  in

  (* Process files *)
  ReactiveFileCollection.process_files files [file_a; file_b];

  Printf.printf "Word counts:\n";
  word_counts
  |> Reactive.iter (fun word count -> Printf.printf "  %s: %d\n" word count);

  Printf.printf "Frequent words (count >= 2):\n";
  frequent_words
  |> Reactive.iter (fun word count -> Printf.printf "  %s: %d\n" word count);

  (* Verify: hello=3 (2 from a + 1 from b), world=1, foo=1 *)
  assert (Reactive.get word_counts "hello" = Some 3);
  assert (Reactive.get word_counts "world" = Some 1);
  assert (Reactive.get word_counts "foo" = Some 1);
  assert (Reactive.length word_counts = 3);

  (* Verify frequent: only "hello" with count 3 *)
  assert (Reactive.length frequent_words = 1);
  assert (Reactive.get frequent_words "hello" = Some 3);

  (* Modify file_a: now hello(1), world(2) *)
  Printf.printf "\nModifying file_a...\n";
  write_lines file_a ["world world"; "hello"];
  ReactiveFileCollection.process_files files [file_a];

  Printf.printf "Word counts after modification:\n";
  Reactive.iter
    (fun word count -> Printf.printf "  %s: %d\n" word count)
    word_counts;

  Printf.printf "Frequent words after modification:\n";
  Reactive.iter
    (fun word count -> Printf.printf "  %s: %d\n" word count)
    frequent_words;

  (* Verify: hello=2 (1 from a + 1 from b), world=2, foo=1 *)
  assert (Reactive.get word_counts "hello" = Some 2);
  assert (Reactive.get word_counts "world" = Some 2);
  assert (Reactive.get word_counts "foo" = Some 1);

  (* Verify frequent: hello=2, world=2 *)
  assert (Reactive.length frequent_words = 2);
  assert (Reactive.get frequent_words "hello" = Some 2);
  assert (Reactive.get frequent_words "world" = Some 2);

  (* Cleanup *)
  Sys.remove file_a;
  Sys.remove file_b;

  Printf.printf "PASSED\n\n"

let test_lookup () =
  Printf.printf "=== Test: lookup (reactive single-key subscription) ===\n";

  let data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let subscribers : ((string, int) delta -> unit) list ref = ref [] in

  let source : (string, int) t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f data);
      get = (fun k -> Hashtbl.find_opt data k);
      length = (fun () -> Hashtbl.length data);
    }
  in

  let emit delta =
    apply_delta data delta;
    List.iter (fun h -> h delta) !subscribers
  in

  (* Create lookup for key "foo" *)
  let foo_lookup = lookup source ~key:"foo" in

  (* Initially empty *)
  assert (length foo_lookup = 0);
  assert (get foo_lookup "foo" = None);

  (* Set foo=42 *)
  emit (Set ("foo", 42));
  Printf.printf "After Set(foo, 42): lookup has %d entries\n"
    (length foo_lookup);
  assert (length foo_lookup = 1);
  assert (get foo_lookup "foo" = Some 42);

  (* Set bar=100 (different key, lookup shouldn't change) *)
  emit (Set ("bar", 100));
  Printf.printf "After Set(bar, 100): lookup still has %d entries\n"
    (length foo_lookup);
  assert (length foo_lookup = 1);
  assert (get foo_lookup "foo" = Some 42);

  (* Update foo=99 *)
  emit (Set ("foo", 99));
  Printf.printf "After Set(foo, 99): lookup value updated\n";
  assert (get foo_lookup "foo" = Some 99);

  (* Track subscription updates *)
  let updates = ref [] in
  foo_lookup.subscribe (fun delta -> updates := delta :: !updates);

  emit (Set ("foo", 1));
  emit (Set ("bar", 2));
  emit (Remove "foo");

  Printf.printf
    "Subscription received %d updates (expected 2: Set+Remove for foo)\n"
    (List.length !updates);
  assert (List.length !updates = 2);

  Printf.printf "PASSED\n\n"

let test_join () =
  Printf.printf "=== Test: join (reactive lookup/join) ===\n";

  (* Left collection: exception refs (path -> loc_from) *)
  let left_data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let left_subs : ((string, int) delta -> unit) list ref = ref [] in
  let left : (string, int) t =
    {
      subscribe = (fun h -> left_subs := h :: !left_subs);
      iter = (fun f -> Hashtbl.iter f left_data);
      get = (fun k -> Hashtbl.find_opt left_data k);
      length = (fun () -> Hashtbl.length left_data);
    }
  in
  let emit_left delta =
    apply_delta left_data delta;
    List.iter (fun h -> h delta) !left_subs
  in

  (* Right collection: decl index (path -> decl_pos) *)
  let right_data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let right_subs : ((string, int) delta -> unit) list ref = ref [] in
  let right : (string, int) t =
    {
      subscribe = (fun h -> right_subs := h :: !right_subs);
      iter = (fun f -> Hashtbl.iter f right_data);
      get = (fun k -> Hashtbl.find_opt right_data k);
      length = (fun () -> Hashtbl.length right_data);
    }
  in
  let emit_right delta =
    apply_delta right_data delta;
    List.iter (fun h -> h delta) !right_subs
  in

  (* Join: for each (path, loc_from) in left, look up path in right *)
  let joined =
    join left right
      ~key_of:(fun path _loc_from -> path)
      ~f:(fun _path loc_from decl_pos_opt ->
        match decl_pos_opt with
        | Some decl_pos ->
          (* Produce (decl_pos, loc_from) pairs *)
          [(decl_pos, loc_from)]
        | None -> [])
      ()
  in

  (* Initially empty *)
  assert (length joined = 0);

  (* Add declaration at path "A" with pos 100 *)
  emit_right (Set ("A", 100));
  Printf.printf "After right Set(A, 100): joined=%d\n" (length joined);
  assert (length joined = 0);

  (* No left entries yet *)

  (* Add exception ref at path "A" from loc 1 *)
  emit_left (Set ("A", 1));
  Printf.printf "After left Set(A, 1): joined=%d\n" (length joined);
  assert (length joined = 1);
  assert (get joined 100 = Some 1);

  (* decl_pos 100 -> loc_from 1 *)

  (* Add another exception ref at path "B" (no matching decl) *)
  emit_left (Set ("B", 2));
  Printf.printf "After left Set(B, 2): joined=%d (B has no decl)\n"
    (length joined);
  assert (length joined = 1);

  (* Add declaration for path "B" *)
  emit_right (Set ("B", 200));
  Printf.printf "After right Set(B, 200): joined=%d\n" (length joined);
  assert (length joined = 2);
  assert (get joined 200 = Some 2);

  (* Update right: change B's decl_pos *)
  emit_right (Set ("B", 201));
  Printf.printf "After right Set(B, 201): joined=%d\n" (length joined);
  assert (length joined = 2);
  assert (get joined 200 = None);
  (* Old key gone *)
  assert (get joined 201 = Some 2);

  (* New key has the value *)

  (* Remove left entry A *)
  emit_left (Remove "A");
  Printf.printf "After left Remove(A): joined=%d\n" (length joined);
  assert (length joined = 1);
  assert (get joined 100 = None);

  Printf.printf "PASSED\n\n"

let test_join_with_merge () =
  Printf.printf "=== Test: join with merge ===\n";

  (* Multiple left entries can map to same right key *)
  let left_data : (int, string) Hashtbl.t = Hashtbl.create 16 in
  let left_subs : ((int, string) delta -> unit) list ref = ref [] in
  let left : (int, string) t =
    {
      subscribe = (fun h -> left_subs := h :: !left_subs);
      iter = (fun f -> Hashtbl.iter f left_data);
      get = (fun k -> Hashtbl.find_opt left_data k);
      length = (fun () -> Hashtbl.length left_data);
    }
  in
  let emit_left delta =
    apply_delta left_data delta;
    List.iter (fun h -> h delta) !left_subs
  in

  let right_data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let right_subs : ((string, int) delta -> unit) list ref = ref [] in
  let right : (string, int) t =
    {
      subscribe = (fun h -> right_subs := h :: !right_subs);
      iter = (fun f -> Hashtbl.iter f right_data);
      get = (fun k -> Hashtbl.find_opt right_data k);
      length = (fun () -> Hashtbl.length right_data);
    }
  in
  let emit_right delta =
    apply_delta right_data delta;
    List.iter (fun h -> h delta) !right_subs
  in

  (* Join with merge: all entries produce to key 0 *)
  let joined =
    join left right
      ~key_of:(fun _id path -> path) (* Look up by path *)
      ~f:(fun _id _path value_opt ->
        match value_opt with
        | Some v -> [(0, v)] (* All contribute to key 0 *)
        | None -> [])
      ~merge:( + ) (* Sum values *)
      ()
  in

  emit_right (Set ("X", 10));
  emit_left (Set (1, "X"));
  emit_left (Set (2, "X"));

  Printf.printf "Two entries looking up X (value 10): sum=%d\n"
    (get joined 0 |> Option.value ~default:0);
  assert (get joined 0 = Some 20);

  (* 10 + 10 *)
  emit_right (Set ("X", 5));
  Printf.printf "After right changes to 5: sum=%d\n"
    (get joined 0 |> Option.value ~default:0);
  assert (get joined 0 = Some 10);

  (* 5 + 5 *)
  emit_left (Remove 1);
  Printf.printf "After removing one left entry: sum=%d\n"
    (get joined 0 |> Option.value ~default:0);
  assert (get joined 0 = Some 5);

  (* Only one left *)
  Printf.printf "PASSED\n\n"

let () =
  Printf.printf "\n====== Reactive Collection Tests ======\n\n";
  test_flatmap_basic ();
  test_flatmap_with_merge ();
  test_composition ();
  test_flatmap_on_existing_data ();
  test_file_collection ();
  test_lookup ();
  test_join ();
  test_join_with_merge ();
  Printf.printf "All tests passed!\n"
