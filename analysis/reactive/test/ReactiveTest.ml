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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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
      stats = create_stats ();
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

(* Test union *)
let test_union_basic () =
  Printf.printf "=== Test: union basic ===\n";

  (* Left collection *)
  let left_data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let left_subs : ((string, int) delta -> unit) list ref = ref [] in
  let left : (string, int) t =
    {
      subscribe = (fun h -> left_subs := h :: !left_subs);
      iter = (fun f -> Hashtbl.iter f left_data);
      get = (fun k -> Hashtbl.find_opt left_data k);
      length = (fun () -> Hashtbl.length left_data);
      stats = create_stats ();
    }
  in
  let emit_left delta =
    apply_delta left_data delta;
    List.iter (fun h -> h delta) !left_subs
  in

  (* Right collection *)
  let right_data : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let right_subs : ((string, int) delta -> unit) list ref = ref [] in
  let right : (string, int) t =
    {
      subscribe = (fun h -> right_subs := h :: !right_subs);
      iter = (fun f -> Hashtbl.iter f right_data);
      get = (fun k -> Hashtbl.find_opt right_data k);
      length = (fun () -> Hashtbl.length right_data);
      stats = create_stats ();
    }
  in
  let emit_right delta =
    apply_delta right_data delta;
    List.iter (fun h -> h delta) !right_subs
  in

  (* Create union without merge (right takes precedence) *)
  let combined = Reactive.union left right () in

  (* Initially empty *)
  assert (length combined = 0);

  (* Add to left *)
  emit_left (Set ("a", 1));
  Printf.printf "After left Set(a, 1): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get combined "a" = Some 1);

  (* Add different key to right *)
  emit_right (Set ("b", 2));
  Printf.printf "After right Set(b, 2): combined=%d\n" (length combined);
  assert (length combined = 2);
  assert (get combined "a" = Some 1);
  assert (get combined "b" = Some 2);

  (* Add same key to right (should override left) *)
  emit_right (Set ("a", 10));
  Printf.printf "After right Set(a, 10): combined a=%d\n"
    (get combined "a" |> Option.value ~default:(-1));
  assert (length combined = 2);
  assert (get combined "a" = Some 10);

  (* Right takes precedence *)

  (* Remove from right (left value should show through) *)
  emit_right (Remove "a");
  Printf.printf "After right Remove(a): combined a=%d\n"
    (get combined "a" |> Option.value ~default:(-1));
  assert (get combined "a" = Some 1);

  (* Left shows through *)

  (* Remove from left *)
  emit_left (Remove "a");
  Printf.printf "After left Remove(a): combined=%d\n" (length combined);
  assert (length combined = 1);
  assert (get combined "a" = None);
  assert (get combined "b" = Some 2);

  Printf.printf "PASSED\n\n"

let test_union_with_merge () =
  Printf.printf "=== Test: union with merge ===\n";

  (* Left collection *)
  let left_data : (string, IntSet.t) Hashtbl.t = Hashtbl.create 16 in
  let left_subs : ((string, IntSet.t) delta -> unit) list ref = ref [] in
  let left : (string, IntSet.t) t =
    {
      subscribe = (fun h -> left_subs := h :: !left_subs);
      iter = (fun f -> Hashtbl.iter f left_data);
      get = (fun k -> Hashtbl.find_opt left_data k);
      length = (fun () -> Hashtbl.length left_data);
      stats = create_stats ();
    }
  in
  let emit_left delta =
    apply_delta left_data delta;
    List.iter (fun h -> h delta) !left_subs
  in

  (* Right collection *)
  let right_data : (string, IntSet.t) Hashtbl.t = Hashtbl.create 16 in
  let right_subs : ((string, IntSet.t) delta -> unit) list ref = ref [] in
  let right : (string, IntSet.t) t =
    {
      subscribe = (fun h -> right_subs := h :: !right_subs);
      iter = (fun f -> Hashtbl.iter f right_data);
      get = (fun k -> Hashtbl.find_opt right_data k);
      length = (fun () -> Hashtbl.length right_data);
      stats = create_stats ();
    }
  in
  let emit_right delta =
    apply_delta right_data delta;
    List.iter (fun h -> h delta) !right_subs
  in

  (* Create union with set union as merge *)
  let combined = Reactive.union left right ~merge:IntSet.union () in

  (* Add to left: key "x" -> {1, 2} *)
  emit_left (Set ("x", IntSet.of_list [1; 2]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After left Set(x, {1,2}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2]));

  (* Add to right: key "x" -> {3, 4} (should merge) *)
  emit_right (Set ("x", IntSet.of_list [3; 4]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After right Set(x, {3,4}): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 2; 3; 4]));

  (* Update left: key "x" -> {1, 5} *)
  emit_left (Set ("x", IntSet.of_list [1; 5]));
  let v = get combined "x" |> Option.get in
  Printf.printf "After left update to {1,5}: {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 3; 4; 5]));

  (* Remove right *)
  emit_right (Remove "x");
  let v = get combined "x" |> Option.get in
  Printf.printf "After right Remove(x): {%s}\n"
    (IntSet.elements v |> List.map string_of_int |> String.concat ", ");
  assert (IntSet.equal v (IntSet.of_list [1; 5]));

  Printf.printf "PASSED\n\n"

let test_union_existing_data () =
  Printf.printf "=== Test: union on collections with existing data ===\n";

  (* Create collections with existing data *)
  let left_data : (int, string) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.add left_data 1 "a";
  Hashtbl.add left_data 2 "b";
  let left_subs : ((int, string) delta -> unit) list ref = ref [] in
  let left : (int, string) t =
    {
      subscribe = (fun h -> left_subs := h :: !left_subs);
      iter = (fun f -> Hashtbl.iter f left_data);
      get = (fun k -> Hashtbl.find_opt left_data k);
      length = (fun () -> Hashtbl.length left_data);
      stats = create_stats ();
    }
  in

  let right_data : (int, string) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.add right_data 2 "B";
  (* Overlaps with left *)
  Hashtbl.add right_data 3 "c";
  let right_subs : ((int, string) delta -> unit) list ref = ref [] in
  let right : (int, string) t =
    {
      subscribe = (fun h -> right_subs := h :: !right_subs);
      iter = (fun f -> Hashtbl.iter f right_data);
      get = (fun k -> Hashtbl.find_opt right_data k);
      length = (fun () -> Hashtbl.length right_data);
      stats = create_stats ();
    }
  in

  (* Create union after both have data *)
  let combined = Reactive.union left right () in

  Printf.printf "Union has %d entries (expected 3)\n" (length combined);
  assert (length combined = 3);
  assert (get combined 1 = Some "a");
  (* Only in left *)
  assert (get combined 2 = Some "B");
  (* Right takes precedence *)
  assert (get combined 3 = Some "c");

  (* Only in right *)
  Printf.printf "PASSED\n\n"

(* Helper to create mutable reactive collections for testing *)
let create_mutable_collection () =
  let tbl = Hashtbl.create 16 in
  let subscribers = ref [] in
  let my_stats = Reactive.create_stats () in
  let emit delta =
    my_stats.updates_emitted <- my_stats.updates_emitted + 1;
    Reactive.apply_delta tbl delta;
    List.iter (fun h -> h delta) !subscribers
  in
  let collection : ('k, 'v) Reactive.t =
    {
      subscribe = (fun h -> subscribers := h :: !subscribers);
      iter = (fun f -> Hashtbl.iter f tbl);
      get = (fun k -> Hashtbl.find_opt tbl k);
      length = (fun () -> Hashtbl.length tbl);
      stats = my_stats;
    }
  in
  (collection, emit, tbl)

(* Test fixpoint basic *)
let test_fixpoint () =
  Printf.printf "Test: fixpoint\n";

  let init, emit_init, _init_tbl = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Set up graph: 1 -> [2, 3], 2 -> [4], 3 -> [4] *)
  Hashtbl.replace edges_tbl 1 [2; 3];
  Hashtbl.replace edges_tbl 2 [4];
  Hashtbl.replace edges_tbl 3 [4];

  (* Compute fixpoint *)
  let reachable = Reactive.fixpoint ~init ~edges () in

  (* Initially empty *)
  Printf.printf "Initially: length=%d\n" (Reactive.length reachable);
  assert (Reactive.length reachable = 0);

  (* Add root 1 *)
  emit_init (Set (1, ()));
  Printf.printf "After adding root 1: length=%d\n" (Reactive.length reachable);
  assert (Reactive.length reachable = 4);
  (* 1, 2, 3, 4 *)
  assert (Reactive.get reachable 1 = Some ());
  assert (Reactive.get reachable 2 = Some ());
  assert (Reactive.get reachable 3 = Some ());
  assert (Reactive.get reachable 4 = Some ());
  assert (Reactive.get reachable 5 = None);

  (* Add another root 5 with edge 5 -> [6] *)
  emit_edges (Set (5, [6]));
  emit_init (Set (5, ()));
  Printf.printf "After adding root 5: length=%d\n" (Reactive.length reachable);
  assert (Reactive.length reachable = 6);

  (* 1, 2, 3, 4, 5, 6 *)

  (* Remove root 1 *)
  emit_init (Remove 1);
  Printf.printf "After removing root 1: length=%d\n" (Reactive.length reachable);
  assert (Reactive.length reachable = 2);
  (* 5, 6 *)
  assert (Reactive.get reachable 1 = None);
  assert (Reactive.get reachable 5 = Some ());
  assert (Reactive.get reachable 6 = Some ());

  Printf.printf "PASSED\n\n"

(* Test: Basic Expansion *)
let test_fixpoint_basic_expansion () =
  Printf.printf "=== Test: fixpoint basic expansion ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b -> c *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));

  assert (Reactive.length fp = 3);
  assert (Reactive.get fp "a" = Some ());
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.get fp "c" = Some ());
  assert (Reactive.get fp "d" = None);

  Printf.printf "PASSED\n\n"

(* Test: Multiple Roots *)
let test_fixpoint_multiple_roots () =
  Printf.printf "=== Test: fixpoint multiple roots ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b, c -> d (disconnected components) *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "c" ["d"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  emit_init (Set ("c", ()));

  assert (Reactive.length fp = 4);
  assert (Reactive.get fp "a" = Some ());
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.get fp "c" = Some ());
  assert (Reactive.get fp "d" = Some ());

  Printf.printf "PASSED\n\n"

(* Test: Diamond Graph *)
let test_fixpoint_diamond () =
  Printf.printf "=== Test: fixpoint diamond ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b, a -> c, b -> d, c -> d *)
  Hashtbl.replace edges_tbl "a" ["b"; "c"];
  Hashtbl.replace edges_tbl "b" ["d"];
  Hashtbl.replace edges_tbl "c" ["d"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));

  assert (Reactive.length fp = 4);

  Printf.printf "PASSED\n\n"

(* Test: Cycle *)
let test_fixpoint_cycle () =
  Printf.printf "=== Test: fixpoint cycle ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b -> c -> b (cycle from root) *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];
  Hashtbl.replace edges_tbl "c" ["b"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));

  assert (Reactive.length fp = 3);
  assert (Reactive.get fp "a" = Some ());
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.get fp "c" = Some ());

  Printf.printf "PASSED\n\n"

(* Test: Add Base Element *)
let test_fixpoint_add_base () =
  Printf.printf "=== Test: fixpoint add base ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b, c -> d *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "c" ["d"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 2);

  (* a, b *)

  (* Track changes via subscription *)
  let added = ref [] in
  let removed = ref [] in
  fp.subscribe (function
    | Set (k, ()) -> added := k :: !added
    | Remove k -> removed := k :: !removed);

  emit_init (Set ("c", ()));

  Printf.printf "Added: [%s]\n" (String.concat ", " !added);
  assert (List.length !added = 2);
  (* c, d *)
  assert (List.mem "c" !added);
  assert (List.mem "d" !added);
  assert (!removed = []);
  assert (Reactive.length fp = 4);

  Printf.printf "PASSED\n\n"

(* Test: Remove Base Element *)
let test_fixpoint_remove_base () =
  Printf.printf "=== Test: fixpoint remove base ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b -> c *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 3);

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  emit_init (Remove "a");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 3);
  assert (Reactive.length fp = 0);

  Printf.printf "PASSED\n\n"

(* Test: Add Edge *)
let test_fixpoint_add_edge () =
  Printf.printf "=== Test: fixpoint add edge ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, _ = create_mutable_collection () in

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 1);

  (* just a *)
  let added = ref [] in
  fp.subscribe (function
    | Set (k, ()) -> added := k :: !added
    | _ -> ());

  (* Add edge a -> b *)
  emit_edges (Set ("a", ["b"]));

  Printf.printf "Added: [%s]\n" (String.concat ", " !added);
  assert (List.mem "b" !added);
  assert (Reactive.length fp = 2);

  Printf.printf "PASSED\n\n"

(* Test: Remove Edge *)
let test_fixpoint_remove_edge () =
  Printf.printf "=== Test: fixpoint remove edge ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b -> c *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 3);

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Remove edge a -> b *)
  emit_edges (Set ("a", []));

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  assert (List.length !removed = 2);
  (* b, c *)
  assert (Reactive.length fp = 1);

  (* just a *)
  Printf.printf "PASSED\n\n"

(* Test: Cycle Removal (Well-Founded Derivation) *)
let test_fixpoint_cycle_removal () =
  Printf.printf "=== Test: fixpoint cycle removal (well-founded) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b -> c -> b (b-c cycle reachable from a) *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];
  Hashtbl.replace edges_tbl "c" ["b"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 3);

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Remove edge a -> b *)
  emit_edges (Set ("a", []));

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  (* Both b and c should be removed - cycle has no well-founded support *)
  assert (List.length !removed = 2);
  assert (List.mem "b" !removed);
  assert (List.mem "c" !removed);
  assert (Reactive.length fp = 1);

  (* just a *)
  Printf.printf "PASSED\n\n"

(* Test: Alternative Support Keeps Element Alive *)
let test_fixpoint_alternative_support () =
  Printf.printf "=== Test: fixpoint alternative support ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Graph: a -> b, a -> c -> b
     If we remove a -> b, b should survive via a -> c -> b *)
  Hashtbl.replace edges_tbl "a" ["b"; "c"];
  Hashtbl.replace edges_tbl "c" ["b"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));
  assert (Reactive.length fp = 3);

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Remove direct edge a -> b (but keep a -> c) *)
  emit_edges (Set ("a", ["c"]));

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);
  (* b should NOT be removed - still reachable via c *)
  assert (!removed = []);
  assert (Reactive.length fp = 3);

  Printf.printf "PASSED\n\n"

(* Test: Empty Base *)
let test_fixpoint_empty_base () =
  Printf.printf "=== Test: fixpoint empty base ===\n";

  let init, _, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  Hashtbl.replace edges_tbl "a" ["b"];

  let fp = Reactive.fixpoint ~init ~edges () in

  assert (Reactive.length fp = 0);

  Printf.printf "PASSED\n\n"

(* Test: Self Loop *)
let test_fixpoint_self_loop () =
  Printf.printf "=== Test: fixpoint self loop ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _, edges_tbl = create_mutable_collection () in

  (* Graph: a -> a (self loop) *)
  Hashtbl.replace edges_tbl "a" ["a"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("a", ()));

  assert (Reactive.length fp = 1);
  assert (Reactive.get fp "a" = Some ());

  Printf.printf "PASSED\n\n"

(* Test: Delta emissions for incremental updates *)
let test_fixpoint_deltas () =
  Printf.printf "=== Test: fixpoint delta emissions ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  Hashtbl.replace edges_tbl 1 [2; 3];
  Hashtbl.replace edges_tbl 2 [4];

  let fp = Reactive.fixpoint ~init ~edges () in

  let all_deltas = ref [] in
  fp.subscribe (fun d -> all_deltas := d :: !all_deltas);

  (* Add root *)
  emit_init (Set (1, ()));
  Printf.printf "After add root: %d deltas\n" (List.length !all_deltas);
  assert (List.length !all_deltas = 4);

  (* 1, 2, 3, 4 *)
  all_deltas := [];

  (* Add edge 3 -> 5 *)
  emit_edges (Set (3, [5]));
  Printf.printf "After add edge 3->5: %d deltas\n" (List.length !all_deltas);
  assert (List.length !all_deltas = 1);

  (* 5 added *)
  all_deltas := [];

  (* Remove root (should remove all) *)
  emit_init (Remove 1);
  Printf.printf "After remove root: %d deltas\n" (List.length !all_deltas);
  assert (List.length !all_deltas = 5);

  (* 1, 2, 3, 4, 5 removed *)
  Printf.printf "PASSED\n\n"

(* Test: Remove from init but still reachable via edges
   This test reproduces a real bug found in the dead code analysis:
   - A reference arrives before its declaration exists
   - The reference target is incorrectly marked as "externally referenced" (a root)
   - When the declaration arrives, the target is removed from roots
   - But it should remain live because it's reachable from other live nodes
   
   Graph: root (true root) -> a -> b
   Scenario:
   1. Initially, b is spuriously in init (before we know it has a declaration)
   2. Later, b is removed from init (when declaration is discovered)
   3. Bug: b incorrectly removed from fixpoint
   4. Correct: b should stay live (reachable via root -> a -> b) *)
let test_fixpoint_remove_spurious_root () =
  Printf.printf
    "=== Test: fixpoint remove spurious root (still reachable) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, _ = create_mutable_collection () in

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Track all deltas *)
  let added = ref [] in
  let removed = ref [] in
  fp.subscribe (function
    | Set (k, ()) -> added := k :: !added
    | Remove k -> removed := k :: !removed);

  (* Step 1: "b" is spuriously marked as a root 
     (in the real bug, this happens when a reference arrives before its declaration) *)
  emit_init (Set ("b", ()));
  Printf.printf "After spurious root b: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  assert (Reactive.get fp "b" = Some ());

  (* Step 2: The real root "root" is added *)
  emit_init (Set ("root", ()));
  Printf.printf "After true root: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  (* Step 3: Edge root -> a is added *)
  emit_edges (Set ("root", ["a"]));
  Printf.printf "After edge root->a: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  assert (Reactive.get fp "a" = Some ());

  (* Step 4: Edge a -> b is added *)
  emit_edges (Set ("a", ["b"]));
  Printf.printf "After edge a->b: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  (* At this point: root, a, b are all in fixpoint *)
  assert (Reactive.length fp = 3);

  (* Clear tracked changes *)
  added := [];
  removed := [];

  (* Step 5: The spurious root "b" is REMOVED from init
     (in real bug, this happens when declaration for b is discovered,
      showing b is NOT externally referenced - just referenced by a)
     
     BUG: b gets removed from fixpoint
     CORRECT: b should stay because it's still reachable via root -> a -> b *)
  emit_init (Remove "b");

  Printf.printf "After removing b from init: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via a *)
  assert (not (List.mem "b" !removed));
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.length fp = 3);

  Printf.printf "PASSED\n\n"

(* Test: Remove entire edge entry but target still reachable via other source
   
   This tests the `Remove source` case in apply_edges_delta.
   When an entire edge entry is removed, targets may still be reachable
   via edges from OTHER sources.
   
   Graph: a -> b, c -> b (b has two derivations)
   Scenario:
   1. a and c are roots
   2. Both derive b
   3. Remove the entire edge entry for "a" (Remove "a" from edges)
   4. b should stay (still reachable via c -> b) *)
let test_fixpoint_remove_edge_entry_alternative_source () =
  Printf.printf
    "=== Test: fixpoint remove edge entry (alternative source) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Set up initial edges: a -> b, c -> b *)
  Hashtbl.replace edges_tbl "a" ["b"];
  Hashtbl.replace edges_tbl "c" ["b"];

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Add roots a and c *)
  emit_init (Set ("a", ()));
  emit_init (Set ("c", ()));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  (* Should have a, b, c *)
  assert (Reactive.length fp = 3);
  assert (Reactive.get fp "a" = Some ());
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.get fp "c" = Some ());

  removed := [];

  (* Remove entire edge entry for "a" *)
  emit_edges (Remove "a");

  Printf.printf "After Remove edge entry 'a': fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* b should NOT be removed - still reachable via c -> b *)
  assert (not (List.mem "b" !removed));
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.length fp = 3);

  Printf.printf "PASSED\n\n"

(* Test: Remove edge entry - target reachable via higher-ranked source
   
   This is the subtle case where re-derivation check matters.
   When contraction removes an element, but a surviving source with
   HIGHER rank still points to it, well-founded check fails but
   re-derivation should save it.
   
   Graph: root -> a -> b -> c, a -> c (c reachable via b and directly from a)
   Key: c is first reached via a (rank 1), not via b (rank 2)
   When we remove a->c edge, c still has b->c, but rank[b] >= rank[c]
   so well-founded check fails. But c should be re-derived from b.
*)
let test_fixpoint_remove_edge_rederivation () =
  Printf.printf "=== Test: fixpoint remove edge (re-derivation needed) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, _ = create_mutable_collection () in

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | Set (k, ()) -> added := k :: !added);

  (* Add root *)
  emit_init (Set ("root", ()));

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edges (Set ("root", ["a"]));
  emit_edges (Set ("a", ["b"; "c"]));
  (* a reaches both b and c *)
  emit_edges (Set ("b", ["c"]));

  (* b also reaches c *)
  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  (* Should have root, a, b, c *)
  assert (Reactive.length fp = 4);

  (* Check ranks: root=0, a=1, b=2, c=2 (reached from a at level 2) *)
  (* Actually c could be rank 2 (from a) or rank 3 (from b) - depends on BFS order *)
  removed := [];
  added := [];

  (* Remove the direct edge a -> c *)
  emit_edges (Set ("a", ["b"]));

  (* a now only reaches b *)
  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint - reachable via root -> a -> b -> c *)
  assert (Reactive.get fp "c" = Some ());
  assert (Reactive.length fp = 4);

  Printf.printf "PASSED\n\n"

(* Test: Remove edge ENTRY (not Set) - re-derivation case
   
   This specifically tests the `Remove source` case which uses emit_edges (Remove ...)
   rather than emit_edges (Set (..., [])).
   
   Graph: a -> c, b -> c (c reachable from both)
   BFS: a (0), b (0), c (1)
   Key: c has rank 1, both a and b have rank 0
   
   When we remove the entire edge entry for "a" via Remove (not Set),
   c loses derivation from a but should survive via b -> c.
   
   With equal ranks, well-founded check should still find b as support.
*)
let test_fixpoint_remove_edge_entry_rederivation () =
  Printf.printf "=== Test: fixpoint Remove edge entry (re-derivation) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Set up edges before creating fixpoint *)
  Hashtbl.replace edges_tbl "a" ["c"];
  Hashtbl.replace edges_tbl "b" ["c"];

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Add roots a and b *)
  emit_init (Set ("a", ()));
  emit_init (Set ("b", ()));

  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  assert (Reactive.length fp = 3);

  removed := [];

  (* Remove entire edge entry for "a" using Remove delta *)
  emit_edges (Remove "a");

  Printf.printf "After Remove 'a' entry: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* c should survive - b -> c still exists *)
  assert (not (List.mem "c" !removed));
  assert (Reactive.get fp "c" = Some ());
  assert (Reactive.length fp = 3);

  Printf.printf "PASSED\n\n"

(* Test: Remove edge entry - surviving predecessor has HIGHER rank
   
   This is the critical case where re-derivation is needed.
   When well-founded check fails (rank[predecessor] >= rank[target]),
   the target dies. But if the predecessor is surviving, target
   should be re-derived with new rank.
   
   Graph: root -> a -> c, root -> b, b -> c
   BFS order matters here:
   - Level 0: root
   - Level 1: a, b (both from root)
   - Level 2: c (from a, since a is processed first)
   
   c has rank 2, b has rank 1
   inv_index[c] = [a, b]
   
   When we remove "a" entry:
   - c goes into contraction
   - inv_index[c] = [b] 
   - rank[b] = 1 < rank[c] = 2, so b provides support!
   
   Hmm, this still finds support because b has lower rank.
   
   Let me try: root -> a -> b -> c, a -> c
   - Level 0: root
   - Level 1: a
   - Level 2: b, c (both from a, same level)
   
   c has rank 2, b has rank 2
   When we remove a->c edge (not entry), c goes into contraction
   inv_index[c] = [b] (after a removed), rank[b] = rank[c] = 2
   NO support (2 < 2 is false), c dies
   
   But b -> c exists! c should be re-derived with rank 3.
   
   This is the Set case, not Remove. But the logic should be similar.
*)
let test_fixpoint_remove_edge_entry_higher_rank_support () =
  Printf.printf "=== Test: fixpoint edge removal (higher rank support) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, _ = create_mutable_collection () in

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Track changes *)
  let removed = ref [] in
  let added = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | Set (k, ()) -> added := k :: !added);

  (* Add root *)
  emit_init (Set ("root", ()));

  (* Build graph: root -> a -> b -> c, a -> c *)
  emit_edges (Set ("root", ["a"]));
  emit_edges (Set ("a", ["b"; "c"]));
  (* a reaches both b and c at same level *)
  emit_edges (Set ("b", ["c"]));

  (* b also reaches c *)
  Printf.printf "Initial: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));

  assert (Reactive.length fp = 4);
  assert (Reactive.get fp "c" = Some ());

  removed := [];
  added := [];

  (* Remove direct edge a -> c, keeping a -> b *)
  emit_edges (Set ("a", ["b"]));

  Printf.printf "After removing a->c: fp=[%s]\n"
    (let items = ref [] in
     fp.iter (fun k _ -> items := k :: !items);
     String.concat ", " (List.sort String.compare !items));
  Printf.printf "Removed: [%s], Added: [%s]\n"
    (String.concat ", " !removed)
    (String.concat ", " !added);

  (* c should still be in fixpoint via root -> a -> b -> c *)
  (* The re-derivation check should save c even though rank[b] >= rank[c] *)
  assert (Reactive.get fp "c" = Some ());
  assert (Reactive.length fp = 4);

  Printf.printf "PASSED\n\n"

(* Test: Remove edge ENTRY (Remove source) where re-derivation is required.

   This is the classic counterexample: two paths to y, remove the shorter one.
   Without the re-derivation step (reference implementation step 7), contraction
   can incorrectly remove y because its stored rank is too low.

   Graph:
     r -> a -> y         (short path)
     r -> b -> c -> x -> y (long path)

   Scenario:
   1) init = {r}
   2) y is reachable (via a->y)
   3) Remove edge entry for a (emit_edges (Remove "a")), which deletes the short path
   4) y must remain reachable via x->y

   Expected: y stays in fixpoint
   Bug: y is removed if `apply_edges_delta` Remove-case lacks re-derivation. *)
let test_fixpoint_remove_edge_entry_needs_rederivation () =
  Printf.printf
    "=== Test: fixpoint Remove edge entry (needs re-derivation) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, emit_edges, edges_tbl = create_mutable_collection () in

  (* Pre-populate edges so fixpoint initializes with them *)
  Hashtbl.replace edges_tbl "r" ["a"; "b"];
  Hashtbl.replace edges_tbl "a" ["y"];
  Hashtbl.replace edges_tbl "b" ["c"];
  Hashtbl.replace edges_tbl "c" ["x"];
  Hashtbl.replace edges_tbl "x" ["y"];

  let fp = Reactive.fixpoint ~init ~edges () in

  (* Make r live *)
  emit_init (Set ("r", ()));

  (* Sanity: y initially reachable via short path *)
  assert (Reactive.get fp "y" = Some ());
  assert (Reactive.get fp "x" = Some ());

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Remove the entire edge entry for a (removes a->y) *)
  emit_edges (Remove "a");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  (* Correct: y is still reachable via r->b->c->x->y *)
  assert (Reactive.get fp "y" = Some ());

  Printf.printf "PASSED\n\n"

(* Test: Remove BASE element where re-derivation is required.

   Same shape as the verified counterexample, but triggered by removing a base element.
   Verified algorithm applies the re-derivation step after contraction for removed-from-base too.

   Two roots r1 and r2.
   Paths to y:
     r1 -> a -> y            (short path, determines initial rank(y))
     r2 -> b -> c -> x -> y  (long path, still reachable after removing r1)

   Scenario:
   1) init = {r1, r2}
   2) y is reachable (via r1->a->y)
   3) Remove r1 from init (emit_init (Remove "r1")), which deletes the short witness
   4) y must remain reachable via r2->...->y *)
let test_fixpoint_remove_base_needs_rederivation () =
  Printf.printf
    "=== Test: fixpoint Remove base element (needs re-derivation) ===\n";

  let init, emit_init, _ = create_mutable_collection () in
  let edges, _emit_edges, edges_tbl = create_mutable_collection () in

  (* Pre-populate edges so fixpoint initializes with them *)
  Hashtbl.replace edges_tbl "r1" ["a"];
  Hashtbl.replace edges_tbl "a" ["y"];
  Hashtbl.replace edges_tbl "r2" ["b"];
  Hashtbl.replace edges_tbl "b" ["c"];
  Hashtbl.replace edges_tbl "c" ["x"];
  Hashtbl.replace edges_tbl "x" ["y"];

  let fp = Reactive.fixpoint ~init ~edges () in

  emit_init (Set ("r1", ()));
  emit_init (Set ("r2", ()));

  (* Sanity: y initially reachable *)
  assert (Reactive.get fp "y" = Some ());
  assert (Reactive.get fp "x" = Some ());

  let removed = ref [] in
  fp.subscribe (function
    | Remove k -> removed := k :: !removed
    | _ -> ());

  (* Remove r1 from base: y should remain via r2 path *)
  emit_init (Remove "r1");

  Printf.printf "Removed: [%s]\n" (String.concat ", " !removed);

  assert (Reactive.get fp "y" = Some ());
  Printf.printf "PASSED\n\n"

(* Test: Pre-existing data in init and edges *)
let test_fixpoint_existing_data () =
  Printf.printf "=== Test: fixpoint with existing data ===\n";

  (* Create with pre-existing data *)
  let init_tbl = Hashtbl.create 16 in
  Hashtbl.replace init_tbl "root" ();
  let init_subs = ref [] in
  let init : (string, unit) Reactive.t =
    {
      subscribe = (fun h -> init_subs := h :: !init_subs);
      iter = (fun f -> Hashtbl.iter f init_tbl);
      get = (fun k -> Hashtbl.find_opt init_tbl k);
      length = (fun () -> Hashtbl.length init_tbl);
      stats = Reactive.create_stats ();
    }
  in

  let edges_tbl = Hashtbl.create 16 in
  Hashtbl.replace edges_tbl "root" ["a"; "b"];
  Hashtbl.replace edges_tbl "a" ["c"];
  let edges_subs = ref [] in
  let edges : (string, string list) Reactive.t =
    {
      subscribe = (fun h -> edges_subs := h :: !edges_subs);
      iter = (fun f -> Hashtbl.iter f edges_tbl);
      get = (fun k -> Hashtbl.find_opt edges_tbl k);
      length = (fun () -> Hashtbl.length edges_tbl);
      stats = Reactive.create_stats ();
    }
  in

  (* Create fixpoint - should immediately have all reachable *)
  let fp = Reactive.fixpoint ~init ~edges () in

  Printf.printf "Fixpoint length: %d (expected 4)\n" (Reactive.length fp);
  assert (Reactive.length fp = 4);
  (* root, a, b, c *)
  assert (Reactive.get fp "root" = Some ());
  assert (Reactive.get fp "a" = Some ());
  assert (Reactive.get fp "b" = Some ());
  assert (Reactive.get fp "c" = Some ());

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
  test_union_basic ();
  test_union_with_merge ();
  test_union_existing_data ();
  test_fixpoint ();
  (* Incremental fixpoint tests *)
  test_fixpoint_basic_expansion ();
  test_fixpoint_multiple_roots ();
  test_fixpoint_diamond ();
  test_fixpoint_cycle ();
  test_fixpoint_add_base ();
  test_fixpoint_remove_base ();
  test_fixpoint_add_edge ();
  test_fixpoint_remove_edge ();
  test_fixpoint_cycle_removal ();
  test_fixpoint_alternative_support ();
  test_fixpoint_empty_base ();
  test_fixpoint_self_loop ();
  test_fixpoint_deltas ();
  test_fixpoint_existing_data ();
  test_fixpoint_remove_spurious_root ();
  test_fixpoint_remove_edge_entry_alternative_source ();
  test_fixpoint_remove_edge_rederivation ();
  test_fixpoint_remove_edge_entry_rederivation ();
  test_fixpoint_remove_edge_entry_higher_rank_support ();
  test_fixpoint_remove_edge_entry_needs_rederivation ();
  test_fixpoint_remove_base_needs_rederivation ();
  Printf.printf "All tests passed!\n"
