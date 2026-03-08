(** Tests for off-heap ReactiveTable storage. *)

let test_table_promoted_wave_lifecycle () =
  Printf.printf "=== Test: table promoted-wave lifecycle ===\n";
  let iterations = 8 in
  let count = 128 in
  let width = 48 in
  let initial_live_blocks = Allocator.live_block_count () in
  let initial_live_block_slots = Allocator.live_block_capacity_slots () in
  Gc.full_major ();
  ignore (AllocMeasure.words_since ());
  let t = ReactiveTable.create ~initial_capacity:1 in
  let create_words = AllocMeasure.words_since () in
  assert (create_words = 0);
  for iter = 1 to iterations do
    ignore (AllocMeasure.words_since ());
    let fresh =
      Array.init count (fun i ->
          let c = Char.chr (((iter + i) mod 26) + Char.code 'a') in
          let bytes = Bytes.make width c in
          Bytes.set bytes 0 c;
          Bytes.set bytes (width - 1) c;
          bytes)
    in
    let produced_words = AllocMeasure.words_since () in
    assert (produced_words > 0);

    for i = 0 to count - 1 do
      assert (Allocator.is_in_minor_heap fresh.(i))
    done;

    Gc.full_major ();

    for i = 0 to count - 1 do
      assert (not (Allocator.is_in_minor_heap fresh.(i)))
    done;

    ignore (AllocMeasure.words_since ());
    ReactiveTable.clear t;
    for i = 0 to count - 1 do
      ReactiveTable.push t (Offheap.of_value fresh.(i))
    done;
    assert (ReactiveTable.length t = count);
    assert (ReactiveTable.capacity t >= ReactiveTable.length t);
    ReactiveTable.set t 0 (Offheap.of_value fresh.(count - 1));
    assert (Offheap.unsafe_to_value (ReactiveTable.get t 0) == fresh.(count - 1));
    for i = 0 to count - 1 do
      let expected = if i = 0 then fresh.(count - 1) else fresh.(i) in
      let recovered = Offheap.unsafe_to_value (ReactiveTable.get t i) in
      assert (recovered == expected);
      assert (Bytes.get recovered 0 = Bytes.get expected 0);
      assert (Bytes.get recovered (width - 1) = Bytes.get expected (width - 1))
    done;
    assert (Offheap.unsafe_to_value (ReactiveTable.pop t) == fresh.(count - 1));
    assert (ReactiveTable.length t = count - 1);
    ReactiveTable.shrink_to_fit t;
    assert (ReactiveTable.capacity t = ReactiveTable.length t);
    ReactiveTable.clear t;
    assert (ReactiveTable.length t = 0);
    let table_words = AllocMeasure.words_since () in
    Printf.printf "  iter=%d produced=%d table_phase=%d\n" iter produced_words
      table_words;
    assert (table_words = 0);

    Gc.full_major ()
  done;
  ignore (AllocMeasure.words_since ());
  ReactiveTable.destroy t;
  let teardown_words = AllocMeasure.words_since () in
  assert (teardown_words = 0);
  assert (Allocator.live_block_count () = initial_live_blocks);
  assert (Allocator.live_block_capacity_slots () = initial_live_block_slots);
  Printf.printf "  create=%d teardown=%d\n" create_words teardown_words;
  Printf.printf "PASSED\n\n"

let test_table_unsafe_minor_heap_demo () =
  Printf.printf "=== Test: table unsafe minor-heap demo ===\n";
  match Sys.getenv_opt "RESCRIPT_REACTIVE_RUN_UNSAFE_TABLE_DEMO" with
  | None ->
    Printf.printf
      "SKIPPED (set RESCRIPT_REACTIVE_RUN_UNSAFE_TABLE_DEMO=1 to run)\n\n"
  | Some _ ->
    let count = 2048 in
    let width = 64 in
    let t = ReactiveTable.create ~initial_capacity:count in
    (* Each [Bytes.make] result starts in the minor heap. We store only the raw
       addresses off-heap and intentionally drop all OCaml roots. *)
    for i = 0 to count - 1 do
      let c = Char.chr ((i mod 26) + Char.code 'A') in
      let fresh = Bytes.make width c in
      Bytes.set fresh 0 c;
      Bytes.set fresh (width - 1) c;
      ReactiveTable.push t (Offheap.unsafe_of_value fresh)
    done;
    Gc.compact ();
    for round = 1 to 200 do
      for j = 0 to 200 do
        ignore (Bytes.make (1024 + ((round + j) mod 2048)) 'z')
      done;
      Gc.full_major ();
      Gc.compact ()
    done;
    Printf.printf
      "About to validate %d minor-heap values stored off-heap. This is unsafe \
       and may return garbage or crash.\n"
      count;
    let mismatches = ref 0 in
    let samples = ref [] in
    for i = 0 to count - 1 do
      let expected = Char.chr ((i mod 26) + Char.code 'A') in
      let recovered : bytes = Offheap.unsafe_to_value (ReactiveTable.get t i) in
      let ok =
        Bytes.length recovered = width
        && Bytes.get recovered 0 = expected
        && Bytes.get recovered (width - 1) = expected
      in
      if not ok then (
        incr mismatches;
        if List.length !samples < 8 then
          let observed_len = try Bytes.length recovered with _ -> -1 in
          let observed_first = try Bytes.get recovered 0 with _ -> '?' in
          samples :=
            Printf.sprintf "slot=%d expected=%c len=%d first=%c" i expected
              observed_len observed_first
            :: !samples)
    done;
    Printf.printf "Observed mismatches: %d/%d\n" !mismatches count;
    List.iter (fun s -> Printf.printf "%s\n" s) (List.rev !samples);
    ReactiveTable.destroy t;
    Printf.printf
      "UNSAFE DEMO COMPLETED (result is not trustworthy; crash/corruption \
       would also be expected)\n\n"

let run_all () =
  test_table_promoted_wave_lifecycle ();
  test_table_unsafe_minor_heap_demo ()
