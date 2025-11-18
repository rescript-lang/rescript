external print_persistent_size : unit -> unit = "SKIP_print_persistent_size"

let () =
  Reactive.init "/tmp/foo" (1024 * 1024 * 1024);
  let inputs = Reactive.input_files [|"/tmp/xx"; "/tmp/yy"|] in
  let mapped2 = 
    Reactive.map inputs begin fun name values ->
      let _values2 = Reactive.get_array inputs "/tmp/yy" in
      let res = ref [] in
      for i = 1 to 10000 do
        res := [| name ^ string_of_int i, [|string_of_int i|] |] :: !res;
      done;
      Array.concat !res
      end
  in
  let cur = ref mapped2 in
  for i = 1 to 3 do
    let t = Unix.gettimeofday() in
    cur := Reactive.map !cur begin fun key values ->
      [| key, Array.concat [Reactive.get_array !cur key] |]
      end;
    Printf.printf "%f\n" (Unix.gettimeofday() -. t); flush stdout;
  done;
  Reactive.exit();
  print_persistent_size();
  ()
