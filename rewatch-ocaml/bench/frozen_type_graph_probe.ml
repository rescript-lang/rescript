(* Diagnostic microprobe for the type-graph slice of an imported CMI. It does
   not model signature records or Env component construction. *)

let roots_of_signature signature =
  let roots = ref [] in
  let original = Btype.type_iterators in
  let iterator =
    {original with it_type_expr = (fun _ ty -> roots := ty :: !roots)}
  in
  iterator.it_signature iterator signature;
  List.rev !roots

let measure ~iterations name action =
  Gc.full_major ();
  let started = Unix.gettimeofday () in
  let before = Gc.allocated_bytes () in
  let consumed = ref 0 in
  for _ = 1 to iterations do
    consumed := !consumed + action ()
  done;
  let seconds = Unix.gettimeofday () -. started in
  let bytes = Gc.allocated_bytes () -. before in
  Printf.printf "%s\t%d\t%.3f\t%.1f\t%d\n" name iterations (seconds *. 1000.)
    (bytes /. 1e6) !consumed

let () =
  if Array.length Sys.argv <> 3 then (
    prerr_endline "Usage: frozen_type_graph_probe CMI ITERATIONS";
    exit 2);
  let filename = Sys.argv.(1) in
  let iterations = int_of_string Sys.argv.(2) in
  if iterations <= 0 then invalid_arg "iterations must be positive";
  let cmi = Cmi_format.read_cmi filename in
  let roots = roots_of_signature cmi.cmi_sign in
  let image =
    match Frozen_type_graph.freeze roots with
    | Ok image -> image
    | Error reason -> failwith reason
  in
  let bytes = Marshal.to_bytes cmi [] in
  Printf.printf "cmi_bytes=%d type_roots=%d type_nodes=%d\n"
    (Bytes.length bytes) (List.length roots)
    (Frozen_type_graph.node_count image);
  Printf.printf "operation\titerations\tworker_ms\tallocated_MB\tchecksum\n";
  measure ~iterations "freeze_type_graph" (fun () ->
      match Frozen_type_graph.freeze roots with
      | Ok image -> Frozen_type_graph.node_count image
      | Error reason -> failwith reason);
  measure ~iterations "thaw_type_graph" (fun () ->
      List.length (Frozen_type_graph.thaw image));
  measure ~iterations "thaw_first_root" (fun () ->
      (Frozen_type_graph.thaw_root image 0).Types.id);
  measure ~iterations "marshal_cmi_decode" (fun () ->
      let copy : Cmi_format.cmi_infos = Marshal.from_bytes bytes 0 in
      List.length copy.cmi_sign);
  measure ~iterations "subst_signature" (fun () ->
      List.length (Subst.signature Subst.identity cmi.cmi_sign))
