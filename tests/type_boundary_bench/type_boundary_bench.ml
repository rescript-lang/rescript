(* The cost of the boxed boundary of the type store: for .cmi and .cmt files
   given on the command line (or listed, one per line, in a file named with
   [-files]), the time to unmarshal the boxed graph, import it into a store,
   export it back and marshal it, each summed over the files. For .cmi
   files it also checks that the round trip reproduces the file's bytes.
   [-repeat n] runs every file n times (the minimum per file is kept). *)

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. t0)

let read_file path = In_channel.with_open_bin path In_channel.input_all

type totals = {
  mutable files: int;
  mutable bytes: int;
  mutable unmarshal: float;
  mutable import: float;
  mutable export: float;
  mutable marshal: float;
  mutable nodes: int;
  mutable mismatches: int;
}

let new_totals () =
  {
    files = 0;
    bytes = 0;
    unmarshal = 0.;
    import = 0.;
    export = 0.;
    marshal = 0.;
    nodes = 0;
    mismatches = 0;
  }

let cmi = new_totals ()
let cmt = new_totals ()
let repeat = ref 1

let best n f =
  let r, t = time f in
  let t = ref t in
  for _ = 2 to n do
    let _, t' = time f in
    if t' < !t then t := t'
  done;
  (r, !t)

let run_cmi path =
  let contents = read_file path in
  let start = String.length Config.cmi_magic_number in
  let size = Marshal.total_size (Bytes.unsafe_of_string contents) start in
  let (name : string), (sign : Types_boxed.signature) =
    Marshal.from_string contents start
  in
  let _, tu =
    best !repeat (fun () ->
        ignore
          (Marshal.from_string contents start : string * Types_boxed.signature))
  in
  Types.Store.with_store (fun () ->
      let (imported, nodes), ti =
        best !repeat (fun () ->
            Type_arena.Import.run (fun c ->
                let sg = Type_arena.Import.signature c sign in
                (sg, Type_arena.Import.nodes c)))
      in
      let exported, te =
        best !repeat (fun () ->
            Type_arena.Export.run (fun c ->
                Type_arena.Export.signature c imported))
      in
      let bytes, tm =
        best !repeat (fun () -> Marshal.to_string (name, exported) [])
      in
      if not (String.equal bytes (String.sub contents start size)) then (
        Printf.printf "MISMATCH %s\n%!" path;
        cmi.mismatches <- cmi.mismatches + 1);
      cmi.files <- cmi.files + 1;
      cmi.bytes <- cmi.bytes + String.length contents;
      cmi.unmarshal <- cmi.unmarshal +. tu;
      cmi.import <- cmi.import +. ti;
      cmi.export <- cmi.export +. te;
      cmi.marshal <- cmi.marshal +. tm;
      cmi.nodes <- cmi.nodes + nodes)

let run_cmt path =
  let contents = read_file path in
  let start = String.length Config.cmt_magic_number in
  let boxed : Cmt_format.Boxed.cmt_infos = Marshal.from_string contents start in
  let _, tu =
    best !repeat (fun () ->
        ignore (Marshal.from_string contents start : Cmt_format.Boxed.cmt_infos))
  in
  Types.Store.with_store (fun () ->
      let imported, ti = best !repeat (fun () -> Cmt_format.import_cmt boxed) in
      let nodes = Types.Store.length (Types.Store.current ()) in
      let exported, te =
        best !repeat (fun () -> Cmt_format.export_cmt imported)
      in
      let _, tm =
        best !repeat (fun () ->
            Marshal.to_string (exported : Cmt_format.Boxed.cmt_infos) [])
      in
      cmt.files <- cmt.files + 1;
      cmt.bytes <- cmt.bytes + String.length contents;
      cmt.unmarshal <- cmt.unmarshal +. tu;
      cmt.import <- cmt.import +. ti;
      cmt.export <- cmt.export +. te;
      cmt.marshal <- cmt.marshal +. tm;
      cmt.nodes <- cmt.nodes + (nodes / !repeat))

let report kind t =
  Printf.printf
    "%s: %d files, %.1f MB; unmarshal %.3f s, import %.3f s (%.2fx unmarshal), \
     export %.3f s, marshal %.3f s (export %.2fx marshal); %d nodes; %d byte \
     mismatches\n"
    kind t.files
    (float t.bytes /. 1e6)
    t.unmarshal t.import (t.import /. t.unmarshal) t.export t.marshal
    (t.export /. t.marshal) t.nodes t.mismatches

let () =
  let files = ref [] in
  let rec args = function
    | "-repeat" :: n :: rest ->
      repeat := int_of_string n;
      args rest
    | "-files" :: list :: rest ->
      files :=
        List.rev_append
          (String.split_on_char '\n' (read_file list)
          |> List.filter (fun s -> s <> ""))
          !files;
      args rest
    | f :: rest ->
      files := f :: !files;
      args rest
    | [] -> ()
  in
  args (List.tl (Array.to_list Sys.argv));
  List.iter
    (fun f ->
      if Filename.check_suffix f ".cmi" then run_cmi f
      else if Filename.check_suffix f ".cmt" || Filename.check_suffix f ".cmti"
      then run_cmt f)
    (List.rev !files);
  report "cmi" cmi;
  report "cmt" cmt
