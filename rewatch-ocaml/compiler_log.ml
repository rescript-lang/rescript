open Build_artifacts
open File_util

let path root directory = Filename.concat (lib_path root directory) ".compiler.log"

let strip_ansi content =
  let length = String.length content in
  let output = Buffer.create length in
  let rec skip_csi index =
    if index >= length then index
    else
      let code = Char.code content.[index] in
      if code >= 0x40 && code <= 0x7e then index + 1
      else skip_csi (index + 1)
  in
  let rec loop index =
    if index < length then
      if
        (content.[index] = '\027' || content.[index] = '\155')
        && index + 1 < length && content.[index + 1] = '['
      then loop (skip_csi (index + 2))
      else (
        Buffer.add_char output content.[index];
        loop (index + 1))
  in
  loop 0;
  Buffer.contents output

let initialize root =
  let path = path root "bs" in
  ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    Printf.fprintf channel "#Start(%.6f)\n" (Unix.gettimeofday ()))

let append root content =
  let channel =
    open_out_gen [Open_wronly; Open_append; Open_binary] 0o644 (path root "bs")
  in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel (strip_ansi content))

let finalize root =
  append root (Printf.sprintf "#Done(%.6f)\n" (Unix.gettimeofday ()));
  copy_existing_file ~ensure_parent:false (path root "bs") (path root "ocaml")
