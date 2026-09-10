let path root directory =
  Filename.concat (Build_artifacts.lib_path root directory) ".compiler.log"

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
  File_util.ensure_dir (Filename.dirname path);
  File_util.write_file path
    (Printf.sprintf "#Start(%.6f)\n" (Unix.gettimeofday ()))

let append root content =
  File_util.append_file (path root "bs") (strip_ansi content)

let finalize root =
  append root (Printf.sprintf "#Done(%.6f)\n" (Unix.gettimeofday ()));
  File_util.copy_existing_file ~ensure_parent:false (path root "bs") (path root "ocaml")
