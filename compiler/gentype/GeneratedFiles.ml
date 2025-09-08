type file_action =
  | NoMatch (* No @genType annotation found. *)
  | Replace (* Replace existing file on disk with new contents. *)
  | Identical (* File already on disk with identical contents. Skip. *)
  | TypeError
    (* The cmt file was produced after a type error -- don't delete generated files. *)
  | Write (* File not present on disk. *)

let log_file_action file_action file_name =
  if !Debug.basic then
    Log_.item "%s  %s\n"
      (match file_action with
      | NoMatch -> "NoMatch"
      | Replace -> "Replace"
      | Identical -> "Identical"
      | TypeError -> "TypeError"
      | Write -> "Write")
      file_name

let read_lines (file : string) : string list =
  let lines = ref [] in
  let chan = open_in file in
  let finished_lines =
    try
      while true do
        lines := input_line chan :: !lines
      done;
      []
    with End_of_file ->
      close_in chan [@doesNotRaise];
      !lines |> List.rev
  in
  finished_lines

let read_file (file : string) : string = String.concat "\n" (read_lines file)

let write_file (file_path : string) (contents : string) =
  let out_file = open_out file_path in
  output_string out_file contents;
  close_out out_file [@doesNotRaise]

let write_file_if_required ~output_file ~file_contents =
  if Sys.file_exists output_file then
    let old_contents = read_file output_file in
    let identical = old_contents = file_contents in
    if identical then output_file |> log_file_action Identical
    else (
      output_file |> log_file_action Replace;
      write_file output_file file_contents)
  else (
    output_file |> log_file_action Write;
    write_file output_file file_contents)
