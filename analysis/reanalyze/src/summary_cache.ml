type error =
  | Io_error of string
  | Invalid_format of string
  | Digest_mismatch of {expected: string; actual: string}
  | Not_found

type result =
  | Hit
  | Refreshed

let normalize_root project_root =
  if String.equal project_root "" then Sys.getcwd () else project_root

let base_dir ~project_root = Filename.concat (normalize_root project_root) ".reanalyze"
let cache_dir ~project_root = Filename.concat (base_dir ~project_root) "summaries"

let path_for ~project_root ~digest =
  Filename.concat (cache_dir ~project_root) (digest ^ ".json")

let rec ensure_dir path =
  if path = "" || path = Filename.dir_sep then ()
  else if Sys.file_exists path then ()
  else (
    ensure_dir (Filename.dirname path);
    Unix.mkdir path 0o755)

let write_file path contents =
  let tmp =
    Filename.temp_file ~temp_dir:(Filename.dirname path) "summary" ".tmp"
  in
  let oc = open_out_bin tmp in
  let cleanup () =
    close_out_noerr oc;
    (try Sys.remove tmp with
    | Sys_error _ -> ())
  in
  try
    output_string oc contents;
    output_char oc '\n';
    close_out oc;
    Unix.rename tmp path
  with exn ->
    cleanup ();
    raise exn

let write ~project_root summary =
  let dir = cache_dir ~project_root in
  (try ensure_dir dir with
  | Sys_error msg -> raise (Sys_error msg));
  let path = path_for ~project_root ~digest:summary.Summary.digest in
  try
    write_file path (Summary.to_string summary);
    Ok ()
  with
  | Summary.Invalid_format msg -> Error (Invalid_format msg)
  | Sys_error msg -> Error (Io_error msg)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let read ~project_root ~digest =
  let path = path_for ~project_root ~digest in
  if not (Sys.file_exists path) then Error Not_found
  else
    try
      let content = read_file path in
      let summary = Summary.of_string content in
      if String.equal summary.Summary.digest digest then Ok summary
      else Error (Digest_mismatch {expected = digest; actual = summary.digest})
    with
    | Summary.Invalid_format msg -> Error (Invalid_format msg)
    | Sys_error msg -> Error (Io_error msg)

let read_or_recompute ~project_root summary =
  match read ~project_root ~digest:summary.Summary.digest with
  | Ok _ -> Ok Hit
  | Error _ ->
      (match write ~project_root summary with
      | Ok () -> Ok Refreshed
      | Error _ as err -> err)

let error_message = function
  | Io_error msg -> msg
  | Invalid_format msg -> msg
  | Digest_mismatch {expected; actual} ->
      Printf.sprintf "digest mismatch (expected %s, found %s)" expected actual
  | Not_found -> "summary not found"

