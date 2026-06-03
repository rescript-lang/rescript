let split str string = Str.split (Str.regexp_string str) string

let remove_extra_dots path =
  Str.global_replace (Str.regexp_string "/./") "/" path
  |> Str.global_replace (Str.regexp {|^\./\.\./|}) "../"

(* Win32 & MacOS are case-insensitive *)
let path_eq =
  if Sys.os_type = "Linux" then fun a b -> a = b
  else fun a b -> String.lowercase_ascii a = String.lowercase_ascii b

let path_starts_with text prefix =
  String.length prefix <= String.length text
  && path_eq (String.sub text 0 (String.length prefix)) prefix

let slice_to_end str pos = String.sub str pos (String.length str - pos)

let relpath base path =
  if path_starts_with path base then
    let baselen = String.length base in
    let rest = String.sub path baselen (String.length path - baselen) in
    (if rest <> "" && rest.[0] = Filename.dir_sep.[0] then slice_to_end rest 1
     else rest)
    |> remove_extra_dots
  else
    let rec loop bp pp =
      match (bp, pp) with
      | "." :: ra, _ -> loop ra pp
      | _, "." :: rb -> loop bp rb
      | a :: ra, b :: rb when path_eq a b -> loop ra rb
      | _ -> (bp, pp)
    in
    let base, path =
      loop (split Filename.dir_sep base) (split Filename.dir_sep path)
    in
    String.concat Filename.dir_sep
      ((match base with
       | [] -> ["."]
       | _ -> List.map (fun _ -> "..") base)
      @ path)
    |> remove_extra_dots

let maybe_stat path =
  try Some (Unix.stat path) with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let read_file filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let exists path =
  match maybe_stat path with
  | None -> false
  | Some _ -> true
let if_exists path = if exists path then Some path else None

let read_directory dir =
  match Unix.opendir dir with
  | exception Unix.Unix_error (Unix.ENOENT, "opendir", _dir) -> []
  | handle ->
    let rec loop handle =
      try
        let name = Unix.readdir handle in
        if name = Filename.current_dir_name || name = Filename.parent_dir_name
        then loop handle
        else name :: loop handle
      with End_of_file ->
        Unix.closedir handle;
        []
    in
    loop handle

let rec collect_dirs path =
  match maybe_stat path with
  | None -> []
  | Some {Unix.st_kind = Unix.S_DIR} ->
    path
    :: (read_directory path
       |> List.map (fun name -> collect_dirs (Filename.concat path name))
       |> List.concat)
  | _ -> []

let rec collect ?(check_dir = fun _ -> true) ?max_depth path test =
  match (max_depth, maybe_stat path) with
  | None, None -> []
  | Some 0, _ -> []
  | None, Some {Unix.st_kind = Unix.S_DIR} ->
    if check_dir path then
      read_directory path
      |> List.map (fun name ->
             collect ~check_dir (Filename.concat path name) test)
      |> List.concat
    else []
  | Some n, Some {Unix.st_kind = Unix.S_DIR} ->
    if check_dir path then
      read_directory path
      |> List.map (fun name ->
             collect ~check_dir ~max_depth:(n - 1)
               (Filename.concat path name)
               test)
      |> List.concat
    else []
  | _ -> if test path then [path] else []

type classified_file = Res | Resi | Other

let classify_source_file path =
  if Filename.check_suffix path ".res" && exists path then Res
  else if Filename.check_suffix path ".resi" && exists path then Resi
  else Other

let canonicalize_uri uri =
  let path = Uri.to_path uri in
  path |> Unix.realpath |> Uri.from_path |> Uri.to_string
