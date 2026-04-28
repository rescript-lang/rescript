let split str string = Str.split (Str.regexp_string str) string

let removeExtraDots path =
  Str.global_replace (Str.regexp_string "/./") "/" path
  |> Str.global_replace (Str.regexp {|^\./\.\./|}) "../"

(* Win32 & MacOS are case-insensitive *)
let pathEq =
  if Sys.os_type = "Linux" then fun a b -> a = b
  else fun a b -> String.lowercase_ascii a = String.lowercase_ascii b

let pathStartsWith text prefix =
  String.length prefix <= String.length text
  && pathEq (String.sub text 0 (String.length prefix)) prefix

let sliceToEnd str pos = String.sub str pos (String.length str - pos)

let relpath base path =
  if pathStartsWith path base then
    let baselen = String.length base in
    let rest = String.sub path baselen (String.length path - baselen) in
    (if rest <> "" && rest.[0] = Filename.dir_sep.[0] then sliceToEnd rest 1
     else rest)
    |> removeExtraDots
  else
    let rec loop bp pp =
      match (bp, pp) with
      | "." :: ra, _ -> loop ra pp
      | _, "." :: rb -> loop bp rb
      | a :: ra, b :: rb when pathEq a b -> loop ra rb
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
    |> removeExtraDots

let maybeStat path =
  try Some (Unix.stat path) with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let readFile filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let exists path =
  match maybeStat path with
  | None -> false
  | Some _ -> true
let ifExists path = if exists path then Some path else None

let readDirectory dir =
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

let rec collectDirs path =
  match maybeStat path with
  | None -> []
  | Some {Unix.st_kind = Unix.S_DIR} ->
    path
    :: (readDirectory path
       |> List.map (fun name -> collectDirs (Filename.concat path name))
       |> List.concat)
  | _ -> []

let rec collect ?(checkDir = fun _ -> true) ?maxDepth path test =
  match (maxDepth, maybeStat path) with
  | None, None -> []
  | Some 0, _ -> []
  | None, Some {Unix.st_kind = Unix.S_DIR} ->
    if checkDir path then
      readDirectory path
      |> List.map (fun name ->
             collect ~checkDir (Filename.concat path name) test)
      |> List.concat
    else []
  | Some n, Some {Unix.st_kind = Unix.S_DIR} ->
    if checkDir path then
      readDirectory path
      |> List.map (fun name ->
             collect ~checkDir ~maxDepth:(n - 1)
               (Filename.concat path name)
               test)
      |> List.concat
    else []
  | _ -> if test path then [path] else []

type classifiedFile = Res | Resi | Other

let classifySourceFile path =
  if Filename.check_suffix path ".res" && exists path then Res
  else if Filename.check_suffix path ".resi" && exists path then Resi
  else Other

let canonicalizeUri uri =
  let path = Uri.toPath uri in
  path |> Unix.realpath |> Uri.fromPath |> Uri.toString
