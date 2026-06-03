let run_config = Run_config.run_config

let normalize_separators s =
  if Sys.win32 then String.map (fun c -> if c = '\\' then '/' else c) s else s

let split_on_slash s =
  let rec aux acc start i =
    if i >= String.length s then
      let last = String.sub s start (i - start) in
      if last = "" then acc else last :: acc
    else if s.[i] = '/' then
      let seg = String.sub s start (i - start) in
      let acc = if seg = "" then acc else seg :: acc in
      aux acc (i + 1) (i + 1)
    else aux acc start (i + 1)
  in
  List.rev (aux [] 0 0)

let has_glob_char s = String.contains s '*'

(** Match glob pattern segments against path segments.
    A single star matches one path segment, a double star matches zero or more.
    Matches as a prefix: if the pattern is exhausted, the remaining path is accepted. *)
let rec glob_match pattern_segs path_segs =
  match (pattern_segs, path_segs) with
  | [], _ -> true
  | "**" :: rest, _ -> (
    glob_match rest path_segs
    ||
    match path_segs with
    | _ :: path_rest -> glob_match pattern_segs path_rest
    | [] -> false)
  | _ :: _, [] -> false
  | pat :: pat_rest, seg :: path_rest ->
    glob_segment pat seg && glob_match pat_rest path_rest

and glob_segment pattern segment =
  let rec aux pi si =
    if pi >= String.length pattern then si >= String.length segment
    else if pattern.[pi] = '*' then
      let rec try_skip si' =
        si' <= String.length segment && (aux (pi + 1) si' || try_skip (si' + 1))
      in
      try_skip si
    else
      si < String.length segment
      && pattern.[pi] = segment.[si]
      && aux (pi + 1) (si + 1)
  in
  aux 0 0

let check_pattern pattern_ =
  let is_glob = has_glob_char pattern_ in
  let pattern =
    match run_config.project_root = "" with
    | true -> pattern_
    | false -> Filename.concat run_config.project_root pattern_
  in
  let pattern = normalize_separators pattern in
  if is_glob then
    let pattern_segs = split_on_slash pattern in
    fun source_dir ->
      let path_segs = split_on_slash (normalize_separators source_dir) in
      glob_match pattern_segs path_segs
  else
    let prefix_len = pattern |> String.length in
    fun source_dir ->
      let source_dir = normalize_separators source_dir in
      try String.sub source_dir 0 prefix_len = pattern
      with Invalid_argument _ -> false

let suppress_source_dir =
  lazy
    (fun source_dir ->
      run_config.suppress
      |> List.exists (fun pattern -> check_pattern pattern source_dir))

let unsuppress_source_dir =
  lazy
    (fun source_dir ->
      run_config.unsuppress
      |> List.exists (fun pattern -> check_pattern pattern source_dir))

let pos_in_suppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force suppress_source_dir

let pos_in_unsuppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force unsuppress_source_dir

(** First suppress list, then override with unsuppress list *)
let filter pos = (not (pos_in_suppress pos)) || pos_in_unsuppress pos
