type command =
  | Build of build_options
  | Clean of string
  | Watch of build_options
  | Format of {check: bool; stdin: string option; files: string list}
  | Help | Version

and build_options = {folder: string; prod: bool; features: string list option; warn_error: string option}

exception Error of string

let usage = "Usage: rescript-ocaml [build|watch|clean] [OPTIONS] [FOLDER]"

let parse argv =
  let args = Array.to_list argv |> List.tl in
  let parse_build ~watch args =
    let rec loop folder prod features warn_error = function
    | [] ->
      let command = {folder = Option.value folder ~default:"."; prod; features; warn_error} in
      if watch then Watch command else Build command
    | ("-h" | "--help") :: _ -> Help
    | ("-V" | "--version") :: _ -> Version
    | "--prod" :: rest -> loop folder true features warn_error rest
    | "--features" :: value :: rest ->
      let values = String.split_on_char ',' value |> List.filter (fun x -> x <> "") in
      if values = [] then raise (Error "--features requires a non-empty value");
      loop folder prod (Some values) warn_error rest
    | arg :: rest when String.starts_with ~prefix:"--features=" arg ->
      let value = String.sub arg 11 (String.length arg - 11) in
      let values = String.split_on_char ',' value |> List.filter (fun x -> x <> "") in
      if values = [] then raise (Error "--features requires a non-empty value");
      loop folder prod (Some values) warn_error rest
    | "--warn-error" :: value :: rest -> loop folder prod features (Some value) rest
    | ("-v" | "-vv" | "-q" | "-qq" | "--no-timing") :: rest ->
      loop folder prod features warn_error rest
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
      raise (Error ("unknown option " ^ arg))
    | arg :: rest -> (
      match folder with
      | None -> loop (Some arg) prod features warn_error rest
      | Some _ -> raise (Error "too many folder arguments"))
    in loop None false None None args
  in
  match args with
  | "format" :: rest ->
    let rec loop check stdin files = function
      | [] -> Format {check; stdin; files = List.rev files}
      | ("-c" | "--check") :: more -> loop true stdin files more
      | ("-s" | "--stdin") :: extension :: more ->
        if check then raise (Error "--stdin conflicts with --check");
        loop check (Some extension) files more
      | arg :: _ when String.length arg > 0 && arg.[0] = '-' -> raise (Error ("unknown format option " ^ arg))
      | file :: more ->
        if Option.is_some stdin then raise (Error "files conflict with --stdin");
        loop check stdin (file :: files) more
    in loop false None [] rest
  | "clean" :: rest ->
    (match rest with [] -> Clean "." | [folder] -> Clean folder | _ -> raise (Error "too many folder arguments"))
  | "watch" :: rest -> parse_build ~watch:true rest
  | "build" :: rest -> parse_build ~watch:false rest
  | rest -> parse_build ~watch:false rest
