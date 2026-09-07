type command =
  | Build of build_options
  | Clean of string
  | Watch of build_options
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
  | "clean" :: rest ->
    (match rest with [] -> Clean "." | [folder] -> Clean folder | _ -> raise (Error "too many folder arguments"))
  | "watch" :: rest -> parse_build ~watch:true rest
  | "build" :: rest -> parse_build ~watch:false rest
  | rest -> parse_build ~watch:false rest
