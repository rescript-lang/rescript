type load_error = string

let read_cmt path =
  try Ok (Cmt_format.read_cmt path)
  with exn -> Error (Printexc.to_string exn)

let implementation path =
  match read_cmt path with
  | Error _ as e -> e
  | Ok infos -> (
    match infos.cmt_annots with
    | Implementation structure -> Ok (structure, infos)
    | Interface _ ->
      Error
        (Printf.sprintf "Expected implementation but %s is an interface" path)
    | Packed _ ->
      Error
        (Printf.sprintf "Packed modules are not supported for %s" path)
    | Partial_implementation _ | Partial_interface _ ->
      Error
        (Printf.sprintf
           "Partial compilation artefacts are not supported for %s" path))

let interface path =
  match read_cmt path with
  | Error _ as e -> e
  | Ok infos -> (
    match infos.cmt_annots with
    | Interface signature -> Ok (signature, infos)
    | Implementation _ ->
      Error (Printf.sprintf "Expected interface but %s is an implementation" path)
    | Packed _ ->
      Error
        (Printf.sprintf "Packed modules are not supported for %s" path)
    | Partial_implementation _ | Partial_interface _ ->
      Error
        (Printf.sprintf
           "Partial compilation artefacts are not supported for %s" path))

