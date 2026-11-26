let get_build_roots_from_json json =
  let open Yojson.Safe.Util in
  try
    json |> member "cmt_scan" |> to_list
    |> List.map (fun item -> item |> member "build_root" |> to_string)
    |> Option.some
  with Type_error _ -> None

let get_build_roots_from_file ~fs path =
  match Fs.load ~fs path with
  | Some content -> (
    match Yojson.Safe.from_string content with
    | json -> get_build_roots_from_json json
    | exception _ -> None)
  | None -> None
