type cmt_scan_item = {build_root: string} [@@deriving yojson {strict = false}]

type source_dirs = {cmt_scan: cmt_scan_item list}
[@@deriving yojson {strict = false}]

let get_build_roots_from_json json =
  match source_dirs_of_yojson json with
  | Ok {cmt_scan} -> Some (List.map (fun {build_root} -> build_root) cmt_scan)
  | Error _ -> None

let get_build_roots_from_file ~fs path =
  match Fs.load ~fs path with
  | Some content -> (
    match Yojson.Safe.from_string content with
    | json -> get_build_roots_from_json json
    | exception _ -> None)
  | None -> None

let%expect_test "get_build_roots" =
  let print_build_roots result =
    match result with
    | None -> ()
    | Some l -> List.iter print_endline l
  in
  let json_1 =
    Yojson.Safe.from_string
      {|
{
  "cmt_scan": [
    {
      "build_root": "path/to/lib/bs"
    }
  ]
}
    |}
  in

  json_1 |> get_build_roots_from_json |> print_build_roots;
  [%expect {| path/to/lib/bs |}];

  let json_2 =
    Yojson.Safe.from_string
      {|
{
"cmt_scan": [
  {
    "build_root": "path/to/lib/bs"
  },
  {
    "build_root": "path2/to/lib/bs"
  }
]
}
  |}
  in

  json_2 |> get_build_roots_from_json |> print_build_roots;
  [%expect {|
    path/to/lib/bs
    path2/to/lib/bs
    |}];

  let json_3 =
    Yojson.Safe.from_string
      {|
{
"cmt_scan": [
{
  "build_root": []
},
{
  "build_root": {}
}
]
}
|}
  in

  json_3 |> get_build_roots_from_json |> print_build_roots;
  [%expect {| |}]
