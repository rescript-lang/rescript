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

let%expect_test "source directory metadata rejects incomplete scan entries" =
  List.iter
    (fun text ->
      match get_build_roots_from_json (Yojson.Safe.from_string text) with
      | None -> print_endline "invalid"
      | Some roots ->
        `List (List.map (fun root -> `String root) roots)
        |> Yojson.Safe.to_string |> print_endline)
    [
      {|{}|};
      {|{"cmt_scan":null}|};
      {|{"cmt_scan":[]}|};
      {|{"cmt_scan":[{"build_root":"lib/rescript"},{}]}|};
      {|{"cmt_scan":[{"build_root":null}]}|};
      {|{"version":2,"cmt_scan":[{"build_root":"lib/rescript","scan_dirs":[]}]}|};
    ];
  [%expect
    {|
    invalid
    invalid
    []
    invalid
    invalid
    ["lib/rescript"]
    |}]
