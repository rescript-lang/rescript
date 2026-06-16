let get_build_roots_from_json json =
  let build_roots =
    match json with
    | `Assoc fields -> (
      match List.assoc_opt "cmt_scan" fields with
      | Some (`List cmt_scan_items) ->
        let build_roots =
          List.filter_map
            (fun (cmt_scan_item : Yojson.Safe.t) ->
              match cmt_scan_item with
              | `Assoc cmt_scan_fields -> (
                match List.assoc_opt "build_root" cmt_scan_fields with
                | Some (`String build_root) -> Some build_root
                | _ -> None)
              | _ -> None)
            cmt_scan_items
        in
        Some build_roots
      | _ -> None)
    | _ -> None
  in
  build_roots

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
