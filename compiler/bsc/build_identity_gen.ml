let () =
  let relevant file =
    Filename.basename file = "dune"
    || List.exists (Filename.check_suffix file) [".ml"; ".mli"; ".c"; ".h"]
  in
  let files =
    Array.to_list Sys.argv |> List.tl |> List.filter relevant
    |> List.sort String.compare
  in
  let normalize path =
    String.map
      (function
        | '\\' -> '/'
        | character -> character)
      path
  in
  let has_suffix suffix =
    List.exists
      (fun file -> Filename.check_suffix (normalize file) suffix)
      files
  in
  [
    "/flow_parser/parser/expression_parser.ml";
    "/ext/ext_platform_primitives_stubs.c";
    "/ext/platform/native/ext_platform_primitives.ml";
    "/core/build_artifact_stubs.c";
    "/syntax/src/res_driver.ml";
    "/syntax/src/dune";
  ]
  |> List.iter (fun suffix ->
      if not (has_suffix suffix) then
        failwith ("compiler identity is missing required input " ^ suffix));
  let digest =
    files
    |> List.map (fun file -> file ^ "\000" ^ Digest.file file)
    |> String.concat "\000" |> Digest.string
  in
  Printf.printf "let value = %S\n" (Digest.to_hex digest)
