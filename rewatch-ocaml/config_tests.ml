let check condition message = if not condition then failwith message

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let has_diagnostic config field =
  List.exists (fun message -> contains message ("'" ^ field ^ "'")) config.Config.diagnostics

let () =
  let root = Filename.temp_file "rewatch-ocaml-config-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree root)
    (fun () ->
      let path = Filename.concat root "rescript.json" in
      write_file path
        {|{
          "name": "unknown-fields",
          "sources": {"dir": "src", "nested-source-key": true},
          "package-specs": {
            "module": "esmodule",
            "nested-package-key": true
          },
          "warnings": {"nested-warning-key": true},
          "jsx": {"nested-jsx-key": true},
          "sourceMap": {
            "enabled": "always",
            "mode": "linked",
            "nested-map-key": true
          },
          "gentypeconfig": {"nested-gentype-key": true},
          "js-post-build": {"cmd": "true", "nested-post-key": true},
          "top-key": true
        }|};
      let config = Config.load path in
      List.iter
        (fun field ->
          check (has_diagnostic config field)
            ("missing unknown-field diagnostic for " ^ field))
        [
          "warnings.?.nested-warning-key";
          "jsx.?.nested-jsx-key";
          "gentypeconfig.?.nested-gentype-key";
          "js-post-build.?.nested-post-key";
          "top-key";
        ];
      List.iter
        (fun field ->
          check (not (has_diagnostic config field))
            ("unexpected unknown-field diagnostic for " ^ field))
        [
          "sources.?.nested-source-key";
          "package-specs.?.nested-package-key";
          "sourceMap.?.nested-map-key";
        ])
