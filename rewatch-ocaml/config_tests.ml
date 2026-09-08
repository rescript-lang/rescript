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

let rec contains_adjacent left right = function
  | current :: next :: _ when current = left && next = right -> true
  | _ :: rest -> contains_adjacent left right rest
  | [] -> false

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
        ];
      write_file path
        {|{"name":"different-outputs","package-specs":[{"module":"esmodule","in-source":true,"suffix":".js"},{"module":"commonjs","in-source":false,"suffix":".js"}]}|};
      let config = Config.load path in
      check (List.length config.package_specs = 2)
        "the same suffix is allowed in different output locations";
      write_file path
        {|{"name":"gentype-precedence","package-specs":{"module":"commonjs"},"gentypeconfig":{"module":"esmodule"}}|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-gentype-module" "esmodule"
           config.gentype_args)
        "an explicit GenType module overrides package-specs";
      write_file path {|{"name":"no-gentype"}|};
      let config = Config.load path in
      check (config.gentype_args = [])
        "GenType arguments are absent without gentypeconfig";
      write_file path
        {|{"name":"jsx","jsx":{"module":"Voby.JSX","preserve":true}}|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-jsx-module" "Voby.JSX" config.jsx_args)
        "custom JSX modules are accepted";
      check (List.mem "-bs-jsx-preserve" config.jsx_args)
        "JSX preserve is projected to compiler arguments";
      write_file path {|{"name":"maps-disabled","sourceMap":false}|};
      let config = Config.load path in
      check (config.source_map_args = ["-bs-source-map"; "false"])
        "sourceMap false disables source maps explicitly";
      write_file path
        {|{"name":"tooling-config","editor":{"anything":true},"reanalyze":[1,2,3]}|};
      let config = Config.load path in
      check
        (not (has_diagnostic config "editor")
        && not (has_diagnostic config "reanalyze"))
        "editor and reanalyze payloads are accepted without validation";
      write_file path
        {|{"name":"bad-module","package-specs":{"module":"es6-global"}}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "unsupported package module"
      in
      check rejected "unsupported package modules are rejected";
      write_file path
        {|{"name":"hidden-map","sourceMap":{"enabled":"always","mode":"hidden"}}|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-source-map" "hidden" config.source_map_args)
        "hidden source maps are accepted";
      write_file path
        {|{"name":"bad-map","sourceMap":{"enabled":"always","mode":"external"}}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "sourceMap.mode"
      in
      check rejected "unknown source map modes are rejected";
      write_file path
        {|{"name":"legacy-dev-deps","bs-dev-dependencies":["dep"]}|};
      let config = Config.load path in
      check
        (match config.dev_dependencies with
        | [{name = "dep"; features = None}] -> true
        | _ -> false)
        "bs-dev-dependencies is accepted";
      check
        (List.exists
           (fun message -> contains message "field 'bs-dev-dependencies'")
           config.diagnostics)
        "bs-dev-dependencies emits its deprecation";
      write_file path {|{"suffix":".mjs"}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "missing required field \"name\""
      in
      check rejected "a package config without a name is rejected")
