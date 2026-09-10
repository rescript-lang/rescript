open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop index =
    if index + fragment_length > text_length then false
    else if String.sub text index fragment_length = fragment then true
    else loop (index + 1)
  in
  fragment_length = 0 || loop 0

let count_occurrences text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec loop index count =
    if index + fragment_length > text_length then count
    else if String.sub text index fragment_length = fragment then
      loop (index + fragment_length) (count + 1)
    else loop (index + 1) count
  in
  if fragment_length = 0 then 0 else loop 0 0

let rejects path contents fragment =
  write_file path contents;
  try
    ignore (Config.load path);
    false
  with Config.Error message -> contains message fragment

let has_diagnostic config field =
  List.exists
    (fun message -> contains message ("'" ^ field ^ "'"))
    config.Config.diagnostics

let rec contains_adjacent left right = function
  | current :: next :: _ when current = left && next = right -> true
  | _ :: rest -> contains_adjacent left right rest
  | [] -> false

let tests =
  "config_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-config-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let path = Filename.concat root "rescript.json" in
      let missing_path = Filename.concat root "missing.json" in
      check
        (try
           ignore (Config.load missing_path);
           false
         with Config.Error message ->
           contains message "Could not read"
           && count_occurrences message missing_path = 1)
        "missing configuration files produce contextual config errors";
      let directory_path = Filename.concat root "config-directory" in
      Unix.mkdir directory_path 0o755;
      check
        (try
           ignore (Config.load directory_path);
           false
         with Config.Error message ->
           contains message "Could not read"
           && count_occurrences message directory_path = 1)
        "configuration directories produce contextual config errors";
      write_file path {|{"name":"missing-sources"}|};
      let config = Config.load path in
      check
        (not config.sources_defined)
        "an omitted sources field remains distinguishable for package warnings";
      write_file path {|{"name":"empty-sources","sources":[]}|};
      let config = Config.load path in
      check config.sources_defined
        "an explicit empty sources field does not trigger the missing-field \
         warning";
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
          check
            (has_diagnostic config field)
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
          check
            (not (has_diagnostic config field))
            ("unexpected unknown-field diagnostic for " ^ field))
        [
          "sources.?.nested-source-key";
          "package-specs.?.nested-package-key";
          "sourceMap.?.nested-map-key";
        ];
      write_file path
        {|{"name":"different-outputs","package-specs":[{"module":"esmodule","in-source":true,"suffix":".js"},{"module":"commonjs","in-source":false,"suffix":".js"}]}|};
      let config = Config.load path in
      check
        (List.length config.package_specs = 2)
        "the same suffix is allowed in different output locations";
      write_file path
        {|{"name":"gentype-precedence","package-specs":{"module":"commonjs"},"gentypeconfig":{"module":"esmodule"}}|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-gentype-module" "esmodule" config.gentype_args)
        "an explicit GenType module overrides package-specs";
      let source_dir = Filename.concat root "src" in
      let shim_dir = Filename.concat source_dir "shims" in
      Unix.mkdir source_dir 0o755;
      Unix.mkdir shim_dir 0o755;
      write_file path
        {|{"name":"gentype-subdirs","sources":{"dir":"src","subdirs":true},"gentypeconfig":{}}|};
      let config = Config.load path in
      let stats =
        Build_types.create ~warning_state:(Warning_state.create ())
          ~poll:(fun () -> ())
          ~verbosity:0
      in
      let package =
        Package_graph.discover ~root_config:config ~prod:false ~features:None
          ~warn_error:None ~filter:None ~stats
        |> List.hd
      in
      check
        (contains_adjacent "-bs-gentype-source-dir"
           (Filename.concat "src" "shims")
           package.graph_compile_config.gentype_args)
        "GenType recursively includes directories that may contain TypeScript \
         shims";
      write_file path {|{"name":"no-gentype"}|};
      let config = Config.load path in
      check (config.gentype_args = [])
        "GenType arguments are absent without gentypeconfig";
      write_file path {|{"name":"ignored-payload","ignored-dirs":true}|};
      let config = Config.load path in
      check
        (has_diagnostic config "ignored-dirs")
        "unsupported ignored-dirs payloads are diagnosed but not decoded";
      write_file path {|{"name":"jsx-v3","jsx":{"v3-dependencies":true}}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "jsx.v3-dependencies"
      in
      check rejected "jsx.v3-dependencies must be an array of strings";
      write_file path
        {|{"name":"namespace-entry-without-namespace","namespace-entry":"Entry"}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "requires a namespace"
      in
      check rejected "namespace-entry requires namespace configuration";
      write_file path {|{"name":"unsupported-jsx","jsx":{"version":3}}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message -> contains message "jsx.version"
      in
      check rejected "unsupported JSX versions are rejected without panicking";
      write_file path
        {|{"name":"internal-path","path":"ignored","gentypeconfig":{}}|};
      let alias_dir = Filename.concat root "alias" in
      Unix.mkdir alias_dir 0o755;
      let aliased_path =
        Filename.concat alias_dir
          (Filename.concat Filename.parent_dir_name "rescript.json")
      in
      let config = Config.load aliased_path in
      check
        (config.path = Unix.realpath path)
        "config loading canonicalizes a noncanonical path alias";
      check
        (config.root = Unix.realpath root)
        "the package root uses the canonical config path";
      let source = Filename.concat root "src/A.res" in
      let build_dir = Filename.concat config.root "lib/bs" in
      File_util.ensure_dir build_dir;
      File_util.ensure_dir (Filename.dirname source);
      write_file source "let value = 1\n";
      let parse_job =
        Compiler_process.parse_job ~bsc:"bsc" ~build_dir ~config "src/A.res"
      in
      let module_ =
        Source.
          {
            name = "A";
            implementation = "src/A.res";
            interface = None;
            is_dev = false;
            feature = None;
            deps = [];
          }
      in
      let compile_job =
        Compiler_process.compile_job ~bsc:"bsc" ~runtime:"runtime" ~build_dir
          ~watch:false ~config ~dependency_dirs:[] module_ ~is_interface:false
          "src/A.res"
      in
      let rec argument_after expected = function
        | argument :: value :: _ when argument = expected -> Some value
        | _ :: rest -> argument_after expected rest
        | [] -> None
      in
      check
        (parse_job.Process.cwd = compile_job.Process.cwd
        && parse_job.cwd = build_dir)
        "parser and compiler jobs derive the same canonical working directory";
      check
        (argument_after "-bs-project-root" compile_job.args = Some config.root)
        "the compiler project-root argument uses the canonical job root";
      check
        (rejects path {|{"name":"internal-path","path":false}|} "path")
        "the internal path field retains Rust's string schema";
      write_file path
        {|{"name":"flag-whitespace","compiler-flags":["  -w  +A  "]}|};
      let config = Config.load path in
      check
        (config.compiler_flags = ["-w"; "+A"])
        "compiler flag whitespace does not create empty subprocess arguments";
      write_file path
        {|{
          "name": "argument-order",
          "compiler-flags": ["-open Belt"],
          "warnings": {"number": "+A"},
          "jsx": {"mode": "automatic"},
          "sourceMap": {"enabled": "always", "mode": "hidden"},
          "gentypeconfig": {},
          "experimental-features": {"LetUnwrap": true}
        }|};
      let config = Config.load path in
      check
        (Compiler_args.compiler_flags ~source_maps:false ~watch:false
           ~gentype:false config
        = [
            "-bs-jsx-mode";
            "automatic";
            "-enable-experimental";
            "LetUnwrap";
            "-w";
            "+A";
            "-open";
            "Belt";
          ])
        "parser arguments follow Rust phase ordering";
      check
        (Compiler_args.compiler_flags ~source_maps:true ~watch:false
           ~gentype:true config
        = [
            "-bs-jsx-mode";
            "automatic";
            "-bs-source-map";
            "hidden";
            "-open";
            "Belt";
            "-w";
            "+A";
            "-bs-gentype";
            "-enable-experimental";
            "LetUnwrap";
          ])
        "compiler arguments follow Rust phase ordering";
      let optional_ppx =
        [
          ["graphql-ppx"];
          ["graphql_ppx"];
          ["spice"];
          ["rescript-relay"];
          ["re-formality"];
          ["bisect_ppx"];
          ["always"];
          [];
        ]
      in
      check
        (Compiler_args.filter_ppx_flags ~bisect_enabled:false optional_ppx
           "let value = 1"
        = [["always"]])
        "source-specific and disabled Bisect PPXs are filtered";
      check
        (Compiler_args.filter_ppx_flags ~bisect_enabled:true optional_ppx
           "%graphql @spice %relay %form"
        = [
            ["graphql-ppx"];
            ["graphql_ppx"];
            ["spice"];
            ["rescript-relay"];
            ["re-formality"];
            ["bisect_ppx"];
            ["always"];
          ])
        "source markers and the Bisect environment enable their PPXs";
      check
        (match
           Compiler_args.compiler_flags
             ~ppx_flags:[["tool"; "--arg"]]
             ~source_maps:false ~watch:false ~gentype:false config
         with
        | "-ppx" :: "tool --arg" :: _ -> true
        | _ -> false)
        "parser arguments include the filtered PPX command";
      [
        {|{"name":"first","name":"second"}|};
        {|{"name":"duplicate-source","sources":{"dir":"a","dir":"b"}}|};
        {|{"name":"duplicate-spec","package-specs":{"module":"esmodule","module":"commonjs"}}|};
        {|{"name":"duplicate-warning","warnings":{"number":"A","number":"B"}}|};
        {|{"name":"duplicate-jsx","jsx":{"mode":"classic","mode":"automatic"}}|};
        {|{"name":"duplicate-gentype","gentypeconfig":{"module":"esmodule","module":"commonjs"}}|};
        {|{"name":"duplicate-post","js-post-build":{"cmd":"true","cmd":"false"}}|};
        {|{"name":"duplicate-dependency","dependencies":[{"name":"a","name":"b"}]}|};
      ]
      |> List.iter (fun json ->
          check
            (rejects path json "duplicate field")
            "typed configuration objects reject duplicate fields");
      write_file path
        {|{
          "name": "map-duplicates",
          "future": 1,
          "future": 2,
          "sourceMap": {"enabled": "always", "mode": "linked", "mode": "inline"},
          "features": {"selected": ["first"], "selected": ["last"]},
          "experimental-features": {"LetUnwrap": true, "LetUnwrap": false},
          "gentypeconfig": {"debug": {"all": true, "all": false}}
        }|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-source-map" "inline" config.source_map_args)
        "sourceMap map decoding keeps the last duplicate value";
      check
        (List.assoc "selected" config.features = ["last"])
        "feature map decoding keeps the last duplicate value";
      check
        (config.experimental_args = [])
        "experimental feature maps keep the last duplicate value";
      check
        (not (List.mem "-bs-gentype-debug" config.gentype_args))
        "GenType debug maps keep the last duplicate value";
      [
        "sources";
        "package-specs";
        "warnings";
        "suffix";
        "dependencies";
        "bs-dependencies";
        "dev-dependencies";
        "bs-dev-dependencies";
        "features";
        "ppx-flags";
        "compiler-flags";
        "bsc-flags";
        "namespace";
        "jsx";
        "sourceMap";
        "experimental-features";
        "gentypeconfig";
        "js-post-build";
        "namespace-entry";
        "allowed-dependents";
      ]
      |> List.iter (fun field ->
          write_file path
            (Printf.sprintf {|{"name":"null-option","%s":null}|} field);
          ignore (Config.load path));
      write_file path
        {|{
          "name": "nested-null-options",
          "sources": {"dir": "src", "subdirs": null, "type": null, "feature": null},
          "package-specs": {"module": "esmodule", "suffix": null},
          "warnings": {"number": null, "error": null},
          "dependencies": [{"name": "dep", "features": null}],
          "jsx": {"version": null, "module": null, "mode": null, "v3-dependencies": null, "preserve": null},
          "sourceMap": {"enabled": "always", "mode": "linked", "sourcesContent": null, "sourceRoot": null},
          "gentypeconfig": {"module": null, "moduleResolution": null, "exportInterfaces": null, "generatedFileExtension": null}
        }|};
      ignore (Config.load path);
      write_file path
        {|{"name":"jsx","jsx":{"module":"Voby.JSX","preserve":true}}|};
      let config = Config.load path in
      check
        (contains_adjacent "-bs-jsx-module" "Voby.JSX" config.jsx_args)
        "custom JSX modules are accepted";
      check
        (List.mem "-bs-jsx-preserve" config.jsx_args)
        "JSX preserve is projected to compiler arguments";
      write_file path {|{"name":"maps-disabled","sourceMap":false}|};
      let config = Config.load path in
      check
        (config.source_map_args = ["-bs-source-map"; "false"])
        "sourceMap false disables source maps explicitly";
      write_file path
        {|{"name":"tooling-config","editor":{"anything":true},"reanalyze":[1,2,3]}|};
      let config = Config.load path in
      check
        ((not (has_diagnostic config "editor"))
        && not (has_diagnostic config "reanalyze"))
        "editor and reanalyze payloads are accepted without validation";
      write_file path
        {|{"name":"bad-module","package-specs":{"module":"es6-global"}}|};
      let rejected =
        try
          ignore (Config.load path);
          false
        with Config.Error message ->
          contains message "unsupported package module"
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
        with Config.Error message ->
          contains message "missing required field \"name\""
      in
      check rejected "a package config without a name is rejected";
      write_file path
        {|{"name":"getters","suffix":".mjs","package-specs":{"module":"esmodule"},"dependencies":["plain",{"name":"qualified","features":["native"]}]}|};
      let config = Config.load path in
      check (config.name = "getters") "the package name is retained";
      check
        (match config.package_specs with
        | [spec] -> Config.package_spec_suffix config spec = ".mjs"
        | _ -> false)
        "the configured suffix applies to package specs";
      check
        (match config.dependencies with
        | [plain; qualified] ->
          plain.name = "plain" && plain.features = None
          && qualified.name = "qualified"
          && qualified.features = Some ["native"]
        | _ -> false)
        "shorthand and feature-qualified dependencies retain their data")
