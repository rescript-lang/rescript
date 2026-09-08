let check condition message = if not condition then failwith message

let write_file path contents =
  Build_artifacts.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let names modules =
  List.map (fun (module_ : Source.module_) -> module_.name) modules

let discover config ?(prod = false) ?features () =
  Source.discover config ~prod ~features ~filter:None

let () =
  let root = Filename.temp_file "rewatch-ocaml-sources-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> Build.remove_tree root)
    (fun () ->
      write_file (Filename.concat root "src/Main.res") "let value = 1\n";
      write_file (Filename.concat root "test/Test.res") "let value = 1\n";
      write_file (Filename.concat root "test/nested/Nested.res")
        "let value = 1\n";
      write_file (Filename.concat root "native/Native.res")
        "let value = 1\n";
      let config_path = Filename.concat root "rescript.json" in
      write_file config_path
        {|{
          "name": "source-tests",
          "sources": [
            "src",
            {"dir": "test", "type": "dev", "subdirs": true},
            {"dir": "native", "feature": "native"}
          ]
        }|};
      let config = Config.load config_path in
      check
        (names (discover config ()) = ["Main"; "Native"; "Nested"; "Test"])
        "an unrestricted build includes shorthand, dev, recursive, and tagged sources";
      check
        (names (discover config ~prod:true ()) = ["Main"; "Native"])
        "a production build excludes a recursive dev source";
      check
        (names (discover config ~features:["native"] ())
        = ["Main"; "Native"; "Nested"; "Test"])
        "an active leaf feature includes tagged and untagged sources";
      check
        (names (discover config ~features:["other"] ())
        = ["Main"; "Nested"; "Test"])
        "an inactive feature excludes only its tagged source";
      write_file (Filename.concat root "ignored/Nested.res") "let value = 1\n";
      write_file config_path
        {|{
          "name": "unsupported-ignored-dirs",
          "sources": {"dir": "ignored", "subdirs": true},
          "ignored-dirs": ["ignored"]
        }|};
      let config = Config.load config_path in
      check
        (names (discover config ()) = ["Nested"])
        "unsupported ignored-dirs does not suppress source discovery";
      write_file (Filename.concat root "case/lower.res") "let value = 1\n";
      write_file (Filename.concat root "case/Lower.resi") "let value: int\n";
      write_file config_path
        {|{"name":"interface-case","sources":["case"]}|};
      let config = Config.load config_path in
      let casing_rejected =
        try
          ignore (discover config ());
          false
        with Source.Error message ->
          Build.contains_text message
            "Could not initialize build: Implementation and interface have different path names or different cases: `case/lower.res` vs `case/Lower.resi`"
      in
      check casing_rejected
        "implementation and interface basename casing must match";
      write_file (Filename.concat root "case/Lower.res") "let value = 1\n";
      let duplicate_rejected =
        try
          ignore (discover config ());
          false
        with Source.Error message ->
          Build.contains_text message "Duplicate module name: Lower"
      in
      check duplicate_rejected
        "adding the exact implementation still exposes the differently-cased duplicate";
      write_file (Filename.concat root "paths/a/Path.res") "let value = 1\n";
      write_file (Filename.concat root "paths/b/Path.resi") "let value: int\n";
      write_file config_path
        {|{"name":"interface-path","sources":{"dir":"paths","subdirs":true}}|};
      let config = Config.load config_path in
      let path_rejected =
        try
          ignore (discover config ());
          false
        with Source.Error message ->
          Build.contains_text message
            "different path names or different cases: `paths/a/Path.res` vs `paths/b/Path.resi`"
      in
      check path_rejected
        "an interface cannot attach to a same-named implementation in another directory")
