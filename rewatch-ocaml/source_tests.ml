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
        "an inactive feature excludes only its tagged source")
