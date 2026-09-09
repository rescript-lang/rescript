let fail message = raise (Failure message)
let check condition message = if not condition then fail message

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel contents)

let with_temp_dir run =
  let path = Filename.temp_file "rewatch-compile-assets-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  Fun.protect ~finally:(fun () -> Build_artifacts.remove_tree path) (fun () ->
    run path)

let () =
  with_temp_dir (fun root ->
    let first = Filename.concat root "Example.cmi" in
    let second = Filename.concat root "example.cmt" in
    let unrelated = Filename.concat root "notes.txt" in
    let cleanup_only = Filename.concat root "Example.cmj" in
    let source = Filename.concat root "src/Example.res" in
    let ast = Filename.concat root "Example.ast" in
    let nested = Filename.concat root "nested" in
    write first "cmi";
    write second "cmt";
    write unrelated "notes";
    write cleanup_only "cmj";
    write ast ("Caml1999X\nDependency\n" ^ source ^ "\nbinary payload");
    Unix.mkdir nested 0o755;
    write (Filename.concat nested "Nested.cmi") "nested";
    let state = Compile_assets.create [root; root] in
    check
      (Compile_assets.files state root
      |> List.sort String.compare
      = List.sort String.compare [ast; cleanup_only; first; second])
      "the flat cleanup inventory retains only managed compiler assets";
    check
      (Compile_assets.ast_sources state root = [(ast, source)])
      "published ASTs retain their encoded absolute source location";
    check
      ((Compile_assets.ast state source
       |> Option.map (fun entry -> entry.Compile_assets.path))
      = Some ast)
      "published AST state is addressable by source path";
    let source_path = Filename.concat "src" "Example.res" in
    let ast_modified = (Unix.stat ast).Unix.st_mtime in
    let source_mtimes = Hashtbl.create 1 in
    Hashtbl.add source_mtimes source_path ast_modified;
    check
      (Build.source_is_not_older_than_ast state ~root ~source_mtimes source_path)
      "equal source and AST timestamps follow Rust and require parsing";
    Hashtbl.replace source_mtimes source_path (ast_modified -. 1.);
    check
      (not
         (Build.source_is_not_older_than_ast state ~root ~source_mtimes
            source_path))
      "an AST newer than its source is parse-clean";
    check (Option.is_some (Compile_assets.cmi state "Example"))
      "CMI entries use compiler module keys";
    check (Option.is_some (Compile_assets.cmt state "Example"))
      "CMT entries normalize the first module-name character";
    check (Option.is_none (Compile_assets.cmi state "Nested"))
      "the compiler asset directory is scanned non-recursively";
    Sys.remove first;
    Compile_assets.refresh_cmi state ~key:"Example" ~path:first;
    check (Option.is_none (Compile_assets.cmi state "Example"))
      "refresh removes a deleted CMI")
