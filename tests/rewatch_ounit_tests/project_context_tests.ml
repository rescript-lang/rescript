open OUnit2

let check condition message = assert_bool message condition

let write_file path contents =
  File_util.ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let write_config root contents =
  write_file (Filename.concat root "rescript.json") contents

let tests =
  "project_context_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-project-context-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let dependency = Filename.concat root "packages/dependency" in
      let dev_dependency = Filename.concat root "packages/dev-dependency" in
      let unlisted = Filename.concat root "packages/unlisted" in
      List.iter File_util.ensure_dir [dependency; dev_dependency; unlisted];
      write_config root
        {|{
          "name": "workspace",
          "dependencies": ["dependency"],
          "dev-dependencies": ["dev-dependency"]
        }|};
      write_file
        (Filename.concat root "package.json")
        {|{"workspaces":["packages/*"]}|};
      write_config dependency {|{"name":"dependency"}|};
      write_config dev_dependency {|{"name":"dev-dependency"}|};
      write_config unlisted {|{"name":"unlisted"}|};
      check
        (Project_context.workspace_lock_root dependency = root)
        "listed dependencies inherit the parent workspace context";
      check
        (Project_context.workspace_lock_root dev_dependency = root)
        "listed dev dependencies inherit the parent workspace context";
      check
        (Project_context.workspace_lock_root unlisted = unlisted)
        "package.json workspace globs do not enroll unlisted ReScript packages";
      check
        (Project_context.relative_to root root = ".")
        "a root path is represented by the current-directory component";
      check
        (Project_context.relative_to root (Filename.concat root "packages")
        = "packages")
        "a child path is represented relative to its root";
      let candidates =
        Project_context.dependency_candidates root "@scope/pkg"
      in
      check
        (List.hd candidates
        = Filename.concat (Filename.concat root "node_modules") "@scope/pkg")
        "dependency candidates start with the package-local node_modules path";
      check
        (List.mem (Filename.concat root "packages/pkg") candidates)
        "scoped dependency candidates retain the workspace package fallback";
      let repository_tmp = Filename.concat (Sys.getcwd ()) "tmp" in
      File_util.ensure_dir repository_tmp;
      let standalone =
        Filename.temp_file ~temp_dir:repository_tmp "rewatch-ocaml-standalone-"
          ""
      in
      Sys.remove standalone;
      Unix.mkdir standalone 0o755;
      Fun.protect
        ~finally:(fun () -> File_util.remove_tree standalone)
        (fun () ->
          write_config standalone {|{"name":"unlisted-standalone"}|};
          let source = Filename.concat standalone "src/A.res" in
          write_file source "let value = 1\n";
          check
            (Project_context.workspace_lock_root standalone = standalone)
            "an unrelated project below a workspace remains standalone";
          let arguments = Build.compiler_args source in
          check
            (not (Test_support.contains_text arguments "\"-bs-jsx\""))
            "standalone compiler arguments do not inherit workspace JSX";
          check
            (Test_support.contains_text arguments "esmodule:src:.js")
            "standalone compiler arguments retain their default package output"))
