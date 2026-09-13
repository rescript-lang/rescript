open OUnit2

let check condition message = assert_bool message condition

let write_file = Test_support.write_file

let write_config root contents =
  write_file (Filename.concat root "rescript.json") contents

let tests =
  "project_context_tests" >:: fun _context ->
  let root = Filename.temp_file "rewatch-ocaml-project-context-" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let root = Unix.realpath root in
  Fun.protect
    ~finally:(fun () -> File_util.remove_tree root)
    (fun () ->
      let dependency = Test_support.path root "packages/dependency" in
      let dev_dependency = Test_support.path root "packages/dev-dependency" in
      let unlisted = Test_support.path root "packages/unlisted" in
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
      check
        (Project_context.display_path ~root (Filename.concat root "packages")
        = "./packages")
        "diagnostic paths inside the project are visibly relative";
      check
        (Project_context.relative_or_absolute ~root
           (Filename.concat root "packages")
        = "packages")
        "source locations inside the project omit a current-directory prefix";
      check
        (Project_context.display_path ~root "/external/dependency"
        = "/external/dependency")
        "diagnostic paths outside the project remain absolute";
      let candidates =
        Project_context.dependency_candidates_in
          (Project_context.dependency_context (Config.load_root root))
          root "@scope/pkg"
      in
      check
        (List.hd candidates
        = Filename.concat (Filename.concat root "node_modules") "@scope/pkg")
        "dependency candidates start with the package-local node_modules path";
      check
        (not (List.mem (Test_support.path root "packages/pkg") candidates))
        "workspace package directories are not implicit dependency candidates";
      check
        (Project_context.dependency_path root "dependency" = None)
        "a package outside node_modules is not resolved implicitly";
      let dependency_config = Config.load_root dependency in
      let dependency_context =
        Project_context.dependency_context dependency_config
      in
      check
        (not
           (Project_context.dependency_is_local_canonical dependency_context
              dev_dependency))
        "a directly invoked workspace package does not own its siblings";
      check
        (Project_context.dependency_candidates_in dependency_context dependency
           "@scope/pkg"
        = [
            Filename.concat
              (Filename.concat dependency "node_modules")
              "@scope/pkg";
            Filename.concat (Filename.concat root "node_modules") "@scope/pkg";
          ])
        "a workspace package checks only its own and the workspace node_modules";
      let node_modules = Filename.concat root "node_modules" in
      File_util.ensure_dir node_modules;
      if
        Test_support.symlink_if_supported ~to_dir:true "../packages/dependency"
          (Filename.concat node_modules "dependency")
        && Test_support.symlink_if_supported ~to_dir:true
             "../packages/dev-dependency"
             (Filename.concat node_modules "dev-dependency")
      then (
        let root_context =
          Project_context.dependency_context (Config.load_root root)
        in
        check
          (Project_context.dependency_is_local_canonical root_context dependency)
          "a workspace-root invocation owns linked workspace dependencies";
        check
          (Project_context.dependency_is_local_canonical root_context
             dev_dependency)
          "a workspace-root invocation owns linked development dependencies";
        let resolution = Package_resolution.create (Config.load_root root) in
        let first =
          Package_resolution.resolve resolution ~package_root:root
            Config.{name = "dependency"; features = Some ["first"]}
        in
        let second =
          Package_resolution.resolve resolution ~package_root:root
            Config.{name = "dependency"; features = Some ["second"]}
        in
        check
          (first.directory = second.directory && first.config = second.config)
          "cached dependency resolution reuses only package identity");
      let repository_tmp = Filename.concat (Sys.getcwd ()) "tmp" in
      File_util.ensure_dir repository_tmp;
      let standalone =
        Filename.temp_file ~temp_dir:repository_tmp "rewatch-ocaml-standalone-"
          ""
      in
      Sys.remove standalone;
      Unix.mkdir standalone 0o755;
      let standalone = Unix.realpath standalone in
      Fun.protect
        ~finally:(fun () -> File_util.remove_tree standalone)
        (fun () ->
          write_config standalone {|{"name":"unlisted-standalone"}|};
          let source = Test_support.path standalone "src/A.res" in
          write_file source "let value = 1\n";
          check
            (Project_context.workspace_lock_root standalone = standalone)
            "an unrelated project below a workspace remains standalone";
          let arguments = Compiler_args_command.run source in
          check
            (not (Test_support.contains_text arguments "\"-bs-jsx\""))
            "standalone compiler arguments do not inherit workspace JSX";
          check
            (Test_support.contains_text arguments "esmodule:src:.js")
            "standalone compiler arguments retain their default package output"))
