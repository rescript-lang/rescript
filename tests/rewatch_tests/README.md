# Rewatch end‑to‑end tests (repos)

In [./repos](repos) we define small projects for different package managers/runtimes and run them through the local `<repo>/cli/rescript.js` (rewatch). For reliable tests we must not depend on how a particular `bsc` binary happens to discover the standard library (stdlib).

## Why we disable stdlib in tests

`bsc` in this repository resolves the stdlib relative to the executable path. It first computes a runtime root, then sets the stdlib path to `<runtime>/lib/ocaml`:
```1:12:compiler/ext/config.ml
(* This resolves the location of the standard library starting from the location of bsc.exe
   (@rescript/{platform}/bin/bsc.exe), handling different supported package layouts. *)
let runtime_module_path =
  let build_path rest path =
    String.concat Filename.dir_sep (List.rev_append rest path)
  in
  match
    Sys.executable_name |> Filename.dirname
    |> String.split_on_char Filename.dir_sep.[0]
    |> List.rev
  with
```
```24:37:compiler/ext/config.ml
  | "bin" :: _platform :: "@rescript" :: "node_modules" :: rest ->
    build_path rest ["node_modules"; "@rescript"; "runtime"]
  (* Local development: e.g. <repo>/packages/@rescript/{platform}/bin *)
  | _ :: _ :: _ :: _ :: rest ->
    build_path rest ["packages"; "@rescript"; "runtime"]

let standard_library =
  let ( // ) = Filename.concat in
  runtime_module_path // "lib" // "ocaml"
```

Unless explicitly disabled, that stdlib directory is appended to the compiler load path:
```26:33:compiler/core/res_compmisc.ml
Config.load_path :=
  if !Js_config.no_stdlib then exp_dirs
  else List.rev_append exp_dirs [Config.standard_library];
```

This means different `bsc` layouts (local build vs installed platform package vs deno-installed wrapper) can produce different behavior in tests. To make repos tests consistent and independent of the `bsc` location, we always disable stdlib in those projects.

## Required test configuration

Add this to every `rescript.json` in `./repos/*` test projects:
```json
{
  "compiler-flags": ["-nostdlib"]
}
```

The flag is wired as follows:
```292:295:compiler/bsc/rescript_compiler_main.ml
("-nostdlib", set Js_config.no_stdlib, "*internal* Don't use stdlib");
```

With `-nostdlib`, `bsc` won’t auto-include the stdlib derived from the executable path, so tests won’t accidentally pass/fail based on where the binary lives. Any required runtime modules must then be provided via the installed packages in the test repo’s `node_modules`—mirroring real user setups.