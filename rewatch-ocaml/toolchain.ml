exception Error of string

let canonical_existing ~message path =
  try
    if File_util.exists path then Platform.canonicalize_path path
    else raise (Error (message path))
  with
  | Unix.Unix_error (error, function_name, argument) ->
    raise
      (Error
         (Printf.sprintf "%s: %s (%s %s)" (message path)
            (Unix.error_message error) function_name argument))
  | Sys_error detail -> raise (Error (message path ^ ": " ^ detail))

let absolute_program ~cwd program =
  let resolved = Platform.resolve_program ~cwd program in
  if Filename.is_relative resolved then Filename.concat cwd resolved
  else resolved

(* The npm package (@rescript/<target>) with the binaries for a platform, as in
   cli/common/platform.cjs. [system] and [architecture] are the names OCaml
   uses for the platform this executable was built for, where x64 is "amd64".
   Windows on ARM needs no case of its own: it runs the win32-x64 binaries,
   which were built for amd64. Intel Macs have no package since ReScript 13. *)
let platform_package ~system ~architecture =
  let os =
    match system with
    | "linux" -> Some "linux"
    | "macosx" -> Some "darwin"
    | "mingw" | "mingw64" | "win32" | "win64" | "cygwin" -> Some "win32"
    | _ -> None
  in
  let arch =
    match architecture with
    | "amd64" -> Some "x64"
    | "arm64" -> Some "arm64"
    | _ -> None
  in
  match (os, arch) with
  | Some "darwin", Some "x64" -> None
  | Some os, Some arch -> Some ("@rescript/" ^ os ^ "-" ^ arch)
  | _ -> None

(* Builds use the embedded compiler. The bsc.exe shipped next to the
   executable is still recorded in compiler-info.json, where editor tooling
   looks for bsc.exe and the other platform binaries. A development build has
   no bsc.exe next to it, so it records the one in the platform package
   installed with rescript instead, if there is one. *)
let bundled_bsc ~cwd ~executable ~root ~find_package =
  let executable = absolute_program ~cwd executable in
  let executable =
    canonical_existing
      ~message:(fun path -> "Could not locate current executable " ^ path)
      executable
  in
  let sibling = Filename.concat (Filename.dirname executable) "bsc.exe" in
  let package_bsc directory =
    let bsc = Filename.concat (Filename.concat directory "bin") "bsc.exe" in
    if File_util.exists bsc then Some bsc else None
  in
  let platform_bsc name =
    match find_package root name with
    | Some directory -> package_bsc directory
    | None ->
      (* With pnpm, the platform package is only visible from rescript. *)
      Option.bind (find_package root "rescript") (fun rescript ->
          Option.bind (find_package rescript name) package_bsc)
  in
  if File_util.exists sibling then sibling
  else
    match
      platform_package ~system:Build_platform.system
        ~architecture:Build_platform.architecture
    with
    | None -> sibling
    | Some name -> Option.value (platform_bsc name) ~default:sibling

let runtime ~find_package =
  match Sys.getenv_opt "RESCRIPT_RUNTIME" with
  | Some path ->
    canonical_existing
      ~message:(fun missing ->
        "RESCRIPT_RUNTIME points to missing path " ^ missing)
      path
  | None -> (
    match find_package "@rescript/runtime" with
    | Some path -> path
    | None ->
      raise
        (Error
           "The rescript runtime package could not be found.\n\
            Please set RESCRIPT_RUNTIME environment variable or make sure the \
            runtime package is installed."))
