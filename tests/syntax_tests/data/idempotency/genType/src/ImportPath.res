open GenTypeCommon

type t = (string, string)

let propTypes = ("", "prop-types")

let react = ("", "react")
let reasonReactPath = (~config) => ("", config.reasonReactPath)
let bsBlockPath = (~config) => ("", Config_.getBsBlockPath(~config))

let bsCurryPath = (~config) => ("", Config_.getBsCurryPath(~config))

let fromModule = (~dir, ~importExtension, moduleName) => {
  let withNoPath =
    (moduleName->ModuleName.toString->ScopedPackage.removeGeneratedModule) ++ importExtension
  (dir, withNoPath)
}

let fromStringUnsafe = s => ("", s)

let chopExtensionSafe = s =>
  try s->Filename.chop_extension catch {
  | Invalid_argument(_) => s
  }

let dump = ((dir, s)) => NodeFilename.concat(dir, s)

let toCmt = (~config, ~outputFileRelative, (dir, s)) => {
  open Filename
  concat(
    outputFileRelative->dirname,
    ((dir, s->chopExtensionSafe)->dump) ++
      (switch config.namespace {
      | None => ""
      | Some(name) => "-" ++ name
      } ++
      ".cmt"),
  )
}

let emit = (~config, (dir, s)) =>
  switch config.importPath {
  | Relative => (dir, s)->dump
  | Node => s
  }
