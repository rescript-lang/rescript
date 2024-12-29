module Path = {
  @module("path") @variadic external join: array<string> => string = "join"
  @module("path") external dirname: string => string = "dirname"
}

module Process = {
  @scope("process") external cwd: unit => string = "cwd"
  @scope("process") @val external version: string = "version"
}

module Fs = {
  @module("fs") external readdirSync: string => array<string> = "readdirSync"
  @module("node:fs/promises") external writeFile: (string, string) => promise<unit> = "writeFile"
}

module Buffer = {
  type t
  @send external toString: t => string = "toString"
}

module ChildProcess = {
  type readable
  type spawnReturns = {stderr: readable, stdout: readable}
  type options = {cwd?: string, env?: Dict.t<string>, timeout?: int}
  @module("child_process")
  external spawn: (string, array<string>, ~options: options=?) => spawnReturns = "spawn"

  @send external on: (readable, string, Buffer.t => unit) => unit = "on"
  @send
  external once: (spawnReturns, string, (Js.Null.t<float>, Js.Null.t<string>) => unit) => unit =
    "once"
}

module OS = {
  @module("os")
  external cpus: unit => array<{.}> = "cpus"
}

module URL = {
  @module("url") external fileURLToPath: string => string = "fileURLToPath"
}

@val @scope(("import", "meta")) external url: string = "url"
