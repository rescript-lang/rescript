module Path = {
  @module("node:path") external join2: (string, string) => string = "join"
  @module("node:path") @variadic external join: array<string> => string = "join"
  @module("node:path") external dirname: string => string = "dirname"
}

module Process = {
  @scope("process") external cwd: unit => string = "cwd"
  @scope("process") @val external version: string = "version"
  @scope("process") @val external argv: array<string> = "argv"
  @scope("process") external exit: int => unit = "exit"
  @scope("process") external env: Dict.t<string> = "env"
}

module Fs = {
  @module("node:fs") external readdirSync: string => array<string> = "readdirSync"
  @module("node:fs/promises") external writeFile: (string, string) => promise<unit> = "writeFile"
  @module("node:fs") external existsSync: string => bool = "existsSync"
  @module("node:fs") external mkdirSync: string => unit = "mkdirSync"
  @module("node:fs") external writeFileSync: (string, string) => unit = "writeFileSync"
  @module("node:fs") external readFileSync: (string, ~encoding: string) => string = "readFileSync"
}

module Buffer = {
  type t
  @send external toString: t => string = "toString"
}

module ChildProcess = {
  type readable
  type spawnReturns = {stderr: readable, stdout: readable}
  type spawnOptions = {cwd?: string, env?: Dict.t<string>, timeout?: int}
  @module("node:child_process")
  external spawn: (string, array<string>, ~options: spawnOptions=?) => spawnReturns = "spawn"

  @send external on: (readable, string, Buffer.t => unit) => unit = "on"
  @send
  external once: (spawnReturns, string, (Js.Null.t<float>, Js.Null.t<string>) => unit) => unit =
    "once"
  type execSyncOptions = {maxBuffer?: float}
  @module("child_process")
  external execSync: (string, ~options: execSyncOptions=?) => Buffer.t = "execSync"
}

module OS = {
  @module("node:os")
  external cpus: unit => array<{.}> = "cpus"
}

module URL = {
  @module("node:url") external fileURLToPath: string => string = "fileURLToPath"
}

@val @scope(("import", "meta")) external url: string = "url"
@val @scope(("import", "meta")) external dirname: string = "dirname"
