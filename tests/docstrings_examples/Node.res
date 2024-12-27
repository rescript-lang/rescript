module Path = {
  @module("path") external join2: (string, string) => string = "join"
  @module("path") @variadic external join: array<string> => string = "join"
}

module Process = {
  @scope("process") external exit: int => unit = "exit"
  @scope(("process", "stderr"))
  external stderrWrite: string => unit = "write"
  @scope("process") external cwd: unit => string = "cwd"
  @val @scope("process")
  external argv: array<string> = "argv"
}

module Fs = {
  @module("fs") external existsSync: string => bool = "existsSync"
  @module("fs") external readdirSync: string => array<string> = "readdirSync"
  @module("node:fs/promises") external writeFile: (string, string) => promise<unit> = "writeFile"
}

module Buffer = {
  type t
  @send external toString: t => string = "toString"
}

module ChildProcess = {
  type spawnSyncReturns = {stdout: Buffer.t}
  @module("child_process")
  external spawnSync: (string, array<string>) => spawnSyncReturns = "spawnSync"

  type readable
  type spawnReturns = {stderr: readable, stdout: readable}
  type options = {cwd?: string, env?: Dict.t<string>, timeout?: int}
  @module("child_process")
  external spawn: (string, array<string>, ~options: options=?) => spawnReturns = "spawn"

  @send external on: (readable, string, Buffer.t => unit) => unit = "on"
  @send external onFromSpawn: (spawnReturns, string, Js.Null.t<float> => unit) => unit = "on"
  @send
  external once: (spawnReturns, string, (Js.Null.t<float>, Js.Null.t<string>) => unit) => unit =
    "once"
  @send
  external onceError: (spawnReturns, string, Js.Exn.t => unit) => unit = "once"
}

module OS = {
  @module("os")
  external tmpdir: unit => string = "tmpdir"

  @module("os")
  external cpus: unit => array<{.}> = "cpus"
}

module Util = {
  type arg = {@as("type") type_: string}
  type config = {
    args: array<string>,
    options: Dict.t<arg>,
  }
  type parsed = {
    values: Dict.t<string>,
    positionals: array<string>,
  }
  @module("node:util") external parseArgs: config => parsed = "parseArgs"
}
