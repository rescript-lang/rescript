type shellResult = {exitCode: int}

module CommandChild = {
  type t = promise<shellResult>

  @send
  external kill: (t, @as("SIGKILL") _) => unit = "kill"
}

module ShellPromise = {
  type t = promise<shellResult>

  @send
  external text: t => promise<string> = "text"

  @send
  external json: t => promise<JSON.t> = "json"

  @send
  external cwd: (t, string) => t = "cwd"

  @send
  external noThrow: t => t = "noThrow"

  @send
  external spawn: t => CommandChild.t = "spawn"
}

@module("dax-sh") @taggedTemplate
external sh: (array<string>, array<string>) => ShellPromise.t = "$"
