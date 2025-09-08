module Path = {
  @module("node:path")
  external resolve: (string, string) => string = "resolve"

  @module("node:path")
  external dirname: string => string = "dirname"
}

module Url = {
  @module("node:url")
  external fileURLToPath: string => string = "fileURLToPath"
}

module FsPromises = {
  @module("node:fs/promises")
  external access: string => promise<unit> = "access"
}

module Test = {
  @module("node:test")
  external describe: (string, unit => unit) => unit = "describe"

  @module("node:test")
  external before: (unit => promise<unit>) => unit = "before"

  @module("node:test")
  external beforeEach: (unit => promise<unit>) => unit = "beforeEach"

  @module("node:test")
  external test: (string, unit => promise<unit>) => unit = "test"
}

@scope(("import", "meta"))
external importMetaUrl: string = "url"

external import_: string => promise<'t> = "import"
