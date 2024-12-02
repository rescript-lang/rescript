module Node = {
  module Path = {
    @module("path") external join2: (string, string) => string = "join"
    @module("path") @variadic external join: array<string> => string = "join"
    @module("path") external dirname: string => string = "dirname"
  }

  module URL = {
    @module("url") external fileURLToPath: string => string = "fileURLToPath"
  }

  module Process = {
    @scope("process") external exit: int => unit = "exit"
    @scope(("process", "stderr"))
    external stderrWrite: string => unit = "write"
    @scope("process") external cwd: unit => string = "cwd"
  }

  module Fs = {
    @module("fs") external readFileSync: string => string = "readFileSync"
    @module("fs") external writeFileSync: (string, string) => unit = "writeFileSync"
    @module("fs") external mkdirSync: string => option<string> = "mkdirSync"
    @module("fs") external existsSync: string => bool = "existsSync"
    @module("fs") external readdirSync: string => array<string> = "readdirSync"
    @module("node:fs/promises") external writeFile: (string, string) => promise<unit> = "writeFile"
    @module("node:fs/promises") external unlink: string => promise<unit> = "unlink"
    @module("node:fs/promises") external lstat: string => promise<'a> = "lstat"
  }

  module Buffer = {
    type t
    @send external toString: t => string = "toString"
  }

  module ChildProcess = {
    type execSyncOpts = {stdio?: string, cwd?: string}
    @module("child_process")
    external execFileSync: (string, array<string>, execSyncOpts) => Buffer.t = "execFileSync"

    type spawnSyncReturns = {stdout: Buffer.t}
    @module("child_process")
    external spawnSync: (string, array<string>) => spawnSyncReturns = "spawnSync"

    type readable
    type spawnReturns = {stderr: readable}
    @module("child_process")
    external spawn: (string, array<string>) => spawnReturns = "spawn"

    @send external on: (readable, string, Buffer.t => unit) => unit = "on"
    @send
    external once: (spawnReturns, string, (Js.Null.t<float>, Js.Null.t<string>) => unit) => unit =
      "once"
  }

  module OS = {
    @module("os")
    external tmpdir: unit => string = "tmpdir"
  }
}

open Node

module Docgen = RescriptTools.Docgen

let bscBin = Path.join(["cli", "bsc"])

type example = {
  id: string,
  kind: string,
  name: string,
  docstrings: array<string>,
}

let createFileInTempDir = id => Path.join2(OS.tmpdir(), id)

let testCode = async (~id, ~code) => {
  let id = id->String.includes("/") ? String.replace(id, "/", "slash_op") : id
  let tempFileName = createFileInTempDir(id)

  let () = await Fs.writeFile(tempFileName ++ ".res", code)

  let args = [tempFileName ++ ".res", "-w", "-3-109"]

  let promise = await Promise.make((resolve, _reject) => {
    let spawn = ChildProcess.spawn(bscBin, args)
    let stderr = []
    spawn.stderr->ChildProcess.on("data", data => {
      Array.push(stderr, data)
    })
    spawn->ChildProcess.once("close", (_code, _signal) => {
      resolve(stderr)
    })
  })

  switch Array.length(promise) > 0 {
  | true =>
    promise
    ->Array.map(e => e->Buffer.toString)
    ->Array.join("")
    ->Error
  | false => Ok()
  }
}

let extractDocFromFile = file => {
  let toolsBin = Path.join([Process.cwd(), "cli", "rescript-tools"])
  let spawn = ChildProcess.spawnSync(toolsBin, ["doc", file])

  spawn.stdout
  ->Buffer.toString
  ->JSON.parseExn
  ->Docgen.decodeFromJson
}

let getExamples = ({items}: Docgen.doc) => {
  let rec loop = (items: list<Docgen.item>, acc: list<example>) => {
    switch items {
    | list{Value({docstrings, id, name}), ...rest} =>
      loop(rest, list{{id, name, docstrings, kind: "value"}, ...acc})
    | list{Type({docstrings, id, name}), ...rest} =>
      loop(rest, list{{id, name, docstrings, kind: "type"}, ...acc})
    | list{Module({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "module"}, ...acc},
      )
    | list{ModuleType({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "moduleType"}, ...acc},
      )
    | list{ModuleAlias({id, name, docstrings, items}), ...rest} =>
      loop(
        list{...rest, ...List.fromArray(items)},
        list{{id, name, docstrings, kind: "moduleAlias"}, ...acc},
      )
    | list{} => acc
    }
  }

  items
  ->List.fromArray
  ->loop(list{})
  ->List.toArray
  ->Array.filter(({docstrings}) => Array.length(docstrings) > 0)
}

let getCodeBlocks = example => {
  let rec loopEndCodeBlock = (lines, acc) => {
    switch lines {
    | list{hd, ...rest} =>
      if (
        hd
        ->String.trim
        ->String.endsWith("```")
      ) {
        acc
      } else {
        loopEndCodeBlock(rest, list{hd, ...acc})
      }
    | list{} => panic(`Failed to find end of code block for ${example.kind}: ${example.id}`)
    }
  }

  let rec loop = (lines: list<string>, acc: list<string>) => {
    switch lines {
    | list{hd, ...rest} =>
      switch hd
      ->String.trim
      ->String.startsWith("```res") {
      | true =>
        let code = loopEndCodeBlock(rest, list{})
        loop(
          rest,
          list{
            code
            ->List.reverse
            ->List.toArray
            ->Array.join("\n"),
            ...acc,
          },
        )
      | false => loop(rest, acc)
      }
    | list{} => acc
    }
  }

  example.docstrings
  ->Array.reduce([], (acc, docstring) => acc->Array.concat(docstring->String.split("\n")))
  ->List.fromArray
  ->loop(list{})
  ->List.toArray
}

let main = async () => {
  let modules =
    Fs.readdirSync("runtime")
    ->Array.filter(f => f->String.endsWith(".resi") && !(f->String.startsWith("Belt")))
    ->Array.map(f => extractDocFromFile(Path.join(["runtime", f]))->getExamples)
    ->Array.flat

  let results =
    await modules
    ->Array.map(async example => {
      let id = example.id->String.replaceAll(".", "_")
      let codes = example->getCodeBlocks
      let results =
        await codes
        ->Array.mapWithIndex(async (code, int) => {
          let id = `${id}_${Int.toString(int)}`
          await testCode(~id, ~code)
        })
        ->Promise.all
      (example, results)
    })
    ->Promise.all

  let errors = results->Belt.Array.keepMap(((example, results)) => {
    let errors = results->Belt.Array.keepMap(result =>
      switch result {
      | Ok() => None
      | Error(msg) => Some(msg)
      }
    )

    if Array.length(errors) > 0 {
      Some((example, errors))
    } else {
      None
    }
  })

  // Print Errors
  let () = errors->Array.forEach(((test, errors)) => {
    let red = s => `\x1B[1;31m${s}\x1B[0m`
    let cyan = s => `\x1b[36m${s}\x1b[0m`
    let kind = switch test.kind {
    | "moduleAlias" => "module alias"
    | other => other
    }

    let errorMessage =
      errors
      ->Array.map(e => {
        // Drop line from path file
        e
        ->String.split("\n")
        ->Array.filterWithIndex((_, i) => i !== 2)
        ->Array.join("\n")
      })
      ->Array.join("\n")

    let message = `${"error"->red}: failed to compile examples from ${kind} ${test.id->cyan}\n${errorMessage}`

    Process.stderrWrite(message)
  })

  errors->Array.length == 0 ? 0 : 1
}

let exitCode = await main()

Process.exit(exitCode)
