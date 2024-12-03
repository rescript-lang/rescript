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
    @val @scope("process")
    external argv: array<string> = "argv"
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
    external tmpdir: unit => string = "tmpdir"
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
}

open Node

module Docgen = RescriptTools.Docgen

let bscBin = Path.join(["cli", "bsc"])

let options = Dict.fromArray([("ignore-runtime-tests", {Util.type_: "string"})])

let {Util.values: values} = Util.parseArgs({
  args: Process.argv->Array.sliceToEnd(~start=2),
  options,
})

let ignoreRuntimeTests = switch values->Dict.get("ignore-runtime-tests") {
| Some(v) =>
  v
  ->String.split(",")
  ->Array.map(s => s->String.trim)
| None => []
}

module SpawnAsync = {
  type t = {
    stdout: array<Buffer.t>,
    stderr: array<Buffer.t>,
    code: Null.t<float>,
  }
  let run = async (~command, ~args, ~options=?) => {
    await Promise.make((resolve, _reject) => {
      let spawn = ChildProcess.spawn(command, args, ~options?)
      let stdout = []
      let stderr = []
      spawn.stdout->ChildProcess.on("data", data => {
        Array.push(stdout, data)
      })
      spawn.stderr->ChildProcess.on("data", data => {
        Array.push(stderr, data)
      })
      spawn->ChildProcess.once("close", (code, _signal) => {
        resolve({stdout, stderr, code})
      })
    })
  }
}

type example = {
  id: string,
  kind: string,
  name: string,
  docstrings: array<string>,
}

let createFileInTempDir = id => Path.join2(OS.tmpdir(), id)

let compileTest = async (~id, ~code) => {
  let id = id->String.includes("/") ? String.replace(id, "/", "slash_op") : id
  let tempFileName = createFileInTempDir(id)

  let () = await Fs.writeFile(tempFileName ++ ".res", code)

  let args = [tempFileName ++ ".res", "-w", "-3-109"]

  let {stderr, stdout} = await SpawnAsync.run(~command=bscBin, ~args)

  switch Array.length(stderr) > 0 {
  | true =>
    stderr
    ->Array.map(e => e->Buffer.toString)
    ->Array.join("")
    ->Error
  | false =>
    stdout
    ->Array.map(e => e->Buffer.toString)
    ->Array.join("")
    ->Ok
  }
}

let runtimeTests = async code => {
  let {stdout, stderr, code: exitCode} = await SpawnAsync.run(
    ~command="node",
    ~args=["-e", code],
    ~options={
      cwd: Process.cwd(),
      timeout: 2000,
    },
  )

  // Some expressions, like, `console.error("error")` is printed to stderr but
  // exit code is 0
  let std = switch exitCode->Null.toOption {
  | Some(exitCode) if exitCode == 0.0 && Array.length(stderr) > 0 => stderr->Ok
  | Some(exitCode) if exitCode == 0.0 => stdout->Ok
  | None | Some(_) => Error(Array.length(stderr) > 0 ? stderr : stdout)
  }

  switch std {
  | Ok(buf) =>
    buf
    ->Array.map(e => e->Buffer.toString)
    ->Array.join("")
    ->Ok
  | Error(buf) =>
    buf
    ->Array.map(e => e->Buffer.toString)
    ->Array.join("")
    ->Error
  }
}

let indentOutputCode = code => {
  let indent = String.repeat(" ", 2)

  code
  ->String.split("\n")
  ->Array.map(s => `${indent}${s}`)
  ->Array.join("\n")
}

type error =
  | ReScript({error: string})
  | Runtime({rescript: string, js: string, error: string})

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
          (code, await compileTest(~id, ~code))
        })
        ->Promise.all
      (example, results)
    })
    ->Promise.all

  let examples = results->Array.map(((example, results)) => {
    let (compiled, errors) = results->Array.reduce(([], []), (acc, (resCode, result)) => {
      let (oks, errors) = acc
      switch result {
      | Ok(jsCode) => ([...oks, (resCode, jsCode)], errors)
      | Error(output) => (oks, [...errors, ReScript({error: output})])
      }
    })
    (example, (compiled, errors))
  })

  let exampleErrors =
    await examples
    ->Array.filter((({id}, _)) => !Array.includes(ignoreRuntimeTests, id))
    ->Array.map(async ((example, (compiled, errors))) => {
      let nodeTests =
        await compiled
        ->Array.map(async ((res, js)) => (res, js, await runtimeTests(js)))
        ->Promise.all

      let runtimeErrors = nodeTests->Belt.Array.keepMap(((res, js, output)) =>
        switch output {
        | Ok(_) => None
        | Error(error) => Some(Runtime({rescript: res, js, error}))
        }
      )

      (example, Array.concat(runtimeErrors, errors))
    })
    ->Promise.all

  // Print Errors
  let () = exampleErrors->Array.forEach(((example, errors)) => {
    let red = s => `\x1B[1;31m${s}\x1B[0m`
    let cyan = s => `\x1b[36m${s}\x1b[0m`
    let kind = switch example.kind {
    | "moduleAlias" => "module alias"
    | other => other
    }

    let errorMessage = errors->Array.map(err =>
      switch err {
      | ReScript({error}) =>
        let err =
          error
          ->String.split("\n")
          ->Array.filterWithIndex((_, i) => i !== 2)
          ->Array.join("\n")

        `${"error"->red}: failed to compile examples from ${kind} ${example.id->cyan}
${err}`
      | Runtime({rescript, js, error}) =>
        let indent = String.repeat(" ", 2)

        `${"runtime error"->red}: failed to run examples from ${kind} ${example.id->cyan}

${indent}${"ReScript"->cyan}

${rescript->indentOutputCode}

${indent}${"Compiled Js"->cyan}

${js->indentOutputCode}

${indent}${"stacktrace"->red}

${error->indentOutputCode}
`
      }
    )

    errorMessage->Array.forEach(e => Process.stderrWrite(e))
  })

  let someError = exampleErrors->Array.some(((_, err)) => Array.length(err) > 0)

  someError ? 1 : 0
}

let exitCode = await main()

Process.exit(exitCode)
