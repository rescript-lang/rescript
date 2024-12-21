module Node = {
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
      spawn->ChildProcess.onFromSpawn("close", code => {
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

let compileTest = async (~id as _, ~code) => {
  let {stderr, stdout} = await SpawnAsync.run(
    ~command=bscBin,
    // NOTE: warnings argument (-w) should be before eval (-e) argument
    ~args=["-w", "-3-109-44", "-e", code],
  )

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
    ~args=["-e", code, "--input-type", "commonjs"],
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
  | ReScriptError(string)
  | RuntimeError({rescript: string, js: string, error: string})

let extractDocFromFile = file => {
  let toolsBin = Path.join([Process.cwd(), "cli", "rescript-tools"])
  let spawn = ChildProcess.spawnSync(toolsBin, ["doc", file])

  let output = spawn.stdout->Buffer.toString

  try {
    output
    ->JSON.parseExn
    ->Docgen.decodeFromJson
  } catch {
  | Exn.Error(_) => Error.panic(`Failed to generate docstrings from ${file}`)
  | _ => assert(false)
  }
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
  ->Belt.Array.reverse
  ->Array.join("\n\n")
}

let chunkArray = (array, chunkSize) => {
  let result = []

  let rec loop = (i: int) => {
    if i < Array.length(array) {
      Array.push(
        result,
        Array.slice(array, ~start=i, ~end=Math.Int.min(i + chunkSize, Array.length(array))),
      )
      loop(i + chunkSize)
    }
  }

  loop(0)
  result
}

let main = async () => {
  let files = Fs.readdirSync("runtime")

  let modules =
    files
    // Ignore Js modules and RescriptTools for now
    ->Array.filter(f => !String.startsWith(f, "Js") && !String.startsWith(f, "RescriptTools"))
    ->Array.filter(f => f->String.endsWith(".res") || f->String.endsWith(".resi"))
    // ->Array.filter(f => f === "Uint8ClampedArray.res")
    ->Array.reduce([], (acc, cur) => {
      let isInterface = cur->String.endsWith(".resi")

      let resi = Path.join2("runtime", cur ++ "i")

      // If .resi files exists append to array
      if !isInterface && Fs.existsSync(resi) {
        Array.concat(acc, [cur ++ "i"])
      } else if !Array.includes(acc, cur) {
        Array.concat(acc, [cur])
      } else {
        acc
      }
    })
    ->Array.map(f => extractDocFromFile(Path.join(["runtime", f]))->getExamples)
    ->Array.flat

  let batchSize = OS.cpus()->Array.length * 2

  // Console.log2("module length", modules->Array.length)
  let chuncks = modules->chunkArray(batchSize)

  // Console.log2("chuncks length", chuncks->Array.length)
  // Console.log2("chunck part size", chuncks->Array.map(f => f->Array.length))

  let context = ref(0)

  let asyncIterator = AsyncIterator.make(async () => {
    let currentValue = context.contents
    // Increment current value
    context := currentValue + 1

    {
      AsyncIterator.value: Some(currentValue),
      done: currentValue == Array.length(chuncks) - 1,
    }
  })

  let result = []
  // await asyncIterator->AsyncIterator.forEach(async value =>
  //   switch value {
  //   | Some(value) =>
  //     let c = Array.getUnsafe(chuncks, value)
  //
  //     let a = await c->Array.map(async example => {
  //       let id = example.id->String.replaceAll(".", "__")
  //       let rescriptCode = example->getCodeBlocks
  //       let jsCode = await compileTest(~id, ~code=rescriptCode)
  //       (example, (rescriptCode, jsCode))
  //     })->Promise.all
  //
  //     Array.push(result, a)->ignore
  //
  //   | None => ()
  //   }
  // )

  let processMyAsyncIterator = async () => {
    // ReScript doesn't have `for ... of` loops, but it's easy to mimic using a while loop.
    let break = ref(false)

    while !break.contents {
      // Await the next iterator value
      let {value, done} = await asyncIterator->AsyncIterator.next

      // Exit the while loop if the iterator says it's done
      break := done

      // This will log the (int) value of the current async iteration, if a value was returned.
      switch value {
      | Some(index) =>
        let c = Array.getUnsafe(chuncks, index)
        let a =
          await c
          ->Array.map(async example => {
            let id = example.id->String.replaceAll(".", "__")
            let rescriptCode = example->getCodeBlocks
            let jsCode = await compileTest(~id, ~code=rescriptCode)
            (example, (rescriptCode, jsCode))
          })
          ->Promise.all

        Array.push(result, a)

      | None => ()
      }
    }
  }

  let () = await processMyAsyncIterator()

  let compilationResults = result->Array.flat

  // let compilationResults =
  //   (await chuncks
  //   ->Array.map(async arrExample => {
  //     await arrExample
  //     ->Array.map(async example => {
  //       let id = example.id->String.replaceAll(".", "__")
  //       let rescriptCode = example->getCodeBlocks
  //       let jsCode = await compileTest(~id, ~code=rescriptCode)
  //       (example, (rescriptCode, jsCode))
  //     })
  //     ->Promise.all
  //   })
  //   ->Promise.all)
  //   ->Array.flat

  // let compilationResults =
  //   await modules
  //   ->Array.map(async example => {
  //     let id = example.id->String.replaceAll(".", "__")
  //     let rescriptCode = example->getCodeBlocks
  //     let jsCode = await compileTest(~id, ~code=rescriptCode)
  //     (example, (rescriptCode, jsCode))
  //   })
  //   ->Promise.all

  let (compiled, compilationErrors) = compilationResults->Array.reduce(([], []), (
    acc,
    (example, (rescriptCode, jsCode)),
  ) => {
    let (lhs, rhs) = acc
    switch jsCode {
    | Ok(jsCode) => lhs->Array.push((example, rescriptCode, jsCode))
    | Error(err) => rhs->Array.push((example, ReScriptError(err)))
    }
    (lhs, rhs)
  })

  let _batches = chunkArray(compiled, batchSize)

  // let a =
  //   await batches
  //   ->Array.map(async t => {
  //     (await t
  //     ->Array.filter((({id}, _, _)) => !Array.includes(ignoreRuntimeTests, id))
  //     ->Array.map(async ((example, rescriptCode, jsCode)) => {
  //       let nodeTest = await runtimeTests(jsCode)
  //       switch nodeTest {
  //       | Ok(_) => None
  //       | Error(error) => Some(example, RuntimeError({rescript: rescriptCode, js: jsCode, error}))
  //       }
  //     })
  //     ->Promise.all)
  //     ->Array.filterMap(i =>
  //       switch i {
  //       | Some(i) => Some(i)
  //       | None => None
  //       }
  //     )
  //   })
  //   ->Promise.all

  let runtimeErrors = []
  // let runtimeErrors = Array.flat(a)

  // let runtimeErrors =
  //   (await compiled
  //   ->Array.filter((({id}, _, _)) => !Array.includes(ignoreRuntimeTests, id))
  //   ->Array.map(async ((example, rescriptCode, jsCode)) => {
  //     let nodeTests = await jsCode->runtimeTests
  //     switch nodeTests {
  //     | Ok(_) => None
  //     | Error(error) => Some(example, RuntimeError({rescript: rescriptCode, js: jsCode, error}))
  //     }
  //   })
  //   ->Promise.all)
  //   ->Array.filterMap(i =>
  //     switch i {
  //     | Some(i) => Some(i)
  //     | None => None
  //     }
  //   )

  let allErrors = Array.concat(runtimeErrors, compilationErrors)

  // Print Errors
  let () = allErrors->Array.forEach(((example, errors)) => {
    let red = s => `\x1B[1;31m${s}\x1B[0m`
    let cyan = s => `\x1b[36m${s}\x1b[0m`
    let kind = switch example.kind {
    | "moduleAlias" => "module alias"
    | other => other
    }

    let a = switch errors {
    | ReScriptError(error) =>
      let err =
        error
        ->String.split("\n")
        // Drop line of filename
        ->Array.filterWithIndex((_, i) => i !== 2)
        ->Array.join("\n")

      `${"error"->red}: failed to compile examples from ${kind} ${example.id->cyan}
${err}`
    | RuntimeError({rescript, js, error}) =>
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

    Process.stderrWrite(a)
  })

  let someError = allErrors->Array.length > 0

  someError ? 1 : 0
}

let exitCode = await main()

Process.exit(exitCode)
