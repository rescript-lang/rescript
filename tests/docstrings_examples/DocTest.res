open Node

module Docgen = RescriptTools.Docgen

type example = {
  id: string,
  kind: string,
  name: string,
  docstrings: array<string>,
}

type error =
  | ReScriptError(string)
  | RuntimeError({rescript: string, js: string, error: string})

let bscBin = Path.join(["cli", "bsc"])

let getOutput = buffer =>
  buffer
  ->Array.map(e => e->Buffer.toString)
  ->Array.join("")

let extractDocFromFile = async file => {
  let toolsBin = Path.join([Process.cwd(), "cli", "rescript-tools"])

  let {stdout} = await SpawnAsync.run(~command=toolsBin, ~args=["doc", file])

  try {
    stdout
    ->getOutput
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

let batchSize = OS.cpus()->Array.length

let extractExamples = async () => {
  let files = Fs.readdirSync("runtime")

  let docFiles = files->Array.filter(f =>
    switch f {
    // Ignore Js modules and RescriptTools for now
    | f if f->String.startsWith("Js") || f->String.startsWith("RescriptTools") => false
    | f if f->String.endsWith(".resi") => true
    | f if f->String.endsWith(".res") && !(files->Array.includes(f ++ "i")) => true
    | _ => false
    }
  )

  Console.log(`Extracting examples from ${docFiles->Array.length->Int.toString} runtime files...`)

  let examples = []
  await docFiles->ArrayUtils.forEachAsyncInBatches(~batchSize, async f => {
    let doc = await extractDocFromFile(Path.join(["runtime", f]))
    examples->Array.pushMany(doc->getExamples)
  })

  examples
}

let compileTest = async (~code) => {
  // NOTE: warnings argument (-w) should be before eval (-e) argument
  let args = ["-w", "-3-109-44", "-e", code]
  let {stderr, stdout} = await SpawnAsync.run(~command=bscBin, ~args)

  stderr->Array.length > 0 ? Error(stderr->getOutput) : Ok(stdout->getOutput)
}

let compileExamples = async examples => {
  Console.log(`Compiling ${examples->Array.length->Int.toString} examples from docstrings...`)

  let compiled = []
  let compilationErrors = []

  await examples->ArrayUtils.forEachAsyncInBatches(~batchSize, async example => {
    // let id = example.id->String.replaceAll(".", "__")
    let rescriptCode = example->getCodeBlocks

    switch await compileTest(~code=rescriptCode) {
    | Ok(jsCode) => compiled->Array.push((example, rescriptCode, jsCode))
    | Error(err) => compilationErrors->Array.push((example, ReScriptError(err)))
    }
  })

  (compiled, compilationErrors)
}

let runtimeTests = async code => {
  let {stdout, stderr, code: exitCode} = await SpawnAsync.run(
    ~command="node",
    ~args=["-e", code, "--input-type", "commonjs"],
    ~options={cwd: Process.cwd(), timeout: 2000},
  )

  // Some expressions, like `console.error("error")` are printed to stderr,
  // but exit code is 0
  let std = switch exitCode->Null.toOption {
  | Some(exitCode) if exitCode == 0.0 && Array.length(stderr) > 0 => stderr->Ok
  | Some(exitCode) if exitCode == 0.0 => stdout->Ok
  | None | Some(_) => Error(Array.length(stderr) > 0 ? stderr : stdout)
  }

  switch std {
  | Ok(buf) => Ok(buf->getOutput)
  | Error(buf) => Error(buf->getOutput)
  }
}

let runExamples = async compiled => {
  Console.log(`Running ${compiled->Array.length->Int.toString} compiled examples...`)

  let runtimeErrors = []
  await compiled->ArrayUtils.forEachAsyncInBatches(~batchSize, async compiled => {
    let (example, rescriptCode, jsCode) = compiled

    switch await runtimeTests(jsCode) {
    | Ok(_) => ()
    | Error(error) =>
      let runtimeError = RuntimeError({rescript: rescriptCode, js: jsCode, error})
      runtimeErrors->Array.push((example, runtimeError))
    }
  })

  runtimeErrors
}

let indentOutputCode = code => {
  let indent = String.repeat(" ", 2)

  code
  ->String.split("\n")
  ->Array.map(s => `${indent}${s}`)
  ->Array.join("\n")
}

let printErrors = errors => {
  errors->Array.forEach(((example, errors)) => {
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
}

let main = async () => {
  let examples = await extractExamples()
  let (compiled, compilationErrors) = await compileExamples(examples)
  let runtimeErrors = await runExamples(compiled)

  let allErrors = Array.concat(runtimeErrors, compilationErrors)

  if allErrors->Array.length > 0 {
    printErrors(allErrors)
    1
  } else {
    Console.log("All examples passed successfully")
    0
  }
}

let exitCode = await main()

Process.exit(exitCode)
