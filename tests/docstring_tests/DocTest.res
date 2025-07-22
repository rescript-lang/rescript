open Node

// Only major version
let nodeVersion =
  Process.version
  ->String.replace("v", "")
  ->String.split(".")
  ->Array.get(0)
  ->Option.getOrThrow(~message="Failed to find major version of Node")
  ->Int.fromString
  ->Option.getOrThrow(~message="Failed to convert node version to Int")

let ignoreRuntimeTests = [
  (
    // Ignore tests that require Node.js v20+
    20,
    ["Stdlib_Array.toReversed", "Stdlib_Array.toSorted"],
  ),
  (
    // Ignore tests that require Node.js v22+
    22,
    [
      "Stdlib_Promise.withResolvers",
      "Stdlib_Set.union",
      "Stdlib_Set.isSupersetOf",
      "Stdlib_Set.isSubsetOf",
      "Stdlib_Set.isDisjointFrom",
      "Stdlib_Set.intersection",
      "Stdlib_Set.symmetricDifference",
      "Stdlib_Set.difference",
    ],
  ),
]

let getOutput = buffer =>
  buffer
  ->Array.map(e => e->Buffer.toString)
  ->Array.join("")

let extractDocFromFile = async file => {
  let toolsBin = Path.join([Process.cwd(), "cli", "rescript-tools.js"])

  let {stdout} = await SpawnAsync.run(
    ~command=toolsBin,
    ~args=["extract-codeblocks", file, "--transform-assert-equal"],
  )

  try {
    stdout
    ->getOutput
    ->JSON.parseOrThrow
    ->RescriptTools.ExtractCodeBlocks.decodeFromJson
  } catch {
  | JsExn(e) =>
    Console.error(e)
    JsError.panic(`Failed to extract code blocks from ${file}`)
  }
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
    switch doc {
    | Ok(doc) =>
      // TODO: Should this be a flag in the actual command instead, to only include code blocks with tests?
      examples->Array.pushMany(doc->Array.filter(d => d.code->String.includes("assertEqual(")))
    | Error(e) => Console.error(e)
    }
  })

  examples->Array.sort((a, b) => String.compare(a.id, b.id))
  examples
}

let main = async () => {
  let examples = await extractExamples()

  let dict = dict{}

  examples->Array.forEach(cur => {
    let modulePath = cur.id->String.split(".")

    let id =
      modulePath
      ->Array.slice(~start=0, ~end=Array.length(modulePath) - 1)
      ->Array.join(".")

    let previous = switch dict->Dict.get(id) {
    | Some(p) => p
    | None => []
    }

    dict->Dict.set(id, Array.concat([cur], previous))
  })

  let output = []

  dict->Dict.forEachWithKey((examples, key) => {
    examples->Array.sort((a, b) => String.compare(a.name, b.name))

    let codeExamples = examples->Array.filterMap(example => {
      let ignoreExample =
        ignoreRuntimeTests->Array.some(
          ((version, tests)) => nodeVersion < version && tests->Array.includes(example.id),
        )

      if ignoreExample {
        Console.warn(
          `Ignoring ${example.id} tests. Not supported by Node ${nodeVersion->Int.toString}`,
        )
        None
      } else {
        let code = example.code

        if code->String.length === 0 {
          None
        } else {
          // Let's add the examples inside a Test module because some examples
          // have type definitions that are not supported inside a block.
          // Also add unit type `()`
          Some(
            `test("${example.name}", () => {
  module Test = {
    ${code}
  }
  ()
})`,
          )
        }
      }
    })

    if codeExamples->Array.length > 0 {
      let content = `describe("${key}", () => {
${codeExamples->Array.join("\n")}
 })`
      output->Array.push(content)
    }
  })

  let dirname = url->URL.fileURLToPath->Path.dirname
  let filepath = Path.join([dirname, "generated_mocha_test.res"])
  let fileContent = `open Mocha
@@warning("-32-34-60-37-109-3-44")

${output->Array.join("\n")}`

  await Fs.writeFile(filepath, fileContent)
}

let () = await main()
