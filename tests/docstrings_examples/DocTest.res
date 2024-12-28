open Node

module Docgen = RescriptTools.Docgen

type example = {
  id: string,
  kind: string,
  name: string,
  docstrings: array<string>,
}

// Ignore some tests not supported by node v18
let ignoreRuntimeTests = [
  "Array.toReversed",
  "Array.toSorted",
  "Promise.withResolvers",
  "Set.union",
  "Set.isSupersetOf",
  "Set.isSubsetOf",
  "Set.isDisjointFrom",
  "Set.intersection",
  "Set.symmetricDifference",
  "Set.difference",
]

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

let main = async () => {
  let examples = await extractExamples()
  examples->Array.sort((a, b) =>
    String.length(a.id) > String.length(b.id) ? Ordering.fromInt(1) : Ordering.fromInt(-1)
  )
  let testsContent =
    examples
    ->Array.filterMap(example => {
      let codeExamples = getCodeBlocks(example)

      let ignore = Array.includes(ignoreRuntimeTests, example.id)

      switch String.length(codeExamples) == 0 {
      | true => None
      | false =>
        ignore
          ? None
          : Some(
              `describe("${example.id}", () => {
  test("${example.id}", () => {
    module Test = {
      ${codeExamples}
    }
    ()
  })
})`,
            )
      }
    })
    ->Array.join("\n\n")

  let dirname = url->URL.fileURLToPath->Path.dirname
  let filepath = Path.join([dirname, "generated_mocha_test.res"])
  let fileContent = `open Mocha
@@warning("-32-34-60-37-109-3-44")

${testsContent}`

  await Fs.writeFile(filepath, fileContent)
}

let () = await main()
