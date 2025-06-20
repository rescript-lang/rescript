/***
Generate API docs from ReScript Compiler

## Run

```bash
node scripts/res/GenApiDocs.res.js
```
*/
open Node
module Docgen = RescriptTools.Docgen

let packagePath = Path.join([Node.dirname, "..", "..", "package.json"])
let version = switch Fs.readFileSync(packagePath, ~encoding="utf8")->JSON.parseOrThrow {
  | Object(dict{"version": JSON.String(version)}) => version
  | _ => JsError.panic("Invalid package.json format")
}
let version = Semver.parse(version)->Option.getExn 
let version = Semver.toString({...version, preRelease: None}) // Remove pre-release identifiers for API docs
let dirVersion = Path.join([Node.dirname, "apiDocs", version])
if !Fs.existsSync(dirVersion) {
  Fs.mkdirSync(dirVersion)
}


let entryPointFiles = ["Belt.res", "Dom.res", "Js.res", "Stdlib.res"]

let hiddenModules = ["Js.Internal", "Js.MapperRt"]

type module_ = {
  id: string,
  docstrings: array<string>,
  name: string,
  items: array<Docgen.item>,
}

type section = {
  name: string,
  docstrings: array<string>,
  deprecated: option<string>,
  topLevelItems: array<Docgen.item>,
  submodules: array<module_>,
}

let env = Process.env

let docsDecoded = entryPointFiles->Array.map(libFile =>
  try {
    let entryPointFile = Path.join([Node.dirname, "..", "..", "runtime", libFile])

    let rescriptToolsPath = Path.join([Node.dirname, "..", "..", "cli", "rescript-tools.js"])
    let output = ChildProcess.execSync(
      `${rescriptToolsPath} doc ${entryPointFile}`,
      ~options={
        maxBuffer: 30_000_000.,
      },
    )->Buffer.toString

    let docs = output
    ->JSON.parseOrThrow
    ->Docgen.decodeFromJson
    Console.log(`Generated docs from ${libFile}`)
    docs
  } catch {
  | JsExn(exn) =>
    Console.error(
      `Error while generating docs from ${libFile}: ${exn
        ->JsExn.message
        ->Option.getOr("[no message]")}`,
    )
    JsExn.throw(exn)
  }
)

let removeStdlibOrPrimitive = s => s->String.replaceAllRegExp(/Stdlib_|Primitive_js_extern\./g, "")

let docs = docsDecoded->Array.map(doc => {
  let topLevelItems = doc.items->Array.filterMap(item =>
    switch item {
    | Value(_) as item | Type(_) as item => item->Some
    | _ => None
    }
  )

  let rec getModules = (lst: list<Docgen.item>, moduleNames: list<module_>) =>
    switch lst {
    | list{
        Module({id, items, name, docstrings})
        | ModuleAlias({id, items, name, docstrings})
        | ModuleType({id, items, name, docstrings}),
        ...rest,
      } =>
      if Array.includes(hiddenModules, id) {
        getModules(rest, moduleNames)
      } else {
        getModules(
          list{...rest, ...List.fromArray(items)},
          list{{id, items, name, docstrings}, ...moduleNames},
        )
      }
    | list{Type(_) | Value(_), ...rest} => getModules(rest, moduleNames)
    | list{} => moduleNames
    }

  let id = doc.name

  let top = {id, name: id, docstrings: doc.docstrings, items: topLevelItems}
  let submodules = getModules(doc.items->List.fromArray, list{})->List.toArray
  let result = [top]->Array.concat(submodules)

  (id, result)
})

let allModules = {
  open JSON
  let encodeItem = (docItem: Docgen.item) => {
    switch docItem {
    | Value({id, name, docstrings, signature, ?deprecated}) => {
        let dict = Dict.fromArray(
          [
            ("id", id->String),
            ("kind", "value"->String),
            ("name", name->String),
            (
              "docstrings",
              docstrings
              ->Array.map(s => s->removeStdlibOrPrimitive->String)
              ->Array,
            ),
            (
              "signature",
              signature
              ->removeStdlibOrPrimitive
              ->String,
            ),
          ]->Array.concat(
            switch deprecated {
            | Some(v) => [("deprecated", v->String)]
            | None => []
            },
          ),
        )
        dict->Object->Some
      }

    | Type({id, name, docstrings, signature, ?deprecated}) =>
      let dict = Dict.fromArray(
        [
          ("id", id->String),
          ("kind", "type"->String),
          ("name", name->String),
          ("docstrings", docstrings->Array.map(s => s->removeStdlibOrPrimitive->String)->Array),
          ("signature", signature->removeStdlibOrPrimitive->String),
        ]->Array.concat(
          switch deprecated {
          | Some(v) => [("deprecated", v->String)]
          | None => []
          },
        ),
      )
      Object(dict)->Some

    | _ => None
    }
  }

  docs->Array.map(((topLevelName, modules)) => {
    let submodules =
      modules
      ->Array.map(mod => {
        let items =
          mod.items
          ->Array.filterMap(item => encodeItem(item))
          ->Array

        let rest = Dict.fromArray([
          ("id", mod.id->String),
          ("name", mod.name->String),
          ("docstrings", mod.docstrings->Array.map(s => s->String)->Array),
          ("items", items),
        ])
        (
          mod.id
          ->String.split(".")
          ->Array.join("/")
          ->String.toLowerCase,
          rest->Object,
        )
      })
      ->Dict.fromArray

    (topLevelName, submodules)
  })
}

let () = {
  allModules->Array.forEach(((topLevelName, mod)) => {
    let json = JSON.Object(mod)

    Fs.writeFileSync(
      Path.join([dirVersion, `${topLevelName->String.toLowerCase}.json`]),
      json->JSON.stringify(~space=2),
    )
  })
}

type rec node = {
  name: string,
  path: array<string>,
  children: array<node>,
}

// Generate TOC modules
let () = {
  let joinPath = (~path: array<string>, ~name: string) => {
    Array.concat(path, [name])->Array.map(path => path->String.toLowerCase)
  }
  let rec getModules = (lst: list<Docgen.item>, moduleNames, path) => {
    switch lst {
    | list{
        Module({id, items, name}) | ModuleAlias({id, items, name}) | ModuleType({id, items, name}),
        ...rest,
      } =>
      if Array.includes(hiddenModules, id) {
        getModules(rest, moduleNames, path)
      } else {
        let itemsList = items->List.fromArray
        let children = getModules(itemsList, [], joinPath(~path, ~name))

        getModules(
          rest,
          Array.concat([{name, path: joinPath(~path, ~name), children}], moduleNames),
          path,
        )
      }
    | list{Type(_) | Value(_), ...rest} => getModules(rest, moduleNames, path)
    | list{} => moduleNames
    }
  }

  let tocTree = docsDecoded->Array.map(({name, items}) => {
    let path = name->String.toLowerCase
    (
      path,
      {
        name,
        path: [path],
        children: items
        ->List.fromArray
        ->getModules([], [path]),
      },
    )
  })

  Fs.writeFileSync(
    Path.join([dirVersion, "toc_tree.json"]),
    tocTree
    ->Dict.fromArray
    ->JSON.stringifyAny
    ->Option.getExn,
  )
  Console.log("Generated toc_tree.json")
  Console.log(`API docs generated successfully in ${dirVersion}`)
}
