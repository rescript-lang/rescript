// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Os from "os";
import * as Url from "url";
import * as Path from "path";
import * as Stdlib from "rescript/lib/es6/Stdlib.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as ArrayUtils from "./ArrayUtils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as SpawnAsync from "./SpawnAsync.res.mjs";
import * as Stdlib_Exn from "rescript/lib/es6/Stdlib_Exn.js";
import * as Stdlib_Int from "rescript/lib/es6/Stdlib_Int.js";
import * as Stdlib_Dict from "rescript/lib/es6/Stdlib_Dict.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Stdlib_Array from "rescript/lib/es6/Stdlib_Array.js";
import * as Stdlib_Error from "rescript/lib/es6/Stdlib_Error.js";
import * as Stdlib_Option from "rescript/lib/es6/Stdlib_Option.js";
import * as Primitive_string from "rescript/lib/es6/Primitive_string.js";
import * as Promises from "node:fs/promises";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";
import * as RescriptTools_Docgen from "rescript/lib/es6/RescriptTools_Docgen.js";

let nodeVersion = Stdlib_Option.getExn(Stdlib_Int.fromString(Stdlib_Option.getExn(process.version.replace("v", "").split(".")[0], "Failed to find major version of Node"), undefined), "Failed to convert node version to Int");

let ignoreRuntimeTests = [
  [
    20,
    [
      "Stdlib.Array.toReversed",
      "Stdlib.Array.toSorted"
    ]
  ],
  [
    22,
    [
      "Stdlib.Promise.withResolvers",
      "Stdlib.Set.union",
      "Stdlib.Set.isSupersetOf",
      "Stdlib.Set.isSubsetOf",
      "Stdlib.Set.isDisjointFrom",
      "Stdlib.Set.intersection",
      "Stdlib.Set.symmetricDifference",
      "Stdlib.Set.difference"
    ]
  ]
];

function getOutput(buffer) {
  return buffer.map(e => e.toString()).join("");
}

async function extractDocFromFile(file) {
  let toolsBin = Path.join(process.cwd(), "cli", "rescript-tools");
  let match = await SpawnAsync.run(toolsBin, [
    "doc",
    file
  ], undefined);
  try {
    return RescriptTools_Docgen.decodeFromJson(JSON.parse(getOutput(match.stdout)));
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === Stdlib_Exn.$$Error) {
      return Stdlib_Error.panic("Failed to generate docstrings from " + file);
    }
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "DocTest.res",
        61,
        9
      ],
      Error: new Error()
    };
  }
}

function getExamples(param) {
  let loop = (_items, _acc) => {
    while (true) {
      let acc = _acc;
      let items = _items;
      if (items === 0) {
        return acc;
      }
      let match = items.hd;
      switch (match.kind) {
        case "value" :
          _acc = {
            hd: {
              id: match.id,
              kind: "value",
              name: match.name,
              docstrings: match.docstrings
            },
            tl: acc
          };
          _items = items.tl;
          continue;
        case "type" :
          _acc = {
            hd: {
              id: match.id,
              kind: "type",
              name: match.name,
              docstrings: match.docstrings
            },
            tl: acc
          };
          _items = items.tl;
          continue;
        case "module" :
          _acc = {
            hd: {
              id: match.id,
              kind: "module",
              name: match.name,
              docstrings: match.docstrings
            },
            tl: acc
          };
          _items = Belt_List.concatMany([
            items.tl,
            Stdlib_List.fromArray(match.items)
          ]);
          continue;
        case "moduleType" :
          _acc = {
            hd: {
              id: match.id,
              kind: "moduleType",
              name: match.name,
              docstrings: match.docstrings
            },
            tl: acc
          };
          _items = Belt_List.concatMany([
            items.tl,
            Stdlib_List.fromArray(match.items)
          ]);
          continue;
        case "moduleAlias" :
          _acc = {
            hd: {
              id: match.id,
              kind: "moduleAlias",
              name: match.name,
              docstrings: match.docstrings
            },
            tl: acc
          };
          _items = Belt_List.concatMany([
            items.tl,
            Stdlib_List.fromArray(match.items)
          ]);
          continue;
      }
    };
  };
  return Stdlib_List.toArray(loop(Stdlib_List.fromArray(param.items), /* [] */0)).filter(param => param.docstrings.length > 0);
}

function getCodeBlocks(example) {
  let loopEndCodeBlock = (_lines, _acc) => {
    while (true) {
      let acc = _acc;
      let lines = _lines;
      if (lines === 0) {
        return Stdlib.panic("Failed to find end of code block for " + example.kind + ": " + example.id);
      }
      let hd = lines.hd;
      if (hd.trim().endsWith("```")) {
        return acc;
      }
      _acc = {
        hd: hd,
        tl: acc
      };
      _lines = lines.tl;
      continue;
    };
  };
  let loop = (_lines, _acc) => {
    while (true) {
      let acc = _acc;
      let lines = _lines;
      if (lines === 0) {
        return acc;
      }
      let rest = lines.tl;
      if (lines.hd.trim().startsWith("```res")) {
        let code = loopEndCodeBlock(rest, /* [] */0);
        _acc = {
          hd: Stdlib_List.toArray(Stdlib_List.reverse(code)).join("\n"),
          tl: acc
        };
        _lines = rest;
        continue;
      }
      _lines = rest;
      continue;
    };
  };
  return Belt_Array.reverse(Stdlib_List.toArray(loop(Stdlib_List.fromArray(Stdlib_Array.reduce(example.docstrings, [], (acc, docstring) => acc.concat(docstring.split("\n")))), /* [] */0))).join("\n\n");
}

let batchSize = Os.cpus().length;

async function extractExamples() {
  let files = Fs.readdirSync("runtime");
  let docFiles = files.filter(f => {
    if (f.startsWith("Js") || f.startsWith("RescriptTools") || f.startsWith("Stdlib_")) {
      return false;
    } else if (f.endsWith(".resi")) {
      return true;
    } else if (f.endsWith(".res")) {
      return !files.includes(f + "i");
    } else {
      return false;
    }
  });
  console.log("Extracting examples from " + docFiles.length.toString() + " runtime files...");
  let examples = [];
  await ArrayUtils.forEachAsyncInBatches(docFiles, batchSize, async f => {
    let doc = await extractDocFromFile(Path.join("runtime", f));
    examples.push(...getExamples(doc));
  });
  examples.sort((a, b) => Primitive_string.compare(a.id, b.id));
  return examples;
}

async function main() {
  let examples = await extractExamples();
  let dict = {};
  examples.forEach(cur => {
    let modulePath = cur.id.split(".");
    let id = modulePath.slice(0, modulePath.length - 1 | 0).join(".");
    let p = dict[id];
    let previous = p !== undefined ? p : [];
    dict[id] = [cur].concat(previous);
  });
  let output = [];
  Stdlib_Dict.forEachWithKey(dict, (examples, key) => {
    examples.sort((a, b) => Primitive_string.compare(a.name, b.name));
    let codeExamples = Stdlib_Array.filterMap(examples, example => {
      let ignoreExample = ignoreRuntimeTests.some(param => {
        if (nodeVersion < param[0]) {
          return param[1].includes(example.id);
        } else {
          return false;
        }
      });
      if (ignoreExample) {
        console.warn("Ignoring " + example.id + " tests. Not supported by Node " + nodeVersion.toString());
        return;
      }
      let code = getCodeBlocks(example);
      if (code.length === 0) {
        return;
      } else {
        return "test(\"" + example.name + "\", () => {\n  module Test = {\n    " + code + "\n  }\n  ()\n})";
      }
    });
    if (codeExamples.length <= 0) {
      return;
    }
    let content = "describe(\"" + key + "\", () => {\n" + codeExamples.join("\n") + "\n })";
    output.push(content);
  });
  let dirname = Path.dirname(Url.fileURLToPath(import.meta.url));
  let filepath = Path.join(dirname, "generated_mocha_test.res");
  let fileContent = "open Mocha\n@@warning(\"-32-34-60-37-109-3-44\")\n\n" + output.join("\n");
  return await Promises.writeFile(filepath, fileContent);
}

await main();

/* nodeVersion Not a pure module */
