// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Os from "os";
import * as Exn from "rescript/lib/es6/Exn.js";
import * as Url from "url";
import * as List from "rescript/lib/es6/List.js";
import * as Path from "path";
import * as $$Array from "rescript/lib/es6/Array.js";
import * as $$Error from "rescript/lib/es6/Error.js";
import * as Ordering from "rescript/lib/es6/Ordering.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as ArrayUtils from "./ArrayUtils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as SpawnAsync from "./SpawnAsync.res.mjs";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Promises from "node:fs/promises";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";
import * as RescriptTools_Docgen from "rescript/lib/es6/RescriptTools_Docgen.js";

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
  "Set.difference"
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
    if (exn.RE_EXN_ID === Exn.$$Error) {
      return $$Error.panic("Failed to generate docstrings from " + file);
    }
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "DocTest.res",
        43,
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
      if (!items) {
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
            List.fromArray(match.items)
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
            List.fromArray(match.items)
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
            List.fromArray(match.items)
          ]);
          continue;
      }
    };
  };
  return List.toArray(loop(List.fromArray(param.items), /* [] */0)).filter(param => param.docstrings.length > 0);
}

function getCodeBlocks(example) {
  let loopEndCodeBlock = (_lines, _acc) => {
    while (true) {
      let acc = _acc;
      let lines = _lines;
      if (!lines) {
        return Pervasives.panic("Failed to find end of code block for " + example.kind + ": " + example.id);
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
      if (!lines) {
        return acc;
      }
      let rest = lines.tl;
      if (lines.hd.trim().startsWith("```res")) {
        let code = loopEndCodeBlock(rest, /* [] */0);
        _acc = {
          hd: List.toArray(List.reverse(code)).join("\n"),
          tl: acc
        };
        _lines = rest;
        continue;
      }
      _lines = rest;
      continue;
    };
  };
  return Belt_Array.reverse(List.toArray(loop(List.fromArray($$Array.reduce(example.docstrings, [], (acc, docstring) => acc.concat(docstring.split("\n")))), /* [] */0))).join("\n\n");
}

let batchSize = Os.cpus().length;

async function extractExamples() {
  let files = Fs.readdirSync("runtime");
  let docFiles = files.filter(f => {
    if (f.startsWith("Js") || f.startsWith("RescriptTools")) {
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
  return examples;
}

async function main() {
  let examples = await extractExamples();
  examples.sort((a, b) => {
    if (Primitive_object.greaterthan(a.id, b.id)) {
      return Ordering.fromInt(1);
    } else {
      return Ordering.fromInt(-1);
    }
  });
  let testsContent = $$Array.filterMap(examples, example => {
    let codeExamples = getCodeBlocks(example);
    let ignore = ignoreRuntimeTests.includes(example.id);
    if (codeExamples.length === 0 || ignore) {
      return;
    } else {
      return "describe(\"" + example.id + "\", () => {\n  test(\"" + example.id + "\", () => {\n    module Test = {\n      " + codeExamples + "\n    }\n    ()\n  })\n})";
    }
  }).join("\n\n");
  let dirname = Path.dirname(Url.fileURLToPath(import.meta.url));
  let filepath = Path.join(dirname, "generated_mocha_test.res");
  let fileContent = "open Mocha\n@@warning(\"-32-34-60-37-109-3-44\")\n\n" + testsContent;
  return await Promises.writeFile(filepath, fileContent);
}

await main();

/* batchSize Not a pure module */
