// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Os from "os";
import * as Exn from "rescript/lib/es6/Exn.js";
import * as List from "rescript/lib/es6/List.js";
import * as Path from "path";
import * as $$Array from "rescript/lib/es6/Array.js";
import * as $$Error from "rescript/lib/es6/Error.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Nodeutil from "node:util";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as $$AsyncIterator from "rescript/lib/es6/AsyncIterator.js";
import * as Child_process from "child_process";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";
import * as RescriptTools_Docgen from "rescript/lib/es6/RescriptTools_Docgen.js";

let Path$1 = {};

let Process = {};

let Fs$1 = {};

let Buffer = {};

let ChildProcess = {};

let OS = {};

let Util = {};

let Node = {
  Path: Path$1,
  Process: Process,
  Fs: Fs$1,
  Buffer: Buffer,
  ChildProcess: ChildProcess,
  OS: OS,
  Util: Util
};

let bscBin = Path.join("cli", "bsc");

let options = Object.fromEntries([[
    "ignore-runtime-tests",
    {
      type: "string"
    }
  ]]);

let match = Nodeutil.parseArgs({
  args: process.argv.slice(2),
  options: options
});

let values = match.values;

let v = values["ignore-runtime-tests"];

let ignoreRuntimeTests = v !== undefined ? v.split(",").map(s => s.trim()) : [];

async function run(command, args, options) {
  let spawn = Child_process.spawn(command, args, options !== undefined ? Primitive_option.valFromOption(options) : undefined);
  let stdout = [];
  let stderr = [];
  spawn.stdout.on("data", data => {
    stdout.push(data);
  });
  spawn.stderr.on("data", data => {
    stderr.push(data);
  });
  return await new Promise((resolve, reject) => {
    spawn.once("error", (param, param$1) => reject({
      stdout: stdout,
      stderr: stderr,
      code: 1.0
    }));
    spawn.once("close", (code, _signal) => resolve({
      stdout: stdout,
      stderr: stderr,
      code: code
    }));
  });
}

let SpawnAsync = {
  run: run
};

function createFileInTempDir(id) {
  return Path.join(Os.tmpdir(), id);
}

async function compileTest(param, code) {
  let match = await run(bscBin, [
    "-w",
    "-3-109-44",
    "-e",
    code
  ], undefined);
  let stderr = match.stderr;
  if (stderr.length > 0) {
    return {
      TAG: "Error",
      _0: stderr.map(e => e.toString()).join("")
    };
  } else {
    return {
      TAG: "Ok",
      _0: match.stdout.map(e => e.toString()).join("")
    };
  }
}

async function runtimeTests(code) {
  let match = await run("node", [
    "-e",
    code,
    "--input-type",
    "commonjs"
  ], {
    cwd: process.cwd(),
    timeout: 2000
  });
  let exitCode = match.code;
  let stderr = match.stderr;
  let stdout = match.stdout;
  let std;
  let exit = 0;
  if (exitCode !== null) {
    if (exitCode === 0.0 && stderr.length > 0) {
      std = {
        TAG: "Ok",
        _0: stderr
      };
    } else if (exitCode === 0.0) {
      std = {
        TAG: "Ok",
        _0: stdout
      };
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    std = {
      TAG: "Error",
      _0: stderr.length > 0 ? stderr : stdout
    };
  }
  if (std.TAG === "Ok") {
    return {
      TAG: "Ok",
      _0: std._0.map(e => e.toString()).join("")
    };
  } else {
    return {
      TAG: "Error",
      _0: std._0.map(e => e.toString()).join("")
    };
  }
}

function indentOutputCode(code) {
  let indent = " ".repeat(2);
  return code.split("\n").map(s => indent + s).join("\n");
}

function extractDocFromFile(file) {
  let toolsBin = Path.join(process.cwd(), "cli", "rescript-tools");
  let spawn = Child_process.spawnSync(toolsBin, [
    "doc",
    file
  ]);
  let output = spawn.stdout.toString();
  try {
    return RescriptTools_Docgen.decodeFromJson(JSON.parse(output));
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === Exn.$$Error) {
      return $$Error.panic("Failed to generate docstrings from " + file);
    }
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "DocTest.res",
        204,
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

function chunkArray(array, chunkSize) {
  let result = [];
  let loop = _i => {
    while (true) {
      let i = _i;
      if (i >= array.length) {
        return;
      }
      result.push(array.slice(i, Math.min(i + chunkSize | 0, array.length)));
      _i = i + chunkSize | 0;
      continue;
    };
  };
  loop(0);
  return result;
}

async function main() {
  let files = Fs.readdirSync("runtime");
  let modules = $$Array.reduce(files.filter(f => {
    if (f.startsWith("Js")) {
      return false;
    } else {
      return !f.startsWith("RescriptTools");
    }
  }).filter(f => {
    if (f.endsWith(".res")) {
      return true;
    } else {
      return f.endsWith(".resi");
    }
  }), [], (acc, cur) => {
    let isInterface = cur.endsWith(".resi");
    let resi = Path.join("runtime", cur + "i");
    if (!isInterface && Fs.existsSync(resi)) {
      return acc.concat([cur + "i"]);
    } else if (acc.includes(cur)) {
      return acc;
    } else {
      return acc.concat([cur]);
    }
  }).map(f => getExamples(extractDocFromFile(Path.join("runtime", f)))).flat();
  let batchSize = (Os.cpus().length << 1);
  let chuncks = chunkArray(modules, batchSize);
  let context = {
    contents: 0
  };
  let asyncIterator = $$AsyncIterator.make(async () => {
    let currentValue = context.contents;
    context.contents = currentValue + 1 | 0;
    return {
      done: currentValue === (chuncks.length - 1 | 0),
      value: currentValue
    };
  });
  let result = [];
  let processMyAsyncIterator = async () => {
    let $$break = false;
    while (!$$break) {
      let match = await asyncIterator.next();
      let value = match.value;
      $$break = match.done;
      if (value !== undefined) {
        let c = chuncks[value];
        let a = await Promise.all(c.map(async example => {
          let id = example.id.replaceAll(".", "__");
          let rescriptCode = getCodeBlocks(example);
          let jsCode = await compileTest(id, rescriptCode);
          return [
            example,
            [
              rescriptCode,
              jsCode
            ]
          ];
        }));
        result.push(a);
      }
      
    };
  };
  await processMyAsyncIterator();
  console.log("Compiation tests finished");
  let compilationResults = result.flat();
  let match = $$Array.reduce(compilationResults, [
    [],
    []
  ], (acc, param) => {
    let rhs = acc[1];
    let lhs = acc[0];
    let match = param[1];
    let jsCode = match[1];
    let example = param[0];
    if (jsCode.TAG === "Ok") {
      lhs.push([
        example,
        match[0],
        jsCode._0
      ]);
    } else {
      rhs.push([
        example,
        {
          TAG: "ReScriptError",
          _0: jsCode._0
        }
      ]);
    }
    return [
      lhs,
      rhs
    ];
  });
  chunkArray(match[0], batchSize);
  let runtimeErrors = [];
  let allErrors = runtimeErrors.concat(match[1]);
  allErrors.forEach(param => {
    let errors = param[1];
    let example = param[0];
    let cyan = s => "\x1b[36m" + s + "\x1b[0m";
    let other = example.kind;
    let kind = other === "moduleAlias" ? "module alias" : other;
    let a;
    if (errors.TAG === "ReScriptError") {
      let err = errors._0.split("\n").filter((param, i) => i !== 2).join("\n");
      a = "\x1B[1;31merror\x1B[0m: failed to compile examples from " + kind + " " + cyan(example.id) + "\n" + err;
    } else {
      let indent = " ".repeat(2);
      a = "\x1B[1;31mruntime error\x1B[0m: failed to run examples from " + kind + " " + cyan(example.id) + "\n\n" + indent + "\x1b[36mReScript\x1b[0m\n\n" + indentOutputCode(errors.rescript) + "\n\n" + indent + "\x1b[36mCompiled Js\x1b[0m\n\n" + indentOutputCode(errors.js) + "\n\n" + indent + "\x1B[1;31mstacktrace\x1B[0m\n\n" + indentOutputCode(errors.error) + "\n";
    }
    process.stderr.write(a);
  });
  let someError = allErrors.length > 0;
  if (someError) {
    return 1;
  } else {
    return 0;
  }
}

let exitCode = await main();

process.exit(exitCode);

let Docgen;

export {
  Node,
  Docgen,
  bscBin,
  options,
  values,
  ignoreRuntimeTests,
  SpawnAsync,
  createFileInTempDir,
  compileTest,
  runtimeTests,
  indentOutputCode,
  extractDocFromFile,
  getExamples,
  getCodeBlocks,
  chunkArray,
  main,
  exitCode,
}
/* bscBin Not a pure module */
