// @ts-check

// Checks that JSX preserve mode keeps the meaning of each element: every case
// in cases/ is compiled with and without preserve mode, the preserved JSX is
// compiled back to jsx() calls with TypeScript, and the element trees both
// versions build against a recording JSX runtime must be equal.

import assert from "node:assert/strict";
import * as fs from "node:fs";
import { createRequire } from "node:module";
import * as path from "node:path";
import ts from "typescript";
import { setup } from "#dev/process";

const dir = import.meta.dirname;
const cases = fs
  .readdirSync(path.join(dir, "cases"))
  .filter(file => file.endsWith(".res"))
  .sort();

for (const project of ["plain", "preserve"]) {
  const src = path.join(dir, project, "src");
  fs.rmSync(src, { recursive: true, force: true });
  fs.mkdirSync(src);
  for (const file of cases) {
    fs.copyFileSync(path.join(dir, "cases", file), path.join(src, file));
  }
  const { execClean, execBuildOrThrow } = setup(path.join(dir, project));
  await execClean();
  await execBuildOrThrow();
}

// A JSX runtime that records the elements it is asked to create
const fragment = Symbol("Fragment");

/** @param {unknown} type */
function typeName(type) {
  if (typeof type === "string") return type;
  if (type === fragment) return "#Fragment";
  if (typeof type === "function") return `function ${type.name}`;
  if (type && typeof type === "object" && "tag" in type) return type.tag;
  return `unknown ${typeof type}`;
}

/** @param {string} kind */
function recordElement(kind) {
  // The argument count tells an explicit undefined key from an absent one
  /** @param {unknown[]} args */
  return (...args) => ({
    kind,
    type: typeName(args[0]),
    props: args[1],
    key: args[2],
    argumentCount: args.length,
  });
}

const jsxRuntime = {
  jsx: recordElement("jsx"),
  jsxs: recordElement("jsxs"),
  Fragment: fragment,
};
/** @type {Record<string, unknown>} */
const mocks = {
  "react/jsx-runtime": jsxRuntime,
  react: {
    Fragment: fragment,
    /** @param {unknown} component */
    memo: component => ({ tag: `memo(${typeName(component)})` }),
    /** @param {unknown} value */
    createContext: value => ({ Provider: { tag: "Provider" }, value }),
    createElement: recordElement("createElement"),
  },
  // CustomRenamed.res: a JSX runtime whose functions have other names
  "some-runtime": {
    j: recordElement("jsx"),
    js: recordElement("jsxs"),
    Frag: fragment,
  },
  "some-lib": {
    default: function SomeLibDefault() {},
    head: function head() {},
  },
};

/**
 * Evaluates a CommonJS module, serving the mocks to its require calls
 * @param {string} file
 * @param {string} code
 */
function load(file, code) {
  const realRequire = createRequire(file);
  /** @param {string} id */
  const require = id => (id in mocks ? mocks[id] : realRequire(id));
  const module = { exports: {} };
  new Function("require", "module", "exports", code)(
    require,
    module,
    module.exports,
  );
  return module.exports;
}

/**
 * Functions can't be compared across the two builds; compare their names
 * @param {unknown} value
 * @returns {unknown}
 */
function comparable(value) {
  if (typeof value === "function") return `function ${value.name}`;
  if (Array.isArray(value)) return value.map(comparable);
  if (value && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([k, v]) => [k, comparable(v)]),
    );
  }
  return value;
}

for (const file of cases) {
  const name = file.replace(/\.res$/, "");
  const plainFile = path.join(dir, "plain", "lib", "js", "src", `${name}.cjs`);
  const preserveFile = path.join(
    dir,
    "preserve",
    "lib",
    "js",
    "src",
    `${name}.jsx`,
  );
  const preserved = fs.readFileSync(preserveFile, "utf8");

  const source = ts.createSourceFile(
    preserveFile,
    preserved,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.JSX,
  );
  /** @type {readonly ts.Diagnostic[]} */
  // parseDiagnostics is internal to TypeScript's API
  const diagnostics = /** @type {any} */ (source).parseDiagnostics;
  assert.deepEqual(
    diagnostics.map(d => ts.flattenDiagnosticMessageText(d.messageText, "\n")),
    [],
    `${name}: preserved output is not valid JSX`,
  );

  const transpiled = ts.transpileModule(preserved, {
    fileName: preserveFile,
    compilerOptions: {
      jsx: ts.JsxEmit.ReactJSX,
      module: ts.ModuleKind.CommonJS,
      target: ts.ScriptTarget.ES2022,
    },
  }).outputText;

  assert.deepEqual(
    comparable(load(preserveFile, transpiled)),
    comparable(load(plainFile, fs.readFileSync(plainFile, "utf8"))),
    `${name}: preserved JSX builds different elements`,
  );
}
