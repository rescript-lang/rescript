#!/usr/bin/env node

// @ts-check

/*
 * You need to build cmij files with the same rescript version as the compiler bundle.
 *
 * This script extracts the cmi / cmj files of packages/@rescript/runtime/lib/ocaml
 * and of every dependency listed in the playground package's rescript.json,
 * creates a cmij.js file for each library and puts them in the playground's
 * packages directory.
 *
 * The cmij files are representing the marshaled dependencies that can be used with the ReScript
 * playground bundle.
 */

import * as fs from "node:fs";
import * as path from "node:path";

import resConfig from "../rescript.json" with { type: "json" };
import {
  exec,
  compilerRootDir,
  playgroundPackagesDir,
} from "./common.mjs";

exec("yarn rescript clean");
exec("yarn rescript build");

// We need to build the compiler's builtin modules as a separate cmij.
// Otherwise we can't use them for compilation within the playground.
buildCmij(path.join(compilerRootDir, "packages", "@rescript", "runtime"), "compiler-builtins");

const packages = resConfig["dependencies"];
for (const pkgName of packages) {
  buildCmij(
    path.join(compilerRootDir, "node_modules", pkgName),
    pkgName,
  );
}

/**
 * @param {string} pkgDir
 * @param {string} pkgName
 */
function buildCmij(pkgDir, pkgName) {
  const libOcamlFolder = path.join(
    pkgDir,
    "lib",
    "ocaml",
  );

  const outputFolder = path.join(playgroundPackagesDir, pkgName);
  fs.mkdirSync(outputFolder, { recursive: true });

  const cmijFile = path.join(outputFolder, "cmij.js");
  const files = fs
    .readdirSync(libOcamlFolder)
    .filter(isCmij)
    .map(basename => {
      const bytes = fs.readFileSync(path.join(libOcamlFolder, basename));
      return `  a("/static/${basename}", "${jsBytesLiteral(bytes)}");\n`;
    });

  // The same pseudo filesystem file as `js_of_ocaml build-fs` writes. The
  // compiler bundle creates the files when it is loaded after it
  // (jsoo_create_file), or picks them up when loaded before it (jsoo_fs_tmp).
  // We write it ourselves because since js_of_ocaml 6.5.0, build-fs encodes
  // the contents in base64, which compresses much worse, and decodes them
  // with a runtime primitive that a standalone file cannot reach.
  fs.writeFileSync(
    cmijFile,
    `(function (c) {
  "use strict";
  function a(name, content) {
    if (c.jsoo_create_file) c.jsoo_create_file(name, content);
    else {
      if (!c.jsoo_fs_tmp) c.jsoo_fs_tmp = [];
      c.jsoo_fs_tmp.push({ name, content });
    }
  }
${files.join("")}})(globalThis);
`,
  );
}

/**
 * The contents of a JavaScript string literal with one character per byte,
 * which is how js_of_ocaml represents bytes.
 *
 * @param {Buffer} bytes
 * @return {string}
 */
function jsBytesLiteral(bytes) {
  let s = "";
  for (let i = 0; i < bytes.length; i++) {
    const b = bytes[i];
    if (b === 0x22 || b === 0x5c) s += `\\${String.fromCharCode(b)}`;
    else if (b >= 0x20 && b < 0x7f) s += String.fromCharCode(b);
    else if (b === 0 && !(bytes[i + 1] >= 0x30 && bytes[i + 1] <= 0x39))
      s += "\\0";
    else s += `\\x${b.toString(16).padStart(2, "0")}`;
  }
  return s;
}

/**
 * @param {string} basename
 * @return {boolean}
 */
function isCmij(basename) {
  return /\.cm(i|j)$/.test(basename);
}
