#!/usr/bin/env node

// @ts-check

/*
 * You need to build cmij files with the same rescript version as the compiler bundle.
 *
 * This script extracts all cmi / cmj files of the rescript/lib/ocaml and all
 * dependencies listed in the project root's rescript.json, creates cmij.js
 * files for each library and puts them in the compiler playground directory.
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
exec("yarn rescript legacy");

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
  const inputFiles = fs.readdirSync(libOcamlFolder).filter(isCmij).join(" ");
  exec(`js_of_ocaml build-fs -o ${cmijFile} -I ${libOcamlFolder} ${inputFiles}`);
}

/**
 * @param {string} basename
 * @return {boolean}
 */
function isCmij(basename) {
  return /\.cm(i|j)$/.test(basename);
}
