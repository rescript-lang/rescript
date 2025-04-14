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

import * as child_process from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";

import resConfig from "../rescript.json" with { type: "json" };

const RESCRIPT_COMPILER_ROOT_DIR = path.join(
  import.meta.dirname,
  "..",
  "..",
  "..",
);

const PLAYGROUND_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "playground");

// The playground-bundling root dir
const PROJECT_ROOT_DIR = path.join(import.meta.dirname, "..");

// Final target output directory where all the cmijs will be stored
const PACKAGES_DIR = path.join(PLAYGROUND_DIR, "packages");

// Making sure this directory exists, since it's not checked in to git
fs.mkdirSync(PACKAGES_DIR, { recursive: true });

/**
 * @param {string} cmd
 */
function e(cmd) {
  console.log(`>>>>>> running command: ${cmd}`);
  child_process.execSync(cmd, {
    cwd: PROJECT_ROOT_DIR,
    encoding: "utf8",
    stdio: [0, 1, 2],
  });
  console.log("<<<<<<");
}

e("yarn install");
e("yarn rescript clean");
e("yarn rescript");

const packages = resConfig["bs-dependencies"];

// We need to build the compiler's builtin modules as a separate cmij.
// Otherwise we can't use them for compilation within the playground.
function buildCompilerCmij() {
  const rescriptLibOcamlFolder = path.join(
    RESCRIPT_COMPILER_ROOT_DIR,
    "lib",
    "ocaml",
  );

  const outputFolder = path.join(PACKAGES_DIR, "compiler-builtins");
  fs.mkdirSync(outputFolder, { recursive: true });

  const cmijFile = path.join(outputFolder, "cmij.cjs");

  e(
    `find ${rescriptLibOcamlFolder} -name "*.cmi" -or -name "*.cmj" | xargs -n1 basename | xargs js_of_ocaml build-fs -o ${cmijFile} -I ${rescriptLibOcamlFolder}`,
  );
}

function buildThirdPartyCmijs() {
  for (const pkg of packages) {
    const libOcamlFolder = path.join(
      RESCRIPT_COMPILER_ROOT_DIR,
      "node_modules",
      pkg,
      "lib",
      "ocaml",
    );
    const libEs6Folder = path.join(
      RESCRIPT_COMPILER_ROOT_DIR,
      "node_modules",
      pkg,
      "lib",
      "es6",
    );
    const outputFolder = path.join(PACKAGES_DIR, pkg);
    fs.mkdirSync(outputFolder, { recursive: true });

    const cmijFile = path.join(outputFolder, "cmij.cjs");

    e(`find ${libEs6Folder} -name '*.js' -exec cp {} ${outputFolder} \\;`);
    e(
      `find ${libOcamlFolder} -name "*.cmi" -or -name "*.cmj" | xargs -n1 basename | xargs js_of_ocaml build-fs -o ${cmijFile} -I ${libOcamlFolder}`,
    );
  }
}

function bundleStdlibJs() {
  e("yarn bundle");
}

buildCompilerCmij();
buildThirdPartyCmijs();
bundleStdlibJs();
