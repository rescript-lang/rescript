#!/usr/bin/env node

// @ts-check

// Writes THIRD_PARTY_LICENSES into every native platform package from
// licenses/components.json. With --check, fails when a committed file
// differs from what would be generated.
//
// Versions and license texts of OCaml libraries come from the current opam
// switch, so this must run in the switch the release binaries are built with.
// The script also fails when the opam packages linked into the shipped
// executables differ from the ones the inventory lists.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { projectDir } from "#dev/paths";

const check = process.argv.includes("--check");
const separator =
  "\n\n-------------------------------------------------------------------------------\n\n";

const platforms = [
  { packageName: "darwin-arm64", family: "darwin" },
  { packageName: "linux-arm64", family: "linux" },
  { packageName: "linux-x64", family: "linux" },
  { packageName: "win32-x64", family: "win32" },
];

// The dune executables in the platform packages, by source directory.
const shippedExecutables = new Map([
  ["compiler/bsc", "rescript_compiler_main"],
  ["rewatch-ocaml", "rescript_ocaml"],
  ["analysis/bin", "main"],
  ["tools/bin", "main"],
]);

/**
 * @typedef {{
 *   name: string,
 *   opam?: string,
 *   version?: string,
 *   libuvVersionFrom?: string,
 *   source: string,
 *   license: string,
 *   scope: string,
 *   notes?: string,
 *   platforms?: string[],
 *   licenseFiles?: string[],
 * }} Component
 */

/** @type {Component[]} */
const components = JSON.parse(
  fs.readFileSync(path.join(projectDir, "licenses", "components.json"), "utf8"),
);

/**
 * @param {string} command
 * @param {string[]} args
 * @returns {string}
 */
function run(command, args) {
  return execFileSync(command, args, {
    cwd: projectDir,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  }).trim();
}

/** @typedef {string | unknown[]} Sexp */

/**
 * Parses the s-expressions dune prints. Atoms are not quoted in the output
 * of `dune describe workspace`.
 *
 * @param {string} text
 * @returns {Sexp}
 */
function parseSexp(text) {
  const tokens = text.match(/[()]|[^\s()]+/g) ?? [];
  /** @type {Sexp[][]} */
  const stack = [[]];
  for (const token of tokens) {
    if (token === "(") {
      stack.push([]);
    } else if (token === ")") {
      const list = stack.pop();
      if (list === undefined || stack.length === 0) {
        throw new Error("Unbalanced dune output");
      }
      stack[stack.length - 1].push(list);
    } else {
      stack[stack.length - 1].push(token);
    }
  }
  return stack[0][0];
}

/**
 * @param {unknown} fields a list of (key value) pairs
 * @param {string} key
 * @returns {unknown}
 */
function field(fields, key) {
  if (!Array.isArray(fields)) return undefined;
  const entry = fields.find(f => Array.isArray(f) && f[0] === key);
  return Array.isArray(entry) ? entry[1] : undefined;
}

/**
 * @param {unknown} list
 * @returns {string[]}
 */
function atoms(list) {
  return Array.isArray(list) ? list.map(String) : [];
}

/**
 * The opam packages providing the libraries linked into the shipped
 * executables, read from dune's view of the workspace: `requires` lists the
 * libraries a stanza links, including the runtime libraries of its PPX
 * rewriters but not the rewriters themselves.
 *
 * @returns {Set<string>}
 */
function linkedOpamPackages() {
  const description = parseSexp(
    run("dune", ["describe", "workspace", "--root", projectDir]),
  );
  /** @type {Map<string, unknown>} */
  const libraries = new Map();
  /** @type {unknown[]} */
  const roots = [];
  for (const stanza of Array.isArray(description) ? description : []) {
    if (!Array.isArray(stanza)) continue;
    const [kind, fields] = stanza;
    if (kind === "library") {
      libraries.set(String(field(fields, "uid")), fields);
    } else if (kind === "executables") {
      // The stanza has no source directory; its modules' paths give it.
      const modules = field(fields, "modules");
      const impl = atoms(
        field(Array.isArray(modules) ? modules[0] : undefined, "impl"),
      )[0];
      const sourceDir = path.posix.dirname(
        path.posix.relative("_build/default", impl ?? ""),
      );
      const name = shippedExecutables.get(sourceDir);
      if (name !== undefined && atoms(field(fields, "names")).includes(name)) {
        roots.push(fields);
      }
    }
  }
  if (roots.length !== shippedExecutables.size) {
    throw new Error("Could not find all shipped executables in dune's output");
  }

  const libDir = run("opam", ["var", "lib"]);
  /** @type {Set<string>} */
  const packages = new Set();
  /** @type {Set<string>} */
  const visited = new Set();
  const pending = [...roots];
  while (pending.length > 0) {
    const fields = pending.pop();
    if (field(fields, "local") === "false") {
      // An installed library lives in <lib>/<opam package>; the compiler's
      // own libraries (unix, str, threads) in <lib>/ocaml.
      const sourceDir = String(field(fields, "source_dir"));
      packages.add(path.relative(libDir, sourceDir).split(path.sep)[0]);
    }
    for (const uid of atoms(field(fields, "requires"))) {
      const library = libraries.get(uid);
      if (library === undefined) {
        throw new Error(`Unknown library ${uid} in dune's output`);
      }
      if (!visited.has(uid)) {
        visited.add(uid);
        pending.push(library);
      }
    }
  }
  return packages;
}

/**
 * @param {string} opamPackage
 * @param {string} variable
 * @returns {string}
 */
function opamVar(opamPackage, variable) {
  return run("opam", ["var", `${opamPackage}:${variable}`]);
}

/**
 * @param {string} opamPackage
 * @returns {string[]}
 */
function opamLicenseTexts(opamPackage) {
  const docDir = opamVar(opamPackage, "doc");
  const files = fs
    .readdirSync(docDir)
    .filter(file => /^(licen[cs]e|copying)(\.md|\.txt)?$/i.test(file))
    .sort();
  if (files.length === 0) {
    throw new Error(`No license file for ${opamPackage} in ${docDir}`);
  }
  return files.map(file => fs.readFileSync(path.join(docDir, file), "utf8"));
}

/**
 * Reads the libuv version from the uv/version.h header an opam package
 * installs.
 *
 * @param {string} opamPackage
 * @returns {string}
 */
function libuvVersion(opamPackage) {
  const header = fs.readFileSync(
    path.join(opamVar(opamPackage, "lib"), "uv", "version.h"),
    "utf8",
  );
  return ["MAJOR", "MINOR", "PATCH"]
    .map(part => {
      const match = header.match(
        new RegExp(`#define UV_VERSION_${part} (\\d+)`),
      );
      if (match === null) throw new Error("Unexpected uv/version.h");
      return match[1];
    })
    .join(".");
}

/**
 * Removes trailing whitespace, which some upstream license files have.
 *
 * @param {string} text
 * @returns {string}
 */
function normalize(text) {
  return text
    .split(/\r?\n/)
    .map(line => line.trimEnd())
    .join("\n")
    .trimEnd();
}

/**
 * @param {Component} component
 * @returns {string}
 */
function renderComponent(component) {
  const version =
    component.opam !== undefined
      ? opamVar(component.opam, "version")
      : component.libuvVersionFrom !== undefined
        ? libuvVersion(component.libuvVersionFrom)
        : component.version;
  const metadata = [
    component.name.toUpperCase(),
    "=".repeat(component.name.length),
    version === undefined ? undefined : `Version: ${version}`,
    `Source: ${component.source}`,
    `License: ${component.license}`,
    `Included in: ${component.scope}`,
    component.notes === undefined ? undefined : `Notes: ${component.notes}`,
  ].filter(line => line !== undefined);
  const licenseTexts = [
    ...(component.opam === undefined ? [] : opamLicenseTexts(component.opam)),
    ...(component.licenseFiles ?? []).map(file =>
      fs.readFileSync(path.join(projectDir, file), "utf8"),
    ),
  ].map(normalize);
  return `${metadata.join("\n")}\n\n${licenseTexts.join("\n\n")}`;
}

const linked = linkedOpamPackages();
const listed = new Set(
  components.flatMap(component =>
    component.opam === undefined ? [] : [component.opam],
  ),
);
const unlisted = [...linked].filter(name => !listed.has(name));
const notLinked = [...listed].filter(name => !linked.has(name));
if (unlisted.length > 0 || notLinked.length > 0) {
  for (const name of unlisted) {
    console.error(`${name} is linked but missing from the inventory.`);
  }
  for (const name of notLinked) {
    console.error(`${name} is in the inventory but not linked.`);
  }
  console.error("Update licenses/components.json.");
  process.exit(1);
}

/** @type {Map<Component, string>} */
const rendered = new Map(
  components.map(component => [component, renderComponent(component)]),
);

for (const platform of platforms) {
  const sections = components
    .filter(
      component =>
        component.platforms === undefined ||
        component.platforms.includes(platform.family),
    )
    .map(component => rendered.get(component));
  const introduction = `THIRD-PARTY LICENSES AND NOTICES
================================

This file covers third-party software incorporated into the native ReScript
binaries for ${platform.packageName}.

It is generated from licenses/components.json in the ReScript
repository by scripts/generateThirdPartyLicenses.js.`;
  const generated = `${introduction}${separator}${sections.join(separator)}\n`;
  const outputFile = path.join(
    projectDir,
    "packages",
    "@rescript",
    platform.packageName,
    "THIRD_PARTY_LICENSES",
  );

  if (check) {
    if (
      !fs.existsSync(outputFile) ||
      fs.readFileSync(outputFile, "utf8") !== generated
    ) {
      console.error(`${outputFile} is stale. Run \`yarn licenses:generate\`.`);
      process.exitCode = 1;
    }
  } else {
    fs.writeFileSync(outputFile, generated);
  }
}
