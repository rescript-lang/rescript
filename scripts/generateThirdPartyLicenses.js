#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { projectDir, rewatchDir } from "#dev/paths";

const check = process.argv.includes("--check");
const cargoAbout = process.env.CARGO_ABOUT ?? "cargo-about";
const licensesDir = path.join(projectDir, "licenses");
const rustCopyrightFile = path.join(
  licensesDir,
  "toolchains",
  "rust-1.91.0-standard-library.html",
);

const platforms = [
  {
    packageName: "darwin-arm64",
    family: "darwin",
    target: "aarch64-apple-darwin",
  },
  {
    packageName: "darwin-x64",
    family: "darwin",
    target: "x86_64-apple-darwin",
  },
  {
    packageName: "linux-arm64",
    family: "linux",
    target: "aarch64-unknown-linux-musl",
  },
  {
    packageName: "linux-x64",
    family: "linux",
    target: "x86_64-unknown-linux-musl",
  },
  {
    packageName: "win32-x64",
    family: "win32",
    target: "x86_64-pc-windows-gnu",
  },
];

/**
 * @typedef {{
 *   name: string,
 *   version?: string,
 *   source: string,
 *   license: string,
 *   scope: string,
 *   notes?: string,
 *   platforms?: string[],
 *   licenseFiles: string[],
 * }} CuratedComponent
 */

/** @type {CuratedComponent[]} */
const curatedComponents = JSON.parse(
  fs.readFileSync(path.join(licensesDir, "curated-components.json"), "utf8"),
);

/**
 * @param {string} text
 * @returns {string}
 */
function decodeHtml(text) {
  /** @type {Record<string, string>} */
  const namedEntities = {
    amp: "&",
    apos: "'",
    gt: ">",
    lt: "<",
    quot: '"',
  };
  return text
    .replace(/&#(x?[0-9a-f]+);/gi, (_match, value) => {
      const hexadecimal = value[0].toLowerCase() === "x";
      return String.fromCodePoint(
        Number.parseInt(
          hexadecimal ? value.slice(1) : value,
          hexadecimal ? 16 : 10,
        ),
      );
    })
    .replace(/&([a-z]+);/gi, (match, name) => namedEntities[name] ?? match);
}

/**
 * @param {string} html
 * @returns {string}
 */
function htmlToText(html) {
  return decodeHtml(
    html
      .replace(/<head>[\s\S]*?<\/head>/i, "")
      .replace(/<li>/gi, "- ")
      .replace(/<br\s*\/?>/gi, "\n")
      .replace(/<\/(?:div|details|h1|h2|h3|li|p|pre|summary|ul)>/gi, "\n")
      .replace(/<[^>]+>/g, ""),
  )
    .split("\n")
    .map(line => line.trimEnd())
    .filter(
      (line, index, lines) =>
        line.trim() !== "" || lines[index - 1]?.trim() !== "",
    )
    .join("\n")
    .trim();
}

/**
 * The Rust release inventory repeats the complete Apache and MIT texts for
 * many standard-library dependencies. Preserve all component metadata and
 * notice text, but store each byte-identical notice only once.
 *
 * @param {string} html
 * @returns {string}
 */
function compactRustCopyright(html) {
  const dependencyHeading = '<h2 id="out-of-tree-dependencies">';
  const dependencyStart = html.indexOf(dependencyHeading);
  if (dependencyStart === -1) {
    throw new Error("Rust standard-library inventory has an unexpected format");
  }

  const overview = htmlToText(html.slice(0, dependencyStart));
  const dependencyHtml = html.slice(dependencyStart);
  /** @type {Map<string, {id: string, names: Set<string>}>} */
  const notices = new Map();
  const dependencies = [];
  let noticeReferenceCount = 0;
  const dependencyPattern = /<h3>([\s\S]*?)<\/h3>([\s\S]*?)(?=<h3>|<\/body>)/g;

  for (const dependencyMatch of dependencyHtml.matchAll(dependencyPattern)) {
    const name = htmlToText(dependencyMatch[1]).replace(/^📦\s*/, "");
    const body = dependencyMatch[2];
    const metadata = htmlToText(body.split("<p><b>Notices:</b>")[0]);
    const references = [];
    const noticePattern =
      /<details>[\s\S]*?<summary><code>([\s\S]*?)<\/code><\/summary>\s*<pre>([\s\S]*?)<\/pre>[\s\S]*?<\/details>/g;

    for (const noticeMatch of body.matchAll(noticePattern)) {
      const fileName = htmlToText(noticeMatch[1]);
      const text = decodeHtml(noticeMatch[2]).trim();
      let notice = notices.get(text);
      if (notice === undefined) {
        notice = {
          id: `RUST-STDLIB-NOTICE-${notices.size + 1}`,
          names: new Set(),
        };
        notices.set(text, notice);
      }
      notice.names.add(name);
      references.push(`${notice.id} (${fileName})`);
      noticeReferenceCount += 1;
    }

    if (body.includes("<p><b>Notices:</b>") && references.length === 0) {
      throw new Error(
        `No notices found for Rust standard-library dependency ${name}`,
      );
    }
    dependencies.push(
      [
        name,
        metadata,
        references.length === 0
          ? undefined
          : `Notice texts: ${references.join(", ")}`,
      ]
        .filter(line => line !== undefined)
        .join("\n"),
    );
  }

  if (dependencies.length === 0) {
    throw new Error("No Rust standard-library dependencies found");
  }
  const sourceNoticeCount = dependencyHtml.match(/<pre>/g)?.length ?? 0;
  if (noticeReferenceCount !== sourceNoticeCount) {
    throw new Error(
      `Expected ${sourceNoticeCount} Rust standard-library notices, parsed ${noticeReferenceCount}`,
    );
  }

  const sharedNotices = [...notices.entries()].map(
    ([text, { id, names }]) =>
      `${id}\nUsed by: ${[...names].join(", ")}\n\n${text}`,
  );

  return `${overview}

Out-of-tree dependencies
------------------------

${dependencies.join("\n\n")}

Shared notice texts
-------------------

${sharedNotices.join(
  "\n\n-------------------------------------------------------------------------------\n\n",
)}`;
}

const compactRustStandardLibrary = compactRustCopyright(
  fs.readFileSync(rustCopyrightFile, "utf8"),
);

/**
 * @param {CuratedComponent} component
 * @returns {string}
 */
function renderComponent(component) {
  const metadata = [
    component.name.toUpperCase(),
    "=".repeat(component.name.length),
    component.version === undefined
      ? undefined
      : `Version: ${component.version}`,
    `Source: ${component.source}`,
    `License: ${component.license}`,
    `Included in: ${component.scope}`,
    component.notes === undefined ? undefined : `Notes: ${component.notes}`,
  ].filter(line => line !== undefined);
  const licenseTexts = component.licenseFiles.map(file =>
    file === "licenses/toolchains/rust-1.91.0-standard-library.html"
      ? compactRustStandardLibrary
      : fs.readFileSync(path.join(projectDir, file), "utf8").trimEnd(),
  );
  return `${metadata.join("\n")}\n\n${licenseTexts.join("\n\n")}`;
}

/**
 * @param {string} target
 * @returns {string}
 */
function generateRustDependencies(target) {
  try {
    return execFileSync(
      cargoAbout,
      [
        "generate",
        "--locked",
        "--fail",
        "--target",
        target,
        "--manifest-path",
        path.join(rewatchDir, "Cargo.toml"),
        "--config",
        path.join(licensesDir, "about.toml"),
        path.join(licensesDir, "about.hbs"),
      ],
      { encoding: "utf8" },
    );
  } catch (error) {
    if (
      typeof error === "object" &&
      error !== null &&
      "code" in error &&
      error.code === "ENOENT"
    ) {
      console.error(
        "cargo-about is required. Install cargo-about 0.9.2 or set CARGO_ABOUT to its executable.",
      );
    }
    throw error;
  }
}

for (const platform of platforms) {
  const components = curatedComponents.filter(
    component =>
      component.platforms === undefined ||
      component.platforms.includes(platform.family),
  );
  const curatedSections = components.map(component =>
    renderComponent(component),
  );
  const rustDependencies = generateRustDependencies(platform.target);
  const introduction = `THIRD-PARTY LICENSES AND NOTICES
================================

This file covers third-party software incorporated into the native ReScript
binaries for ${platform.packageName} (${platform.target}).

The root rescript package, @rescript/runtime, and @rescript/belt do not contain
these native binaries and do not require this notice bundle.`;
  const generated = `${introduction}\n\n${curatedSections.join(
    "\n\n-------------------------------------------------------------------------------\n\n",
  )}\n\n${rustDependencies.trimEnd()}\n`;
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
