#!/usr/bin/env node

// @ts-check

/* Draws commit records as a Semantic Diff page: one card per commit, and for
 * a range a summary, a changelog draft and the files more than one commit
 * changes. A record is a commit message, or a note on refs/notes/records,
 * which takes precedence, in the format of the `records` skill
 * (.claude/skills/records/SKILL.md). The changelog is the
 * changes/<name>.<category>.md fragments the commits add or change. The
 * drawing is scripts/recordRender.js; this file reads git.
 *
 *   node scripts/commitRecords.js --out review.html <rev>...
 *   node scripts/commitRecords.js --out review.html <base>..<tip>...
 *   --pr <number> and --title <text> head the page with the pull request.
 *   --fragment omits the document skeleton.
 */

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { parseFragment } from "./changelog.js";
import { CSS, esc, pageBody, pageTitle, parseRecord } from "./recordRender.js";

/** @import { CommitRecord, FileChange, Fragment, Group } from "./recordRender.js" */

/** @param {string[]} args */
const git = args =>
  execFileSync("git", args, {
    encoding: "utf8",
    maxBuffer: 1 << 28,
    stdio: ["ignore", "pipe", "ignore"],
  });

/** @param {string} path */
const isFragment = path =>
  /^changes\/[^/]+\.md$/.test(path) && path !== "changes/README.md";

/**
 * The changelog fragments a commit adds or changes, as they stand in its
 * tree. A fragment that `yarn changelog:check` rejects (such as one with the
 * placeholder PR link) keeps the category from its name and carries the
 * reason.
 * @param {string} rev
 * @param {FileChange[]} files
 * @returns {Fragment[]}
 */
function fragmentsOf(rev, files) {
  return files
    .filter(f => f.status !== "D" && isFragment(f.path))
    .map(f => {
      const name = f.path.slice("changes/".length);
      const content = git(["show", `${rev}:${f.path}`]);
      try {
        const { category, text } = parseFragment(name, content);
        return { path: f.path, category, text, error: "" };
      } catch (e) {
        return {
          path: f.path,
          category: /\.([a-z]+)\.md$/.exec(name)?.[1] ?? "?",
          text: content.trim(),
          error: e instanceof Error ? e.message : String(e),
        };
      }
    });
}

/**
 * The record of one commit: its note if it has one, else its message, with
 * the files it changes and the diff of the files the checks read.
 * @param {string} rev
 * @returns {CommitRecord}
 */
function readRecord(rev) {
  let note = "";
  try {
    note = git(["notes", "--ref=records", "show", rev]);
  } catch {
    // no note
  }
  const message = git(["show", "-s", "--format=%B", rev]);
  /** @type {Map<string, FileChange>} */
  const files = new Map();
  const tree = ["diff-tree", "--no-commit-id", "-r", "--no-renames", "--root"];
  for (const line of git([...tree, "--name-status", rev]).split("\n")) {
    const [status, ...paths] = line.split("\t");
    const path = paths[paths.length - 1];
    if (status && path)
      files.set(path, { status: status[0], path, added: 0, removed: 0 });
  }
  for (const line of git([...tree, "--numstat", rev]).split("\n")) {
    const [added, removed, path] = line.split("\t");
    const f = files.get(path);
    if (f) {
      f.added = Number(added) || 0;
      f.removed = Number(removed) || 0;
    }
  }
  return parseRecord(note.trim() ? note : message, {
    sha: git(["rev-parse", "--short=9", rev]).trim(),
    author: git(["show", "-s", "--format=%an", rev]).trim(),
    date: git([
      "show",
      "-s",
      "--format=%ad",
      "--date=format:%b %d, %Y",
      rev,
    ]).trim(),
    message,
    noted: note.trim() !== "",
    files: [...files.values()],
    fragments: fragmentsOf(rev, [...files.values()]),
    patch: git(["show", "--format=", rev, "--", "compiler/ext/config.ml"]),
  });
}

/**
 * A revision is one card; a range `base..tip` is its commits, oldest first.
 * @param {string} arg
 * @returns {Group}
 */
function group(arg) {
  if (!arg.includes("..")) return { name: arg, records: [readRecord(arg)] };
  const revs = git(["rev-list", "--reverse", "--no-merges", arg])
    .split("\n")
    .filter(Boolean);
  return {
    name: arg.split("..")[1] || arg,
    range: arg,
    records: revs.map(readRecord),
  };
}

const args = process.argv.slice(2);
/** @type {Record<string, string>} */
const options = {};
/** @type {string[]} */
const targets = [];
let fragment = false;
for (let i = 0; i < args.length; i++) {
  if (["--out", "--pr", "--title"].includes(args[i]) && i + 1 < args.length)
    options[args[i].slice(2)] = args[++i];
  else if (args[i] === "--fragment") fragment = true;
  else targets.push(args[i]);
}
const outFile = options.out;
if (!outFile) {
  console.error(
    "usage: commitRecords.js --out <page.html> [--pr <number>] [--title <text>] [--fragment] <rev | base..tip>...",
  );
  process.exit(1);
}
const heading = {
  pr: (options.pr ?? "").replace(/\D/g, ""),
  title: options.title ?? "",
};
const body = pageBody(targets.map(group), heading);
const head = `<title>${esc(pageTitle(heading))}</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&family=STIX+Two+Text:ital,wght@0,400;0,500;1,400&display=swap">
<style>${CSS}</style>`;
writeFileSync(
  outFile,
  fragment
    ? `${head}\n${body}\n`
    : `<!doctype html>\n<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n${head}</head><body>${body}</body></html>\n`,
);
console.log(outFile);
