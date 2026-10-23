#!/usr/bin/env node

// @ts-check

/* Draws commit records as a Semantic Diff page: one card per commit, each
 * the commit's identity and its message drawn. A record is a commit message
 * in the format of the `records` skill (.claude/skills/records/SKILL.md).
 * This file reads git; scripts/recordRender.js draws.
 *
 *   node scripts/commitRecords.js --out review.html <rev>...
 *   node scripts/commitRecords.js --out review.html <base>..<tip>...
 *   --pr <number> and --title <text> head the page with the pull request.
 *   --markdown <file> also draws the records as the pull request's comment
 *   (scripts/recordMarkdown.js), linking the page at --url <url>.
 */

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { commentMarkdown } from "./recordMarkdown.js";
import { CSS, esc, pageBody, pageTitle, parseRecord } from "./recordRender.js";

/** @import { CommitRecord, Group } from "./recordRender.js" */

/** @param {string[]} args */
const git = args =>
  execFileSync("git", args, {
    encoding: "utf8",
    maxBuffer: 1 << 28,
    stdio: ["ignore", "pipe", "ignore"],
  });

/**
 * The record of one commit: its short hash, author, date and message.
 * @param {string} rev
 * @returns {CommitRecord}
 */
function readRecord(rev) {
  const show = (/** @type {string[]} */ ...format) =>
    git(["show", "-s", ...format, rev]);
  return parseRecord({
    sha: show("--format=%h", "--abbrev=9").trim(),
    author: show("--format=%an").trim(),
    date: show("--format=%ad", "--date=format:%b %d, %Y").trim(),
    message: show("--format=%B"),
  });
}

/**
 * A revision is one card; a range `base..tip` is its commits, oldest first.
 * @param {string} arg
 * @returns {Group}
 */
function group(arg) {
  if (!arg.includes("..")) return { records: [readRecord(arg)] };
  const revs = git(["rev-list", "--reverse", "--no-merges", arg])
    .split("\n")
    .filter(Boolean);
  return { range: arg, records: revs.map(readRecord) };
}

const args = process.argv.slice(2);
/** @type {Record<string, string>} */
const options = {};
/** @type {string[]} */
const targets = [];
for (let i = 0; i < args.length; i++) {
  if (
    ["--out", "--pr", "--title", "--markdown", "--url"].includes(args[i]) &&
    i + 1 < args.length
  )
    options[args[i].slice(2)] = args[++i];
  else targets.push(args[i]);
}
const outFile = options.out;
if (!outFile) {
  console.error(
    "usage: commitRecords.js --out <page.html> [--pr <number>] [--title <text>] [--markdown <comment.md> [--url <page url>]] <rev | base..tip>...",
  );
  process.exit(1);
}
const heading = {
  pr: (options.pr ?? "").replace(/\D/g, ""),
  title: options.title ?? "",
};
const groups = targets.map(group);
const body = pageBody(groups, heading);
if (options.markdown) {
  writeFileSync(
    options.markdown,
    commentMarkdown(groups, { url: options.url }),
  );
  console.log(options.markdown);
}
const head = `<title>${esc(pageTitle(heading))}</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,600&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&family=STIX+Two+Text:ital,wght@0,400;0,500;1,400&display=swap">
<style>${CSS}</style>`;
writeFileSync(
  outFile,
  `<!doctype html>\n<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n${head}</head><body>${body}</body></html>\n`,
);
console.log(outFile);
