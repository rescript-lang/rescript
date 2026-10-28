#!/usr/bin/env node

// @ts-check

/* Draws commit records as a Semantic Diff page: one card per commit, each
 * the commit's identity and its message drawn. A record is a commit message
 * in the format of the `records` skill (.agents/skills/records/SKILL.md).
 * This file reads git; scripts/recordRender.js draws.
 *
 *   node scripts/commitRecords.js --out review.html <rev>...
 *   node scripts/commitRecords.js --out review.html <base>..<tip>...
 *   --pr <number> and --title <text> head the page with the pull request.
 *   --markdown <file> also draws the records as the pull request's comment
 *   (scripts/recordMarkdown.js), linking the page at --url <url>.
 *   --logins <file>, lines `<sha> <login>`, names the GitHub accounts of
 *   the commits' authors, drawn in place of their names. With
 *   --previous <rev>, the pull request's head before the last push, the
 *   comment marks the commits that push added or whose message it edited.
 *   --check fails when a record has no description, a paragraph of prose
 *   or a `Why:` section, naming those records; it needs no --out.
 */

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { categories, parseFragment } from "./changelogFragments.js";
import { commentMarkdown } from "./recordMarkdown.js";
import {
  CSS,
  described,
  esc,
  pageBody,
  pageTitle,
  parseRecord,
} from "./recordRender.js";

/** @import { CommitRecord, Group } from "./recordRender.js" */
/** @import { ChangelogEntry } from "./recordMarkdown.js" */

/** @param {string[]} args */
const git = args =>
  execFileSync("git", args, {
    encoding: "utf8",
    maxBuffer: 1 << 28,
    stdio: ["ignore", "pipe", "ignore"],
  });

/**
 * The changelog entries a range adds or changes: the fragments under
 * `changes/` that differ between its ends, as the tip has them, breaking
 * changes first and then in the changelog's order. A fragment that doesn't
 * parse is drawn by its name; the changelog check reports why.
 * @param {string} base
 * @param {string} tip
 * @returns {ChangelogEntry[]}
 */
function changelog(base, tip) {
  const order = Object.keys(categories);
  return git([
    "diff",
    "--name-only",
    "--no-renames",
    "--diff-filter=AM",
    base,
    tip,
    "--",
    "changes/",
  ])
    .split("\n")
    .filter(f => /^changes\/[^/]+\.md$/.test(f) && f !== "changes/README.md")
    .map(f => {
      const name = f.slice("changes/".length);
      try {
        const { category, text } = parseFragment(
          name,
          git(["show", `${tip}:${f}`]),
        );
        const heading =
          categories[/** @type {keyof typeof categories} */ (category)];
        return { category, heading, text };
      } catch {
        return {
          category: "",
          heading: "#### :grey_question: Not a changelog fragment",
          text: `- \`${name}\``,
        };
      }
    })
    .sort((a, b) => {
      const rank = (/** @type {ChangelogEntry} */ e) =>
        e.category ? order.indexOf(e.category) : order.length;
      return rank(a) - rank(b);
    });
}

const MONTHS = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split(" ");

/** A time as `Oct 23, 2026, 14:41 UTC`, as the cards and the comment print it. */
const utc = (/** @type {Date} */ d) => {
  const two = (/** @type {number} */ n) => String(n).padStart(2, "0");
  return `${MONTHS[d.getUTCMonth()]} ${two(d.getUTCDate())}, ${d.getUTCFullYear()}, ${two(d.getUTCHours())}:${two(d.getUTCMinutes())} UTC`;
};

/** A commit's message, trimmed. @param {string} rev */
const message = rev => git(["show", "-s", "--format=%B", rev]).trim();

/** The messages of the records read, by short hash. */
const messages = new Map();

/**
 * The record of one commit: its short hash, author, author date (in UTC,
 * to the minute) and message.
 * @param {string} rev
 * @returns {CommitRecord}
 */
function readRecord(rev) {
  const show = (/** @type {string[]} */ ...format) =>
    git(["show", "-s", ...format, rev]);
  const record = parseRecord({
    sha: show("--format=%h", "--abbrev=9").trim(),
    author: show("--format=%an").trim(),
    login: logins.get(show("--format=%H").trim()),
    date: utc(new Date(show("--format=%aI").trim())),
    message: show("--format=%B"),
  });
  messages.set(record.sha, message(rev));
  return record;
}

/**
 * What the last push changed, against the head before it: the records it
 * added (`new`) or whose message it edited under the same subject
 * (`edited`), by short hash. The previous commits are those of `previous`
 * that the range's base lacks.
 * @param {CommitRecord[]} records
 * @param {string} base
 * @param {string} previous
 */
function lastPush(records, base, previous) {
  const before = git([
    "rev-list",
    "--no-merges",
    previous,
    `^${git(["merge-base", base, previous]).trim()}`,
  ])
    .split("\n")
    .filter(Boolean)
    .map(message);
  const subject = (/** @type {string} */ m) => m.split("\n")[0];
  const kept = new Set(before);
  const subjects = new Set(before.map(subject));
  /** @type {Map<string, "new" | "edited">} */
  const changes = new Map();
  for (const r of records) {
    const m = messages.get(r.sha) ?? "";
    if (!kept.has(m))
      changes.set(r.sha, subjects.has(subject(m)) ? "edited" : "new");
  }
  return changes;
}

/**
 * The comment's marks of the last push, or none when the previous head
 * can't be read, so that the comment is still drawn.
 * @param {CommitRecord[]} records
 * @param {string} base
 * @param {string} previous
 */
function marks(records, base, previous) {
  try {
    return { changes: lastPush(records, base, previous) };
  } catch {
    return {};
  }
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
  if (args[i] === "--check") options.check = "true";
  else if (
    [
      "--out",
      "--pr",
      "--title",
      "--markdown",
      "--url",
      "--previous",
      "--logins",
    ].includes(args[i]) &&
    i + 1 < args.length
  )
    options[args[i].slice(2)] = args[++i];
  else targets.push(args[i]);
}
const outFile = options.out;
if (!outFile && !options.check) {
  console.error(
    "usage: commitRecords.js (--out <page.html> [--pr <number>] [--title <text>] [--markdown <comment.md> [--url <page url>] [--previous <rev>]] [--logins <file>] | --check) <rev | base..tip>...",
  );
  process.exit(1);
}
/** The GitHub logins of the commits' authors, by full sha. */
const logins = new Map(
  (options.logins ? readFileSync(options.logins, "utf8") : "")
    .split("\n")
    .map(l => l.trim().split(/\s+/))
    .filter(f => f.length === 2)
    .map(([sha, login]) => [sha, login]),
);
const heading = {
  pr: (options.pr ?? "").replace(/\D/g, ""),
  title: options.title ?? "",
};
const groups = targets.map(group);

/** Draws the page and, with --markdown, the comment. @param {string} outFile */
function draw(outFile) {
  const body = pageBody(groups, heading);
  if (options.markdown) {
    const [base, tip] = targets[targets.length - 1].split("..");
    const records = groups.flatMap(g => g.records);
    writeFileSync(
      options.markdown,
      commentMarkdown(groups, {
        url: options.url,
        ...(tip ? { changelog: changelog(base, tip) } : {}),
        ...(options.previous && tip
          ? marks(records, base, options.previous)
          : {}),
      }),
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
}

/**
 * Names the records without a description and fails, telling how to add
 * one. A pull request's check runs this after its page and comment are
 * drawn, so the comment shows the records as they are.
 */
function check() {
  const missing = groups.flatMap(g => g.records).filter(r => !described(r));
  if (!missing.length) return;
  console.error(
    missing.length === 1
      ? "A commit has no description:\n"
      : `${missing.length} commits have no description:\n`,
  );
  for (const r of missing) console.error(`  ${r.sha} ${r.subject}`);
  console.error(`
Below its subject, a commit message says why the commit is made, as a line
or two of prose or a \`Why:\` section (see "Commit messages" in
CONTRIBUTING.md). Add one with \`git rebase -i\`, marking these commits
\`reword\`, or squash them into a commit that has one, and force-push.`);
  process.exit(1);
}

if (outFile) draw(outFile);
if (options.check) check();
