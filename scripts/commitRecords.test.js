import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const script = fileURLToPath(new URL("./commitRecords.js", import.meta.url));

const record = `Compile x + 0 to x

Integer addition of the literal 0 compiles to the operand.

Example (rescript):
    let f = (x: int) => x + 0

Before (js):
    return x + 0 | 0;

After (js):
    return x;

Source: bsc before and after this commit.

Changelog entry: - Compile \`x + 0\` to \`x\`. https://github.com/rescript-lang/rescript/pull/1234

Area: codegen tests
Changelog: polish
Output: changed integer + 0; one snapshot
Formats: none
Tests: +tests/tests/src/a_test.mjs ~tests/tests/src/b_test.mjs
Ran: make (exit 0); make test (exit 0: 3 passing; 0 failing)
Signed-off-by: A Person <a@example.com>
`;

/**
 * Asserts that the page contains the text, without printing the page.
 * @param {string} html
 * @param {string} text
 */
const has = (html, text) =>
  assert.ok(html.includes(text), `page lacks ${JSON.stringify(text)}`);

/**
 * Renders the records of a temporary repository's commits.
 * @param {(git: (...args: string[]) => string, dir: string) => Promise<void>} setup
 * @param {string[]} revs
 */
async function render(setup, revs) {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-records-"));
  const git = (/** @type {string[]} */ ...args) =>
    execFileSync("git", args, { cwd: dir, encoding: "utf8" });
  git("init", "-q", "-b", "main");
  git("config", "user.name", "A Person");
  git("config", "user.email", "a@example.com");
  await setup(git, dir);
  const out = path.join(dir, "page.html");
  execFileSync("node", [script, "--out", out, ...revs], { cwd: dir });
  return fs.readFile(out, "utf8");
}

/**
 * @param {string} dir
 * @param {Record<string, string>} files
 */
async function write(dir, files) {
  for (const [name, content] of Object.entries(files)) {
    await fs.mkdir(path.dirname(path.join(dir, name)), { recursive: true });
    await fs.writeFile(path.join(dir, name), content);
  }
}

test("renders a record and checks it against the diff", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "tests/tests/src/b_test.mjs": "x + 0 | 0\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, {
        "changes/identity.polish.md":
          "- Compile `x + 0` to `x`. https://github.com/rescript-lang/rescript/pull/1234\n",
        "tests/tests/src/a_test.mjs": "x\n",
        "tests/tests/src/b_test.mjs": "x\n",
      });
      git("add", ".");
      git("commit", "-q", "-m", record);
    },
    ["main~1..main"],
  );
  has(html, "<h3>Compile x + 0 to x</h3>");
  has(html, '<span class="ln del">');
  has(html, '<span class="ln add">');
  has(html, '<p class="source"><b>Source</b>');
  has(html, "Changelog entry matches the line the diff adds");
  has(html, "1 committed generated JS file changes; 1 new");
  has(html, "Formats: none");
  has(html, "<code>tests/tests/src/</code>");
  has(html, "<li>make test (exit 0: 3 passing; 0 failing)</li>");
  has(html, "changelog 1");
  has(html, '<td class="num">0</td></tr>');
  has(html, "<li>Compile <code>x + 0</code>");
});

test("flags a record that disagrees with its diff", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "tests/tests/src/b_test.mjs": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, { "tests/tests/src/b_test.mjs": "b\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Change output\n\nArea: codegen\nChangelog: fix\nOutput: unchanged",
      );
    },
    ["HEAD"],
  );
  has(html, "Changelog: fix, but the diff adds no changelog entry");
  has(html, "Output: unchanged, but 1 committed generated JS file changes");
});
