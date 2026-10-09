import assert from "node:assert/strict";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import {
  categories,
  checkRequirement,
  isExempt,
  parseFragment,
  prepareRelease,
  readFragments,
  renderFragments,
  run,
} from "./changelog.js";

const note =
  "- Fix recovery. https://github.com/rescript-lang/rescript/pull/8667";
const historical = "# 12.0.0\n\nOld notes.\n";
const changelog = `# Changelog\n\n# 13.0.0 (Unreleased)\n\n${Object.values(categories).join("\n\n")}\n\n${historical}`;

test("validates category names and bullet content", () => {
  assert.equal(parseFragment("recovery.fix.md", note).category, "fix");
  for (const [name, content] of [
    ["recovery.bugfix.md", note],
    ["recovery.md", note],
    ["recovery.fix.md", ""],
    ["recovery.fix.md", "- Missing link"],
    ["recovery.fix.md", `Heading\n${note}`],
  ]) {
    assert.throws(() => parseFragment(name, content));
  }
  assert.equal(
    parseFragment("recovery.fix.md", `${note}\n${note}`).text,
    `${note}\n${note}`,
  );
});

test("preserves existing notes and historical releases", () => {
  const input = changelog.replace(
    `${categories.fix}\n`,
    `${categories.fix}\n\n- Existing fix.\n`,
  );
  const output = prepareRelease(input, "13.0.0", [
    parseFragment("recovery.fix.md", note),
  ]);
  assert.ok(output.includes(note));
  assert.ok(output.includes("- Existing fix."));
  assert.ok(output.endsWith(historical));
  assert.ok(output.includes("# 13.0.0\n"));
  assert.throws(() => prepareRelease(output, "13.0.0", []));
  assert.throws(() => prepareRelease(input, "14.0.0", []));
});

test("orders notes newest PR first above existing entries", () => {
  const input = changelog.replace(
    `${categories.fix}\n`,
    `${categories.fix}\n\n- Existing fix.\n`,
  );
  const older = parseFragment(
    "a.fix.md",
    "- Older. https://github.com/rescript-lang/rescript/pull/10",
  );
  const newer = parseFragment(
    "b.fix.md",
    "- Newer. https://github.com/rescript-lang/rescript/pull/9 https://github.com/rescript-lang/rescript/pull/20",
  );
  const output = prepareRelease(input, "13.0.0", [older, newer]);
  assert.ok(
    output.includes(
      `${categories.fix}\n\n${newer.text}\n${older.text}\n- Existing fix.\n\n${categories.docs}`,
    ),
  );
  assert.ok(
    renderFragments([older, newer]).includes(`${newer.text}\n${older.text}`),
  );
});

test("keeps section spacing when a section is empty or last", () => {
  const output = prepareRelease(changelog, "13.0.0", [
    parseFragment("a.fix.md", note),
    parseFragment("b.internal.md", note),
  ]);
  assert.ok(
    output.includes(`${categories.fix}\n\n${note}\n\n${categories.docs}`),
  );
  assert.ok(
    output.endsWith(`${categories.internal}\n\n${note}\n\n${historical}`),
  );
});

test("inserts notes containing replacement patterns verbatim", () => {
  const text =
    "- Handle `$$`, `$&`, `$'` and `$\u0060`. https://github.com/rescript-lang/rescript/pull/1";
  const output = prepareRelease(changelog, "13.0.0", [
    parseFragment("dollar.fix.md", text),
  ]);
  assert.ok(output.includes(`${categories.fix}\n\n${text}\n`));
});

test("exempts labelled and Dependabot pull requests", () => {
  const pullRequest = (login, labels = []) => ({
    user: { login },
    labels: labels.map(name => ({ name })),
  });
  assert.equal(isExempt(pullRequest("someone")), false);
  assert.equal(isExempt(pullRequest("someone", ["changelog:skip"])), true);
  assert.equal(isExempt(pullRequest("someone", ["changelog:release"])), true);
  assert.equal(isExempt(pullRequest("dependabot[bot]")), true);
});

test("requires newly added fragments, with explicit exemptions", () => {
  assert.throws(() =>
    checkRequirement([{ status: "M", name: "changelog/existing.fix.md" }]),
  );
  assert.throws(() =>
    checkRequirement([{ status: "A", name: "changelog/README.md" }]),
  );
  checkRequirement([{ status: "A", name: "changelog/new.fix.md" }]);
  checkRequirement([], true);
});

test("validates before mutation, sorts fragments, consumes only on release", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-changelog-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  await fs.mkdir(path.join(root, "changelog"));
  await fs.writeFile(
    path.join(root, "package.json"),
    JSON.stringify({ version: "13.0.0" }),
  );
  await fs.writeFile(path.join(root, "CHANGELOG.md"), changelog);
  const write = (name, text) =>
    fs.writeFile(path.join(root, "changelog", name), text);
  await write("README.md", "Instructions");
  await write("z.fix.md", note);
  await write("a.fix.md", note.replace("recovery", "formatting"));
  await write("bad.unknown.md", note);
  await assert.rejects(run("release", root));
  assert.equal(
    await fs.readFile(path.join(root, "CHANGELOG.md"), "utf8"),
    changelog,
  );
  assert.ok(
    (await fs.readdir(path.join(root, "changelog"))).includes("z.fix.md"),
  );
  await fs.unlink(path.join(root, "changelog", "bad.unknown.md"));
  assert.deepEqual(
    (await readFragments(root)).map(fragment => fragment.name),
    ["a.fix.md", "z.fix.md"],
  );
  await run("release", root);
  assert.deepEqual(await fs.readdir(path.join(root, "changelog")), [
    "README.md",
  ]);
  await assert.rejects(run("release", root));
});

test("preview groups categories without consuming fragments", () => {
  const fragments = [parseFragment("recovery.fix.md", note)];
  const preview = renderFragments(fragments);
  assert.ok(preview.includes(`${categories.fix}\n\n${note}`));
  assert.ok(
    preview.indexOf(categories.breaking) < preview.indexOf(categories.fix),
  );
  assert.equal(fragments.length, 1);
});
