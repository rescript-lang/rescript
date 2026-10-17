import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import {
  addedByPullRequest,
  categories,
  checkPullRequest,
  linkFragment,
  openSection,
  parseFragment,
  placeholderLink,
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
    ["recovery.fix.md", `Heading\n${note}`],
    ["recovery.fix.md", "- Bad\ncontinuation"],
  ]) {
    assert.throws(() => parseFragment(name, content));
  }
  assert.throws(
    () =>
      parseFragment("recovery.fix.md", `- Fix recovery. ${placeholderLink}`),
    /replace the placeholder/,
  );
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

test("collects late fragments into a finalized, unpublished version", () => {
  const released = prepareRelease(changelog, "13.0.0", [
    parseFragment("early.fix.md", note),
  ]);
  const late = parseFragment(
    "late.fix.md",
    "- Late fix. https://github.com/rescript-lang/rescript/pull/9000",
  );
  const output = prepareRelease(released, "13.0.0", [late]);
  assert.ok(
    output.includes(`# 13.0.0\n\n${categories.breaking}`),
    "keeps the finalized heading",
  );
  assert.ok(output.includes(`${categories.fix}\n\n${late.text}\n${note}\n`));
  assert.ok(output.endsWith(historical));
  assert.throws(() => prepareRelease(released, "13.0.0", []), /finalized/);
  // Publish has tagged the version, so its notes are frozen.
  assert.throws(
    () => prepareRelease(released, "13.0.0", [late], { tagged: true }),
    /already been tagged/,
  );
  // Once the next version is opened, late fragments belong to it instead.
  assert.throws(
    () => prepareRelease(openSection(released, "13.0.1"), "13.0.0", [late]),
    /Expected first version heading/,
  );
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

test("checks pull request fragments and CHANGELOG.md edits", () => {
  const pullRequest = (login, labels = []) => ({
    user: { login },
    labels: labels.map(name => ({ name })),
  });
  const someone = pullRequest("someone");
  const fragment = { status: "A", name: "changes/new.fix.md" };
  const changelogEdit = { status: "M", name: "CHANGELOG.md" };
  checkPullRequest([fragment], someone);
  for (const files of [
    [],
    [{ status: "M", name: "changes/existing.fix.md" }],
    [{ status: "A", name: "changes/README.md" }],
    [fragment, changelogEdit],
  ]) {
    assert.throws(() => checkPullRequest(files, someone));
  }
  checkPullRequest([], pullRequest("someone", ["changelog:skip"]));
  checkPullRequest([], pullRequest("dependabot[bot]"));
  for (const author of [
    pullRequest("someone", ["changelog:skip"]),
    pullRequest("dependabot[bot]"),
  ]) {
    assert.throws(() => checkPullRequest([changelogEdit], author), /CHANGELOG/);
  }
  checkPullRequest(
    [changelogEdit],
    pullRequest("someone", ["changelog:release"]),
  );
});

test("opens the next unreleased section for the following release", () => {
  const released = prepareRelease(changelog, "13.0.0", [
    parseFragment("recovery.fix.md", note),
  ]);
  const opened = openSection(released, "13.0.1");
  assert.equal(
    opened,
    released.replace(
      "# 13.0.0\n",
      `# 13.0.1 (Unreleased)\n\n${Object.values(categories).join("\n\n")}\n\n# 13.0.0\n`,
    ),
  );
  assert.ok(
    prepareRelease(opened, "13.0.1", [
      parseFragment("later.feature.md", note),
    ]).includes(`# 13.0.1\n\n${categories.breaking}`),
  );
  assert.throws(() => openSection(opened, "13.0.2"), /must be released/);
  assert.throws(() => openSection(released, "13.0.0"), /already been released/);
});

test("validates before mutation, sorts fragments, consumes only on release", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-changelog-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  await fs.mkdir(path.join(root, "changes"));
  await fs.writeFile(
    path.join(root, "package.json"),
    JSON.stringify({ version: "13.0.0" }),
  );
  await fs.writeFile(path.join(root, "CHANGELOG.md"), changelog);
  const write = (name, text) =>
    fs.writeFile(path.join(root, "changes", name), text);
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
    (await fs.readdir(path.join(root, "changes"))).includes("z.fix.md"),
  );
  await fs.unlink(path.join(root, "changes", "bad.unknown.md"));
  assert.deepEqual(
    (await readFragments(root)).map(fragment => fragment.name),
    ["a.fix.md", "z.fix.md"],
  );
  await run("release", root);
  assert.deepEqual(await fs.readdir(path.join(root, "changes")), ["README.md"]);
  await assert.rejects(run("release", root));
});

test("links bullets without a link to the pull request that added them", () => {
  const fragment = parseFragment(
    "recovery.fix.md",
    "- Fix recovery\n  after errors.\n- Fix formatting. https://github.com/rescript-lang/rescript/pull/7",
  );
  assert.equal(fragment.unlinked, true);
  assert.equal(fragment.pr, 7);
  const linked = linkFragment(fragment, () => 42);
  assert.equal(
    linked.text,
    "- Fix recovery\n  after errors. https://github.com/rescript-lang/rescript/pull/42\n- Fix formatting. https://github.com/rescript-lang/rescript/pull/7",
  );
  assert.equal(linked.pr, 42);
  const plain = parseFragment("plain.fix.md", "- Fix recovery.");
  assert.equal(plain.pr, null);
  assert.equal(
    linkFragment(parseFragment("linked.fix.md", note), () => {
      throw new Error("not looked up");
    }).text,
    note,
  );
  assert.throws(
    () => linkFragment(plain, () => null),
    /changes\/plain\.fix\.md: no PR link/,
  );
});

test("orders fragments without a link first in previews", () => {
  const linked = parseFragment("a.fix.md", note);
  const unlinked = parseFragment("b.fix.md", "- Not merged yet.");
  assert.ok(
    renderFragments([linked, unlinked]).includes(`- Not merged yet.\n${note}`),
  );
});

test("releases fragments without a link with their pull request's link", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-changelog-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  await fs.mkdir(path.join(root, "changes"));
  await fs.writeFile(
    path.join(root, "package.json"),
    JSON.stringify({ version: "13.0.0" }),
  );
  await fs.writeFile(path.join(root, "CHANGELOG.md"), changelog);
  await fs.writeFile(path.join(root, "changes", "old.fix.md"), note);
  await fs.writeFile(path.join(root, "changes", "new.fix.md"), "- New fix.");
  await fs.writeFile(path.join(root, "changes", "lost.fix.md"), "- Lost.");
  const numbers = { "new.fix.md": 9000, "lost.fix.md": null };
  await assert.rejects(
    run("release", root, name => numbers[name]),
    /changes\/lost\.fix\.md/,
  );
  assert.equal(
    await fs.readFile(path.join(root, "CHANGELOG.md"), "utf8"),
    changelog,
  );
  await fs.unlink(path.join(root, "changes", "lost.fix.md"));
  await run("release", root, name => numbers[name]);
  assert.ok(
    (await fs.readFile(path.join(root, "CHANGELOG.md"), "utf8")).includes(
      `${categories.fix}\n\n- New fix. https://github.com/rescript-lang/rescript/pull/9000\n${note}\n`,
    ),
  );
});

test("finds the pull request in the subject of the commit that added a fragment", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-changelog-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  const git = (...args) =>
    execFileSync("git", args, {
      cwd: root,
      env: {
        ...process.env,
        GIT_AUTHOR_NAME: "a",
        GIT_AUTHOR_EMAIL: "a@example.com",
        GIT_COMMITTER_NAME: "a",
        GIT_COMMITTER_EMAIL: "a@example.com",
      },
    });
  git("init", "-q");
  await fs.mkdir(path.join(root, "changes"));
  const commit = async (name, subject) => {
    await fs.writeFile(path.join(root, "changes", name), "- Note.");
    git("add", ".");
    git("commit", "-q", "-m", subject);
  };
  await commit("squashed.fix.md", "Fix recovery (#123)");
  await commit("rebased.fix.md", "Fix formatting");
  assert.equal(addedByPullRequest(root, "squashed.fix.md"), 123);
  assert.equal(addedByPullRequest(root, "rebased.fix.md"), null);
  assert.equal(addedByPullRequest(root, "missing.fix.md"), null);
  // A later edit by another pull request does not change who added it.
  await fs.writeFile(
    path.join(root, "changes", "squashed.fix.md"),
    "- Edited.",
  );
  git("commit", "-q", "-am", "Reword (#456)");
  assert.equal(addedByPullRequest(root, "squashed.fix.md"), 123);
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
