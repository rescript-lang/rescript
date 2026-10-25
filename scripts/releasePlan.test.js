import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import {
  createReleasePlan,
  findFirstReleasedVersion,
  findPendingFragments,
} from "./releasePlan.js";

const websiteTargets = [
  ["12.3.1", "v12"],
  ["12.4.0-alpha.1", "v12"],
  ["13.0.0-alpha.7", "v13"],
  ["13.0.0", "v13"],
  ["13.0.1", "v13"],
  ["14.0.0-alpha.1", "v14"],
  ["14.0.0", "v14"],
];

test("plans a maintenance release", () => {
  assert.deepEqual(createReleasePlan("12.3.0"), {
    version: "12.3.0",
    gitTag: "v12.3.0",
    currentStable: false,
    npmTag: "latest-12",
    publishTag: "latest-12",
    websiteBranch: "v12",
  });
});

test("plans a current stable release", () => {
  const plan = createReleasePlan("13.0.0", true);
  assert.equal(plan.npmTag, "latest");
  assert.equal(plan.publishTag, null);
  assert.equal(plan.websiteBranch, "v13");
});

test("derives the prerelease tag", () => {
  assert.equal(createReleasePlan("13.0.0-alpha.6").npmTag, "next-13");
});

test("derives the prerelease tag for an older major", () => {
  assert.equal(createReleasePlan("12.4.0-beta.1").npmTag, "next-12");
});

test("routes stable and prerelease API docs by compiler major", () => {
  for (const [version, branch] of websiteTargets) {
    assert.equal(createReleasePlan(version).websiteBranch, branch);
  }
});

test("promoting a release changes its npm tag without moving its docs", () => {
  for (const [version, branch] of websiteTargets) {
    if (version.includes("-")) continue;
    const maintenance = createReleasePlan(version);
    const stable = createReleasePlan(version, true);
    assert.notEqual(maintenance.npmTag, stable.npmTag);
    assert.equal(maintenance.websiteBranch, branch);
    assert.equal(stable.websiteBranch, branch);
  }
});

test("exports the selected website branch for release workflows", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-api-docs-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  const script = fileURLToPath(new URL("./releasePlan.js", import.meta.url));

  for (const [version, branch] of websiteTargets) {
    const output = path.join(root, `${version}.output`);
    const summary = path.join(root, `${version}.summary`);
    const result = spawnSync(
      process.execPath,
      [
        script,
        "--version",
        version,
        "--github-output",
        output,
        "--github-summary",
        summary,
      ],
      { encoding: "utf8" },
    );
    assert.equal(result.status, 0, result.stderr);
    assert.ok(
      (await fs.readFile(output, "utf8")).includes(
        `website-branch=${branch}\n`,
      ),
    );
    assert.ok(
      (await fs.readFile(summary, "utf8")).includes(
        `| Website branch | \`${branch}\` |`,
      ),
    );
  }
});

test("keeps CI routing when master advances and a maintenance branch is cut", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-api-docs-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  const cases = [
    ["maintenance/12.x", "12.3.1", "v12"],
    ["master", "13.0.0-alpha.7", "v13"],
    ["maintenance/13.x", "13.0.1", "v13"],
    ["master", "14.0.0-alpha.1", "v14"],
  ];
  for (const [index, [ref, version, branch]] of cases.entries()) {
    await fs.writeFile(
      path.join(root, "package.json"),
      JSON.stringify({ version }),
    );
    const output = path.join(root, `github-output-${index}`);
    const result = spawnSync(
      process.execPath,
      [
        fileURLToPath(new URL("./releasePlan.js", import.meta.url)),
        "--github-output",
        output,
      ],
      {
        cwd: root,
        encoding: "utf8",
        env: {
          ...process.env,
          GITHUB_REF: `refs/heads/${ref}`,
          GITHUB_REF_NAME: ref,
        },
      },
    );
    assert.equal(result.status, 0, result.stderr);
    assert.ok(
      (await fs.readFile(output, "utf8")).includes(
        `website-branch=${branch}\n`,
      ),
      `${ref} at ${version} should use ${branch}`,
    );
  }
});

test("rejects promoting a prerelease to current stable", () => {
  assert.throws(
    () => createReleasePlan("13.0.0-alpha.6", true),
    /prerelease cannot be the current stable/,
  );
});

test("rejects non-canonical and invalid versions", () => {
  assert.throws(() => createReleasePlan("v13.0.0"));
  assert.throws(() => createReleasePlan("13.0"));
  assert.throws(() => createReleasePlan("13.0.0+build.1"));
});

test("finds the first released version after the changelog title", () => {
  const changelog = `# Changelog

> Tags and other introductory content

# 13.0.0-alpha.6

#### New Feature

# 13.0.0-alpha.5
`;
  assert.equal(findFirstReleasedVersion(changelog), "13.0.0-alpha.6");
});

test("ignores unreleased changelog headings", () => {
  const changelog = `# Changelog

# 13.0.0-alpha.6 (Unreleased)

# 13.0.0-alpha.5
`;
  assert.equal(findFirstReleasedVersion(changelog), "13.0.0-alpha.5");
});

test("finds changelog fragments left unreleased", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-release-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  assert.deepEqual(await findPendingFragments(root), []);
  await fs.mkdir(path.join(root, "changes"));
  await fs.writeFile(path.join(root, "changes", "README.md"), "");
  assert.deepEqual(await findPendingFragments(root), []);
  await fs.writeFile(path.join(root, "changes", "late.fix.md"), "");
  assert.deepEqual(await findPendingFragments(root), ["late.fix.md"]);
});
