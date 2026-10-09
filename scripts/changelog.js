import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const categories = {
  breaking: "#### :boom: Breaking Change",
  compliance: "#### :eyeglasses: Spec Compliance",
  feature: "#### :rocket: New Feature",
  fix: "#### :bug: Bug fix",
  docs: "#### :memo: Documentation",
  polish: "#### :nail_care: Polish",
  internal: "#### :house: Internal",
};

const prLink =
  /https:\/\/github\.com\/rescript-lang\/rescript(?:-compiler)?\/pull\/([1-9]\d*)/;
const trailingPrLinks = new RegExp(
  `${prLink.source}(?:\\s+${prLink.source})*$`,
);

export function parseFragment(name, content) {
  const match = /^([a-z0-9][a-z0-9-]*)\.([a-z]+)\.md$/.exec(name);
  if (!match || !Object.hasOwn(categories, match[2])) {
    throw new Error(
      `${name}: expected <description>.<category>.md; allowed categories: ${Object.keys(categories).join(", ")}`,
    );
  }
  const text = content.trim();
  const bullets = text.split(/\n(?=- )/);
  const invalid = bullets.some(
    bullet =>
      !/^- \S/.test(bullet) ||
      !trailingPrLinks.test(bullet) ||
      bullet
        .split("\n")
        .slice(1)
        .some(line => line && !/^ {2}\S/.test(line)),
  );
  if (!text || invalid) {
    throw new Error(
      `${name}: expected Markdown bullets ending with PR links (indent continuation lines by two spaces)`,
    );
  }
  const pr = Math.max(
    ...[...text.matchAll(new RegExp(prLink, "g"))].map(link => Number(link[1])),
  );
  return { name, category: match[2], text, pr };
}

export async function readFragments(root) {
  const directory = path.join(root, "changelog");
  const entries = await fs.readdir(directory, { withFileTypes: true });
  const fragments = [];
  entries.sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
  for (const entry of entries) {
    if (entry.name === "README.md") continue;
    if (!entry.isFile()) {
      throw new Error(
        `changelog/${entry.name}: expected a regular fragment file`,
      );
    }
    fragments.push(
      parseFragment(
        entry.name,
        await fs.readFile(path.join(directory, entry.name), "utf8"),
      ),
    );
  }
  return fragments;
}

// Newest PR first, matching the order of existing CHANGELOG.md entries.
function categoryNotes(fragments, category) {
  return fragments
    .filter(fragment => fragment.category === category)
    .sort(
      (a, b) => b.pr - a.pr || (a.name < b.name ? -1 : a.name > b.name ? 1 : 0),
    )
    .map(fragment => fragment.text);
}

export function renderFragments(fragments) {
  return Object.entries(categories)
    .map(([category, heading]) => {
      const notes = categoryNotes(fragments, category);
      return `${heading}\n\n${notes.length ? `${notes.join("\n")}\n\n` : ""}`;
    })
    .join("");
}

export function prepareRelease(changelog, version, fragments) {
  const headings = [...changelog.matchAll(/^# (.+)$/gm)].filter(
    match => match[1] !== "Changelog",
  );
  const first = headings[0];
  if (!first || first[1] !== `${version} (Unreleased)`) {
    throw new Error(
      `Expected first version heading to be ${version} (Unreleased)`,
    );
  }
  const end = headings[1]?.index ?? changelog.length;
  const lines = changelog.slice(first.index, end).split("\n");
  // Keep existing unreleased entries during migration, and preserve history verbatim.
  for (const [category, heading] of Object.entries(categories)) {
    const notes = categoryNotes(fragments, category);
    if (!notes.length) continue;
    const index = lines.indexOf(heading);
    if (index === -1 || lines.lastIndexOf(heading) !== index) {
      throw new Error(`Expected exactly one ${heading} section`);
    }
    let next = index + 1;
    while (next < lines.length && lines[next] === "") next++;
    // New notes go above existing entries in the same list; otherwise keep the
    // blank lines that separated the heading from what follows.
    const blanks = lines.slice(index + 1, next);
    const after = lines[next]?.startsWith("- ")
      ? []
      : blanks.length
        ? blanks
        : [""];
    lines.splice(
      index + 1,
      blanks.length,
      "",
      ...notes.join("\n").split("\n"),
      ...after,
    );
  }
  lines[0] = `# ${version}`;
  return (
    changelog.slice(0, first.index) + lines.join("\n") + changelog.slice(end)
  );
}

export function isExempt(pullRequest) {
  const labels = pullRequest.labels.map(label => label.name);
  return (
    labels.includes("changelog:skip") ||
    labels.includes("changelog:release") ||
    pullRequest.user.login === "dependabot[bot]"
  );
}

export function checkRequirement(files, exempt = false) {
  if (exempt) return;
  if (
    !files.some(
      file =>
        file.status === "A" &&
        /^changelog\/(?!README\.md$)[^/]+\.md$/.test(file.name),
    )
  ) {
    throw new Error(
      "Add a changelog fragment, or ask a maintainer for the changelog:skip label. Release PRs use changelog:release.",
    );
  }
}

export async function run(command, root = process.cwd()) {
  if (!["check", "preview", "release"].includes(command)) {
    throw new Error("Usage: node scripts/changelog.js check|preview|release");
  }
  const fragments = await readFragments(root);
  if (command === "check") {
    if (process.env.GITHUB_EVENT_NAME === "pull_request") {
      const { pull_request: pullRequest } = JSON.parse(
        await fs.readFile(process.env.GITHUB_EVENT_PATH, "utf8"),
      );
      const diff = execFileSync(
        "git",
        [
          "diff",
          "--no-renames",
          "--name-status",
          "-z",
          `${pullRequest.base.sha}...HEAD`,
        ],
        { cwd: root, encoding: "utf8" },
      ).split("\0");
      const files = [];
      for (let i = 0; i + 1 < diff.length; i += 2) {
        files.push({ status: diff[i], name: diff[i + 1] });
      }
      checkRequirement(files, isExempt(pullRequest));
    }
    return;
  }
  if (command === "preview") {
    process.stdout.write(renderFragments(fragments));
    return;
  }
  const { version } = JSON.parse(
    await fs.readFile(path.join(root, "package.json"), "utf8"),
  );
  const target = path.join(root, "CHANGELOG.md");
  const output = prepareRelease(
    await fs.readFile(target, "utf8"),
    version,
    fragments,
  );
  await fs.writeFile(target, output);
  for (const fragment of fragments) {
    await fs.unlink(path.join(root, "changelog", fragment.name));
  }
}

if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  run(process.argv[2]).catch(error => {
    console.error(error.message);
    process.exitCode = 1;
  });
}
