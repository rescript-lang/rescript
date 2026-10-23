import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  checkMagicNumbers,
  configPath,
  releaseMagicNumbers,
  showAt,
} from "./magicNumbers.js";

export const categories = {
  breaking: "#### :boom: Breaking Change",
  feature: "#### :rocket: New Feature",
  fix: "#### :bug: Bug fix",
  docs: "#### :memo: Documentation",
  polish: "#### :nail_care: Polish",
  internal: "#### :house: Internal",
};

const prLink =
  /https:\/\/github\.com\/rescript-lang\/rescript(?:-compiler)?\/pull\/([1-9]\d*)/;
export const placeholderLink =
  "https://github.com/rescript-lang/rescript/pull/XXXX";
const trailingPrLinks = new RegExp(
  `${prLink.source}(?:\\s+${prLink.source})*$`,
);
const repositoryUrl = "https://github.com/rescript-lang/rescript";

export function parseFragment(name, content) {
  const match = /^([a-z0-9][a-z0-9-]*)\.([a-z]+)\.md$/.exec(name);
  if (!match || !Object.hasOwn(categories, match[2])) {
    throw new Error(
      `${name}: expected <description>.<category>.md; allowed categories: ${Object.keys(categories).join(", ")}`,
    );
  }
  const text = content.trim();
  if (text.includes(placeholderLink)) {
    throw new Error(
      `${name}: replace the placeholder ${placeholderLink} with the PR link`,
    );
  }
  const bullets = text.split(/\n(?=- )/);
  const invalid = bullets.some(
    bullet =>
      !/^- \S/.test(bullet) ||
      bullet
        .split("\n")
        .slice(1)
        .some(line => line && !/^ {2}\S/.test(line)),
  );
  if (!text || invalid) {
    throw new Error(
      `${name}: expected Markdown bullets (indent continuation lines by two spaces)`,
    );
  }
  const links = [...text.matchAll(new RegExp(prLink, "g"))].map(link =>
    Number(link[1]),
  );
  // A bullet without a link gets the link of the pull request that added the
  // fragment when the release is prepared.
  const unlinked = bullets.some(bullet => !trailingPrLinks.test(bullet));
  const pr = links.length ? Math.max(...links) : null;
  return { name, category: match[2], text, pr, unlinked };
}

// The pull request that added a fragment, from the subject of the squash
// commit that added it ("... (#1234)"). Rebase merges and merge commits carry
// no such subject, which is why pull requests are squash-merged.
export function addedByPullRequest(root, name) {
  const subjects = execFileSync(
    "git",
    ["log", "--diff-filter=A", "--format=%s", "--", `changes/${name}`],
    { cwd: root, encoding: "utf8" },
  )
    .split("\n")
    .filter(Boolean);
  // The most recent addition, in case an earlier fragment had the same name.
  const match = /\(#([1-9]\d*)\)\s*$/.exec(subjects[0] ?? "");
  return match ? Number(match[1]) : null;
}

// Appends the link of the pull request that added the fragment to each bullet
// without one.
export function linkFragment(fragment, pullRequestOf) {
  if (!fragment.unlinked) return fragment;
  const pr = pullRequestOf(fragment.name);
  if (!pr) {
    throw new Error(
      `changes/${fragment.name}: no PR link, and the commit that added it does not name its pull request; add the link to the fragment`,
    );
  }
  const text = fragment.text
    .split(/\n(?=- )/)
    .map(bullet =>
      trailingPrLinks.test(bullet)
        ? bullet
        : `${bullet} ${repositoryUrl}/pull/${pr}`,
    )
    .join("\n");
  return {
    ...fragment,
    text,
    pr: Math.max(pr, fragment.pr ?? 0),
    unlinked: false,
  };
}

// What the release will write for a fragment: linked once its pull request is
// merged, and as written before that.
export function previewFragment(fragment, pullRequestOf) {
  if (!fragment.unlinked) return fragment;
  const pr = pullRequestOf(fragment.name);
  return pr ? linkFragment(fragment, () => pr) : fragment;
}

export async function readFragments(root) {
  const directory = path.join(root, "changes");
  const entries = await fs.readdir(directory, { withFileTypes: true });
  const fragments = [];
  entries.sort((a, b) => (a.name < b.name ? -1 : a.name > b.name ? 1 : 0));
  for (const entry of entries) {
    if (entry.name === "README.md") continue;
    if (!entry.isFile()) {
      throw new Error(
        `changes/${entry.name}: expected a regular fragment file`,
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

// Newest PR first, matching the order of existing CHANGELOG.md entries. A
// fragment without a link yet belongs to an unmerged pull request, so it comes
// first.
function categoryNotes(fragments, category) {
  const order = fragment => fragment.pr ?? Number.POSITIVE_INFINITY;
  return fragments
    .filter(fragment => fragment.category === category)
    .sort(
      (a, b) =>
        Math.sign(order(b) - order(a) || 0) ||
        (a.name < b.name ? -1 : a.name > b.name ? 1 : 0),
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

function versionHeadings(changelog) {
  return [...changelog.matchAll(/^# (.+)$/gm)].filter(
    match => match[1] !== "Changelog",
  );
}

export function isFinalized(changelog, version) {
  return versionHeadings(changelog)[0]?.[1] === version;
}

// Publish pushes the vX tag before publishing and refuses to move it, so a
// tagged version can no longer take more notes.
export function releaseTagExists(repositoryUrl, version) {
  const url = repositoryUrl.replace(/^git\+/, "");
  return (
    execFileSync("git", ["ls-remote", "--tags", url, `refs/tags/v${version}`], {
      encoding: "utf8",
    }).trim() !== ""
  );
}

export function prepareRelease(changelog, version, fragments, { tagged } = {}) {
  const headings = versionHeadings(changelog);
  const first = headings[0];
  // A version finalized by an earlier run but not yet published can still
  // collect fragments merged in the meantime.
  const finalized = isFinalized(changelog, version);
  if (!first || (first[1] !== `${version} (Unreleased)` && !finalized)) {
    throw new Error(
      `Expected first version heading to be ${version} (Unreleased)`,
    );
  }
  if (finalized && !fragments.length) {
    throw new Error(`${version} is already finalized and no fragments remain`);
  }
  if (finalized && tagged) {
    throw new Error(
      `${version} has already been tagged for publishing; run yarn changelog:open for the next version first`,
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

// Adds an empty unreleased section with every category heading, so that the
// next release can insert fragments into it.
export function openSection(changelog, version) {
  const [first] = versionHeadings(changelog);
  if (first?.[1].endsWith(" (Unreleased)")) {
    throw new Error(`${first[1]} must be released first`);
  }
  if (first?.[1] === version) {
    throw new Error(`${version} has already been released`);
  }
  const index = first?.index ?? changelog.length;
  return (
    changelog.slice(0, index) +
    `# ${version} (Unreleased)\n\n${Object.values(categories).join("\n\n")}\n\n` +
    changelog.slice(index)
  );
}

export function checkPullRequest(files, pullRequest) {
  const labels = pullRequest.labels.map(label => label.name);
  if (
    files.some(file => file.name === "CHANGELOG.md") &&
    !labels.includes("changelog:release")
  ) {
    throw new Error(
      "Add a changelog fragment instead of editing CHANGELOG.md. Release PRs use the changelog:release label.",
    );
  }
  if (
    labels.includes("changelog:skip") ||
    labels.includes("changelog:release") ||
    pullRequest.user.login === "dependabot[bot]"
  ) {
    return;
  }
  if (
    !files.some(
      file =>
        file.status === "A" &&
        /^changes\/(?!README\.md$)[^/]+\.md$/.test(file.name),
    )
  ) {
    throw new Error(
      "Add a changelog fragment, or ask a maintainer for the changelog:skip label. Release PRs use changelog:release.",
    );
  }
}

export async function run(
  command,
  root = process.cwd(),
  pullRequestOf = name => addedByPullRequest(root, name),
  magicNumbersFor = version => releaseMagicNumbers(root, version),
) {
  if (!["check", "preview", "release", "open"].includes(command)) {
    throw new Error(
      "Usage: node scripts/changelog.js check|preview|release|open",
    );
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
      checkPullRequest(files, pullRequest);
      checkMagicNumbers(
        showAt(root, pullRequest.base.sha, configPath),
        await fs.readFile(path.join(root, configPath), "utf8"),
        pullRequest.labels.map(label => label.name),
      );
    } else if (process.env.GITHUB_EVENT_NAME === "push") {
      // Once merged, every fragment must resolve to its pull request, so a
      // merge without "(#N)" in its subject fails here rather than at release.
      for (const fragment of fragments) linkFragment(fragment, pullRequestOf);
    }
    return;
  }
  if (command === "preview") {
    process.stdout.write(
      renderFragments(
        fragments.map(fragment => previewFragment(fragment, pullRequestOf)),
      ),
    );
    return;
  }
  const { version, repository } = JSON.parse(
    await fs.readFile(path.join(root, "package.json"), "utf8"),
  );
  const target = path.join(root, "CHANGELOG.md");
  const changelog = await fs.readFile(target, "utf8");
  if (command === "open") {
    await fs.writeFile(target, openSection(changelog, version));
    return;
  }
  const linked = fragments.map(fragment =>
    linkFragment(fragment, pullRequestOf),
  );
  const tagged =
    linked.length > 0 &&
    isFinalized(changelog, version) &&
    releaseTagExists(repository.url, version);
  const released = prepareRelease(changelog, version, linked, { tagged });
  // The release bumps the magic numbers of the formats that changed, so that
  // pull requests never touch them.
  const config = await magicNumbersFor(version);
  await fs.writeFile(target, released);
  if (config !== null) await fs.writeFile(path.join(root, configPath), config);
  for (const fragment of fragments) {
    await fs.unlink(path.join(root, "changes", fragment.name));
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
