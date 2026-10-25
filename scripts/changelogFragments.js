// Changelog fragments (changes/<description>.<category>.md): their
// categories and their parsing. This file imports nothing, so that scripts
// run without node_modules can use it, such as the Semantic Diff workflow's
// scripts/commitRecords.js.

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
export const trailingPrLinks = new RegExp(
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
