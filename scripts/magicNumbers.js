import { spawnSync } from "node:child_process";
import fs from "node:fs/promises";
import path from "node:path";
import semver from "semver";

export const configPath = "compiler/ext/config.ml";

// The libraries that define the serialized parsetree and typed tree types,
// and everything those types refer to: `ml` and the libraries it depends on.
export const formatSources = [
  "compiler/ml",
  "compiler/ext",
  "compiler/flow_parser",
];

export const magicNumbers = [
  "cmi_magic_number",
  "cmt_magic_number",
  "ast_impl_magic_number",
  "ast_intf_magic_number",
];

const definition = name => new RegExp(`\\b${name} = "([^"]*)"`);

export function readMagicNumber(config, name) {
  return definition(name).exec(config)?.[1] ?? null;
}

// Increments the trailing counter and keeps its width: ResCmi040 -> ResCmi041.
export function incrementMagicNumber(value) {
  const match = /^(.*?)(\d+)$/.exec(value);
  if (!match) throw new Error(`${value}: expected a trailing counter`);
  const next = String(Number(match[2]) + 1);
  if (next.length > match[2].length) {
    throw new Error(`${value}: the counter is out of digits`);
  }
  return match[1] + next.padStart(match[2].length, "0");
}

// Bumps every magic number that is still the one of the previous release,
// when the format sources changed since then. One that differs was already
// bumped, by an earlier run for the same release or by hand.
export function bumpMagicNumbers(config, previousConfig, sourcesChanged) {
  if (!sourcesChanged) return config;
  let output = config;
  for (const name of magicNumbers) {
    const value = readMagicNumber(config, name);
    if (value === null) throw new Error(`${configPath}: ${name} not found`);
    if (readMagicNumber(previousConfig ?? "", name) !== value) continue;
    output = output.replace(
      definition(name),
      `${name} = "${incrementMagicNumber(value)}"`,
    );
  }
  return output;
}

// The highest release tag below the version being released. The nearest tag
// is not enough: a maintenance branch merged into master brings its newer
// tags, such as v12.3.2 after v13.0.0-alpha.6, with that line's magic numbers.
export function previousRelease(tags, version) {
  let previous = null;
  for (const tag of tags) {
    const parsed = tag.startsWith("v") ? semver.parse(tag.slice(1)) : null;
    if (parsed === null || !semver.lt(parsed, version)) continue;
    if (previous === null || semver.gt(parsed, previous.parsed)) {
      previous = { tag, parsed };
    }
  }
  return previous?.tag ?? null;
}

// The previous release tag among the tags reachable from HEAD.
export function previousReleaseTag(root, version) {
  const result = spawnSync("git", ["tag", "--merged", "HEAD", "--list", "v*"], {
    cwd: root,
    encoding: "utf8",
  });
  const tag =
    result.status === 0
      ? previousRelease(result.stdout.split("\n").filter(Boolean), version)
      : null;
  if (tag === null) {
    throw new Error(
      `No previous release tag found; fetch the tags (git fetch --tags) first.\n${result.stderr}`,
    );
  }
  return tag;
}

function changedSince(root, tag) {
  const result = spawnSync(
    "git",
    ["diff", "--quiet", tag, "HEAD", "--", ...formatSources],
    { cwd: root },
  );
  if (result.status === 0) return false;
  if (result.status === 1) return true;
  throw new Error(`git diff ${tag} HEAD failed: ${result.stderr}`);
}

export function showAt(root, revision, file) {
  const result = spawnSync("git", ["show", `${revision}:${file}`], {
    cwd: root,
    encoding: "utf8",
  });
  return result.status === 0 ? result.stdout : null;
}

const counterPrefix = value => /^(.*?)\d*$/.exec(value)[1];

// A pull request may switch a magic number to another prefix, but only the
// release preparation bumps a counter.
export function checkMagicNumbers(baseConfig, config, labels) {
  if (baseConfig === null || labels.includes("changelog:release")) return;
  const bumped = magicNumbers.filter(name => {
    const before = readMagicNumber(baseConfig, name);
    const after = readMagicNumber(config, name);
    return (
      before !== null &&
      after !== null &&
      before !== after &&
      counterPrefix(before) === counterPrefix(after)
    );
  });
  if (bumped.length) {
    throw new Error(
      `${configPath}: don't bump ${bumped.join(", ")}; yarn changelog:release bumps the magic numbers when a release is prepared.`,
    );
  }
}

// The new contents of config.ml for the release, or null when no magic
// number needs a bump.
export async function releaseMagicNumbers(root, version) {
  const tag = previousReleaseTag(root, version);
  const config = await fs.readFile(path.join(root, configPath), "utf8");
  const output = bumpMagicNumbers(
    config,
    showAt(root, tag, configPath),
    changedSince(root, tag),
  );
  return output === config ? null : output;
}
