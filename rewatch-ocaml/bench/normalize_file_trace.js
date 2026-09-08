#!/usr/bin/env node

import fs from "node:fs";
import path from "node:path";

if (process.argv.length !== 5) {
  console.error("Usage: normalize_file_trace.js TRACE_PREFIX FIXTURE OUTPUT_PREFIX");
  process.exit(2);
}

const [, , tracePrefix, fixtureArgument, outputPrefix] = process.argv;
const fixture = path.resolve(fixtureArgument);
const traceDirectory = path.dirname(tracePrefix);
const traceBasename = `${path.basename(tracePrefix)}.`;
const traces = fs
  .readdirSync(traceDirectory)
  .filter((name) => name.startsWith(traceBasename))
  .sort();

const operations = [];
const categories = new Map();

function decodeQuoted(value) {
  try {
    return JSON.parse(`"${value}"`);
  } catch (_) {
    return value;
  }
}

function normalize(cwd, value) {
  if (value === "") return null;
  const absolute = path.isAbsolute(value) ? path.normalize(value) : path.resolve(cwd, value);
  if (absolute !== fixture && !absolute.startsWith(`${fixture}${path.sep}`)) return null;
  const relative = path.relative(fixture, absolute);
  return relative === "" ? "<ROOT>" : `<ROOT>/${relative.split(path.sep).join("/")}`;
}

function category(operation) {
  if (/^(open|openat|openat2|creat)$/.test(operation)) return "open";
  if (/^(stat|statx|lstat|fstatat|newfstatat|access|faccessat|faccessat2|readlink|readlinkat)$/.test(operation)) return "metadata";
  if (/^(mkdir|mkdirat|mknod|mknodat|link|linkat|symlink|symlinkat)$/.test(operation)) return "create";
  if (/^rename/.test(operation)) return "rename";
  if (/^(unlink|unlinkat|rmdir)$/.test(operation)) return "remove";
  if (/^getdents/.test(operation)) return "directory-scan";
  if (operation === "execve") return "execute";
  return "other";
}

function pathValues(operation, line) {
  if (/^getdents/.test(operation)) {
    const descriptorPath = line.match(/^getdents\w*\(\d+<([^>]+)>/);
    return descriptorPath ? [descriptorPath[1]] : [];
  }
  const quoted = [...line.matchAll(/"((?:[^"\\]|\\.)*)"/g)].map((match) =>
    decodeQuoted(match[1]),
  );
  if (/^(rename|renameat|renameat2|link|linkat)$/.test(operation)) return quoted.slice(0, 2);
  if (/^(symlink|symlinkat)$/.test(operation)) return quoted.slice(-1);
  return quoted.slice(0, 1);
}

for (const trace of traces) {
  let cwd = fixture;
  const lines = fs.readFileSync(path.join(traceDirectory, trace), "utf8").split("\n");
  for (const line of lines) {
    const call = line.match(/^([a-zA-Z0-9_]+)\(/);
    if (!call) continue;
    const operation = call[1];
    const values = pathValues(operation, line);
    const callCwd = cwd;
    for (const value of values) {
      const normalized = normalize(callCwd, value);
      if (normalized === null) continue;
      operations.push(`${operation}\t${normalized}`);
      const name = category(operation);
      categories.set(name, (categories.get(name) || 0) + 1);
    }
    if (operation === "chdir" && line.endsWith("= 0") && values.length === 1) {
      cwd = path.isAbsolute(values[0])
        ? path.normalize(values[0])
        : path.resolve(callCwd, values[0]);
    }
  }
}

operations.sort();
const counted = [];
for (let index = 0; index < operations.length; ) {
  let end = index + 1;
  while (end < operations.length && operations[end] === operations[index]) end += 1;
  counted.push(`${end - index}\t${operations[index]}`);
  index = end;
}
fs.writeFileSync(`${outputPrefix}.paths.tsv`, `${counted.join("\n")}\n`);
fs.writeFileSync(
  `${outputPrefix}.categories.tsv`,
  `${[...categories].sort().map(([name, count]) => `${name}\t${count}`).join("\n")}\n`,
);
