// Usage: node check_snapshot.js SNAPSHOT ACTUAL [ROOT_NAME=PATH...]
//        node check_snapshot.js --normalize OUTPUT ACTUAL [ROOT_NAME=PATH...]
//
// Normalizes ACTUAL and compares it with the checked-in SNAPSHOT, or with
// --normalize writes it to OUTPUT. Every spelling of a root path (with
// forward, backward or escaped backward slashes, as in a quoted string)
// becomes <ROOT_NAME> and CRLF becomes LF. On Windows backslashes also become
// slashes, so one snapshot serves Unix and Windows; elsewhere they are kept,
// so a snapshot can hold an escape such as \n and a path printed with
// backslashes on Unix is a difference. With UPDATE_SNAPSHOTS=1 the snapshot
// is written instead.
import fs from "node:fs";
import path from "node:path";

const args = process.argv.slice(2);
const normalizeOnly = args[0] === "--normalize";
if (normalizeOnly) args.shift();
const [snapshot, actual, ...roots] = args;
if (snapshot === undefined || actual === undefined) {
  throw new Error(
    "Usage: check_snapshot.js [--normalize] SNAPSHOT ACTUAL [NAME=PATH...]",
  );
}

let text = fs.readFileSync(actual, "utf8");
// Longer paths first, so a root inside another root keeps its own name.
const replacements = roots
  .map(root => {
    const separator = root.indexOf("=");
    return { name: root.slice(0, separator), spelling: root.slice(separator + 1) };
  })
  .filter(({ spelling }) => spelling !== "")
  .flatMap(({ name, spelling }) => [
    { name, spelling },
    { name, spelling: spelling.replaceAll("/", "\\") },
    { name, spelling: spelling.replaceAll("/", "\\\\") },
  ])
  .sort((a, b) => b.spelling.length - a.spelling.length);
for (const { name, spelling } of replacements) {
  text = text.split(spelling).join(`<${name}>`);
}
text = text.replaceAll("\r\n", "\n");
if (process.platform === "win32") {
  text = text.replaceAll("\\\\", "/").replaceAll("\\", "/");
}

if (normalizeOnly) {
  fs.writeFileSync(snapshot, text);
  process.exit(0);
}

if (process.env.UPDATE_SNAPSHOTS === "1") {
  fs.mkdirSync(path.dirname(snapshot), { recursive: true });
  fs.writeFileSync(snapshot, text);
  process.exit(0);
}

let expected;
try {
  expected = fs.readFileSync(snapshot, "utf8");
} catch {
  console.error(`Missing snapshot ${snapshot}; run with UPDATE_SNAPSHOTS=1`);
  process.exit(1);
}
if (expected !== text) {
  console.error(`Output differs from snapshot ${snapshot}`);
  console.error("--- expected ---");
  console.error(expected);
  console.error("--- actual ---");
  console.error(text);
  process.exit(1);
}
