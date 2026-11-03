// Checks that every stdlib and Belt value named `*OrThrow` or `*Exn` declares
// what it throws with `@throws(...)`. The exception analysis reads that
// attribute from the declaration at each call site, so a missing one makes
// calls to the function look as if they throw nothing.
//
// A module's declarations are read from its .resi, or from its .res when it
// has no interface; the compiler copies a binding's `@throws` into the
// signature it infers.

import fs from "node:fs";
import path from "node:path";

const root = path.resolve(import.meta.dirname, "../../..");
const dirs = ["packages/@rescript/runtime", "packages/@rescript/belt/src"];

// Names that end in `Exn` without throwing.
const allowed = new Set(["Stdlib_JsError.toJsExn"]);

const declaration =
  /^\s*(?:let|external)\s+(?:rec\s+)?([A-Za-z_][A-Za-z0-9_']*)\s*[:=]/;
const throwsWithPayload = /@throws\(/;

const missing = [];

for (const dir of dirs) {
  const files = fs.readdirSync(path.join(root, dir));
  for (const file of files) {
    const match = /^(.*)\.(resi?)$/.exec(file);
    if (match === null) continue;
    const [, module, ext] = match;
    if (ext === "res" && files.includes(`${module}.resi`)) continue;
    const lines = fs
      .readFileSync(path.join(root, dir, file), "utf8")
      .split("\n");
    let commentDepth = 0;
    lines.forEach((line, i) => {
      const inComment = commentDepth > 0;
      commentDepth +=
        (line.match(/\/\*/g) ?? []).length - (line.match(/\*\//g) ?? []).length;
      if (inComment) return;
      const decl = declaration.exec(line);
      if (decl === null) return;
      const name = decl[1];
      if (!/(OrThrow|Exn)$/.test(name)) return;
      if (allowed.has(`${module}.${name}`)) return;
      // The attributes of a declaration are on its line and the lines above
      // it, up to a blank line or the end of its doc comment.
      let annotated = throwsWithPayload.test(line);
      for (let j = i - 1; !annotated && j >= 0; j--) {
        const above = lines[j].trim();
        if (above === "" || above.endsWith("*/")) break;
        annotated = throwsWithPayload.test(above);
      }
      if (!annotated) missing.push(`${dir}/${file}:${i + 1}: ${name}`);
    });
  }
}

if (missing.length > 0) {
  console.error(
    "These declarations need a @throws(...) naming what they throw:",
  );
  for (const m of missing) console.error(`  ${m}`);
  process.exit(1);
}
console.log("✅ Every *OrThrow and *Exn declaration has a @throws.");
