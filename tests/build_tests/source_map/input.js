// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execBuildOrThrow();

for (const filename of ["Demo.cjs", "Demo.mjs"]) {
  const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
  const mapPath = `${jsPath}.map`;

  const js = await fs.readFile(jsPath, "utf8");
  assert.match(
    js,
    new RegExp(`//# sourceMappingURL=${filename.replace(".", "\\.")}\\.map`),
  );

  const map = JSON.parse(await fs.readFile(mapPath, "utf8"));
  assert.equal(map.version, 3);
  assert.equal(map.file, filename);
  assert.ok(map.mappings.length > 0, `${filename}.map should include mappings`);
  assert.ok(
    map.sources.some(source => source.endsWith("Demo.res")),
    `${filename}.map should include Demo.res, got ${map.sources.join(", ")}`,
  );
  assert.ok(
    map.sourcesContent.some(content => content.includes("let add = (a, b)")),
    `${filename}.map should include source contents`,
  );
}

await execClean();
