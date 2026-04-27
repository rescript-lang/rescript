// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execBuildOrThrow();

const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", "Demo.js");
const mapPath = `${jsPath}.map`;

const js = await fs.readFile(jsPath, "utf8");
assert.match(js, /\/\/# sourceMappingURL=Demo\.js\.map/);

const map = JSON.parse(await fs.readFile(mapPath, "utf8"));
assert.equal(map.version, 3);
assert.equal(map.file, "Demo.js");
assert.ok(map.mappings.length > 0, "source map should include mappings");
assert.ok(
  map.sources.some(source => source.endsWith("Demo.res")),
  `source map should include Demo.res, got ${map.sources.join(", ")}`,
);
assert.ok(
  map.sourcesContent.some(content => content.includes("let add = (a, b)")),
  "source map should include source contents",
);

await execClean();
