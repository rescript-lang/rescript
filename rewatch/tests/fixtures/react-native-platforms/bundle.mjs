import assert from "node:assert/strict";
import { mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import Metro from "metro";
import { build, config, root } from "./build.mjs";

build();
mkdirSync(join(root, "bundles"), { recursive: true });
const metroConfig = await Metro.loadConfig({
  cwd: root,
  config: join(root, "metro.config.cjs"),
});
for (const platform of config.platforms) {
  await Metro.runBuild(metroConfig, {
    entry: "index.cjs",
    platform,
    dev: false,
    minify: false,
    out: join(root, "bundles", `${platform}.bundle.js`),
    sourceMap: true,
  });
  const map = JSON.parse(
    readFileSync(join(root, "bundles", `${platform}.bundle.map`), "utf8"),
  );
  for (const name of ["App", "Button"]) {
    assert(
      map.sources.some(
        path =>
          (name === "Button" &&
            path.endsWith(`/lib/js/src/Button.${platform}.js`)) ||
          (name === "App" && path.endsWith("/lib/js/src/App.js")),
      ),
    );
    for (const other of config.platforms.filter(value => value !== platform)) {
      assert(
        !map.sources.some(path =>
          path.endsWith(`/lib/js/src/${name}.${other}.js`),
        ),
      );
    }
  }
  console.log(`Bundled React Native for ${platform}`);
}
