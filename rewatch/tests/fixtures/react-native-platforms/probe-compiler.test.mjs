import assert from "node:assert/strict";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  realpathSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { createRequire } from "node:module";
import { join, relative } from "node:path";
import vm from "node:vm";
import { resolve as metroResolve } from "metro-resolver";
import {
  build,
  compile,
  config,
  expectSuccess,
  root,
} from "./probe-compiler.mjs";

const require = createRequire(import.meta.url);
const react = require("react");
const reactJsxRuntime = require("react/jsx-runtime");

function resolveModule(originModulePath, request, platform) {
  return metroResolve(
    {
      originModulePath,
      assetExts: new Set(),
      sourceExts: ["js"],
      preferNativePlatform: true,
      mainFields: ["react-native", "main"],
      getPackageForModule: () => null,
      getPackage: () => null,
      doesFileExist: existsSync,
      fileSystemLookup(path) {
        if (!existsSync(path)) return { exists: false };
        return {
          exists: true,
          type: statSync(path).isDirectory() ? "d" : "f",
          realPath: realpathSync(path),
        };
      },
    },
    request,
    platform,
  ).filePath;
}

// Execute generated modules with Metro's actual resolver and real React.
// Native UI rendering alone is stubbed; complete RN bundles are tested separately.
function load(file, platform, cache = new Map()) {
  if (cache.has(file)) return cache.get(file).exports;
  const module = { exports: {} };
  cache.set(file, module);
  const localRequire = request => {
    if (request === "react/jsx-runtime") return reactJsxRuntime;
    if (request === "react-native") return { Button: "NativeButton" };
    assert(request.startsWith("."), `Unexpected dependency: ${request}`);
    return load(resolveModule(file, request, platform), platform, cache);
  };
  vm.runInThisContext(
    `(function(require, module, exports) {\n${readFileSync(file, "utf8")}\n})`,
    {
      filename: file,
    },
  )(localRequire, module, module.exports);
  return module.exports;
}

for (const crossModuleOpt of [false, true]) {
  const tag = crossModuleOpt ? "optimized" : "default";
  const { work, output, shared } = build({ crossModuleOpt, tag });
  const contract = readFileSync(join(shared, "Button.cmi"));
  for (const platform of config.platforms) {
    const context = join(work, platform);
    assert.deepEqual(readFileSync(join(context, "Button.cmi")), contract);
    const entry = resolveModule(join(output, "index.js"), "./App", platform);
    assert.equal(entry, join(output, `App.${platform}.js`));
    const app = load(entry, platform);
    assert.equal(app.platform, platform);
    assert.equal(app.enabled, platform === "android");
    assert.equal(app.describe(), `${platform}-token`);
    const appElement = app.make({});
    assert(react.isValidElement(appElement));
    assert.equal(typeof appElement.type, "function");
    const element = appElement.type(appElement.props);
    assert(react.isValidElement(element));
    assert.equal(element.type, "NativeButton");
    assert.equal(
      element.props.title,
      `Platform button on ${platform === "ios" ? "iOS" : "Android"}`,
    );
    assert.equal(
      element.props.color,
      platform === "ios" ? "#007aff" : "#3ddc84",
    );
    assert.equal(element.props.onPress(), undefined);
    assert.match(
      readFileSync(join(context, "App.lambda.txt"), "utf8"),
      /global Button/,
    );
    assert.match(readFileSync(entry, "utf8"), /let enabled = (true|false);/);
    assert.throws(() => resolveModule(entry, "./Button.js", platform));

    // Every implementation must satisfy the shared contract, even though its
    // original dotted filename differs from the interface's filename.
    const badSource = join(context, `Button.${platform}.res`);
    writeFileSync(
      badSource,
      readFileSync(join(root, "src", `Button.${platform}.res`), "utf8").replace(
        /let enabled = (true|false)/,
        "let enabled = 123",
      ),
    );
    const bad = compile(
      ["-bs-read-cmi", "-o", join(context, "Button.cmj"), badSource],
      {
        includes: [context, shared],
        output: join(context, "js"),
        crossModuleOpt,
      },
    );
    assert.equal(bad.status, 2, bad.stderr);
    assert.match(bad.stderr, /enabled/);
    assert.match(bad.stderr, /int/);
    assert.match(bad.stderr, /bool/);
    assert.deepEqual(readFileSync(join(context, "Button.cmi")), contract);
  }

  if (!crossModuleOpt) {
    // A shared App compiled using Android's .cmj is unsound for iOS, even with
    // -bs-no-cross-module-opt. Metro selects iOS's Button, but the bool is baked in.
    const mixed = load(join(output, "App.android.js"), "ios");
    assert.equal(mixed.platform, "ios");
    assert.equal(mixed.enabled, true);
    console.log(
      "Confirmed: reusing one platform's App.js leaks boolean constants",
    );

    const suffixDir = join(work, "suffix-only");
    mkdirSync(suffixDir);
    copyFileSync(join(shared, "Button.cmi"), join(suffixDir, "Button.cmi"));
    expectSuccess(
      compile(
        [
          "-bs-package-output",
          `commonjs:${relative(root, suffixDir)}:.ios.js`,
          "-bs-read-cmi",
          "-o",
          join(suffixDir, "Button.cmj"),
          join(root, "src/Button.ios.res"),
        ],
        { includes: [suffixDir, shared] },
      ),
    );
    expectSuccess(
      compile(
        [
          "-bs-package-output",
          `commonjs:${relative(root, suffixDir)}:.js`,
          "-o",
          join(suffixDir, "App.cmj"),
          join(root, "src/App.res"),
        ],
        { includes: [suffixDir, shared] },
      ),
    );
    assert.match(
      readFileSync(join(suffixDir, "App.js"), "utf8"),
      /require\("\.\/Button\.ios\.js"\)/,
    );
    console.log("Confirmed: suffix alone hard-codes the platform into imports");

  }
  console.log(
    `Passed both platforms with cross-module optimization ${crossModuleOpt ? "on" : "off"}`,
  );
}
