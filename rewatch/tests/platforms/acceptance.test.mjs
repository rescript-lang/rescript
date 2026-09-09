import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import {
  copyFileSync,
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import vm from "node:vm";

const fixture = fileURLToPath(
  new URL("../fixtures/react-native-platforms/", import.meta.url),
);
const repository = resolve(fixture, "../../../..");
const executable = process.env.REWATCH_EXECUTABLE;
assert(
  executable,
  "Set REWATCH_EXECUTABLE; acceptance tests must invoke Rewatch",
);
const platformBin = join(
  repository,
  "packages/@rescript",
  `${process.platform}-${process.arch}`,
  "bin",
);
const env = {
  ...process.env,
  RESCRIPT_BSC_EXE:
    process.env.RESCRIPT_BSC_EXE ?? join(platformBin, "bsc.exe"),
  RESCRIPT_RUNTIME:
    process.env.RESCRIPT_RUNTIME ??
    join(repository, "packages/@rescript/runtime"),
};

function project(t) {
  const root = mkdtempSync(join(tmpdir(), "rescript-platform-test-"));
  t.after(() => rmSync(root, { recursive: true, force: true }));
  cpSync(join(fixture, "src"), join(root, "src"), { recursive: true });
  copyFileSync(join(fixture, "rescript.json"), join(root, "rescript.json"));
  return root;
}

function run(root, command = "build", succeeds = true, verbose = false) {
  const result = spawnSync(executable, verbose ? ["-vv", command] : [command], {
    cwd: root,
    env,
    encoding: "utf8",
    timeout: 30_000,
  });
  const output = (result.stdout ?? "") + (result.stderr ?? "");
  if (result.error) {
    throw new Error(`${result.error.message}\n${output}`, {
      cause: result.error,
    });
  }
  if (succeeds) assert.equal(result.status, 0, output);
  else
    assert.notEqual(result.status, 0, "Expected Rewatch to reject the project");
  return output;
}

function compilerArgs(root, file) {
  const result = spawnSync(executable, ["compiler-args", join("src", file)], {
    cwd: root,
    env,
    encoding: "utf8",
  });
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return JSON.parse(result.stdout).compiler_args.map(normalizePaths);
}

function hasExited(child) {
  return child.exitCode !== null || child.signalCode !== null;
}

async function waitFor(predicate, message, timeout = 30_000) {
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    if (predicate()) return;
    await new Promise(resolve => setTimeout(resolve, 50));
  }
  throw new Error(message);
}

async function stopWatcher(root, child) {
  if (hasExited(child)) return;
  rmSync(join(root, "lib/watch.lock"), { force: true });
  try {
    await waitFor(() => hasExited(child), "Watcher did not stop", 2_000);
  } catch {
    child.kill();
    await waitFor(() => hasExited(child), "Watcher could not be killed", 5_000);
  }
}

function updateConfig(root, update) {
  const path = join(root, "rescript.json");
  const config = JSON.parse(readFileSync(path, "utf8"));
  update(config);
  writeFileSync(path, JSON.stringify(config));
}

function normalizePaths(value) {
  return value.replaceAll("\\", "/");
}

function js(root, name, platform) {
  const directory = join(root, "lib/js/src");
  return name === "Button"
    ? join(directory, `${name}.${platform}.js`)
    : join(directory, `${name}.js`);
}

// This checks generated module behavior without installing React Native in the
// ordinary integration suite. Actual Metro selection is checked by npm run bundle
// in the fixture; this loader is deliberately not a replacement Metro resolver.
function load(file, platform, cache = new Map()) {
  if (cache.has(file)) return cache.get(file).exports;
  const module = { exports: {} };
  cache.set(file, module);
  const require = request => {
    if (request === "react/jsx-runtime")
      return {
        jsx: (type, props) => ({ type, props }),
        jsxs: (type, props) => ({ type, props }),
      };
    if (request === "react-native") return { Button: "NativeButton" };
    assert(
      request.startsWith("."),
      `Unexpected generated dependency: ${request}`,
    );
    assert(
      !/\.(js|mjs|cjs)$/.test(request),
      `Platform imports must be extensionless: ${request}`,
    );
    const base = resolve(dirname(file), request);
    const target = existsSync(`${base}.${platform}.js`)
      ? `${base}.${platform}.js`
      : `${base}.js`;
    return load(target, platform, cache);
  };
  vm.runInThisContext(
    `(function(require, module, exports) {\n${readFileSync(file, "utf8")}\n})`,
    { filename: file },
  )(require, module, module.exports);
  return module.exports;
}

function check(root, platform, enabled = platform === "android") {
  assert(
    existsSync(js(root, "Button", platform)),
    `Missing Button.${platform}.js`,
  );
  const app = load(js(root, "App", platform), platform);
  assert.equal(app.platform, platform);
  assert.equal(
    app.enabled,
    enabled,
    "A consumer must not use another platform's constant",
  );
  assert.equal(
    app.describe(),
    `${platform}-token`,
    "Abstract representations must stay within their platform",
  );
  assert.equal(app.details, `${platform}-details`);
  assert.equal(
    app.identify(),
    `${platform}-details`,
    "Nested and hoisted exports must use the selected implementation",
  );
  const appElement = app.make({});
  assert.equal(typeof appElement.type, "function");
  const element = appElement.type(appElement.props);
  assert.equal(element.type, "NativeButton");
  assert.equal(
    element.props.title,
    `Platform button on ${platform === "ios" ? "iOS" : "Android"}`,
  );
}

function edit(root, file, from, to) {
  const path = join(root, "src", file);
  const source = readFileSync(path, "utf8");
  assert(source.includes(from), `Missing fixture text: ${from}`);
  writeFileSync(path, source.replace(from, to));
}

for (const optimize of [false, true]) {
  test(`shared interface and platform behavior (cross-module optimization ${optimize ? "on" : "off"})`, t => {
    const root = project(t);
    updateConfig(root, config => {
      config["compiler-flags"] = [
        optimize ? "-bs-cross-module-opt" : "-bs-no-cross-module-opt",
      ];
    });
    run(root);
    assert(existsSync(js(root, "App", "android")), "Missing shared App.js");
    assert(!existsSync(join(root, "lib/js/src/App.android.js")));
    assert(!existsSync(join(root, "lib/js/src/App.ios.js")));
    const sharedApp = readFileSync(js(root, "App", "android"), "utf8");
    assert.match(sharedApp, /require\("\.\/Button"\)/);
    assert.match(sharedApp, /Button\.enabled/);
    assert.match(sharedApp, /Button\.Details\.identify/);
    assert.doesNotMatch(sharedApp, /Button\.(android|ios)/);
    assert.doesNotMatch(sharedApp, /Button\.Details\$identify/);
    check(root, "android");
    check(root, "ios");
  });
}

test("no-op build preserves outputs; changing Android preserves iOS behavior", t => {
  const root = project(t);
  run(root);
  const files = ["android", "ios"].flatMap(platform => [
    js(root, "Button", platform),
    js(root, "App", platform),
  ]);
  const before = files.map(file => statSync(file, { bigint: true }).mtimeNs);
  run(root);
  assert.deepEqual(
    files.map(file => statSync(file, { bigint: true }).mtimeNs),
    before,
  );
  edit(root, "Button.android.res", "let enabled = true", "let enabled = false");
  run(root);
  check(root, "android", false);
  check(root, "ios");
});

for (const platform of ["android", "ios"]) {
  test(`${platform} implementation must satisfy Button.resi, including after an edit`, t => {
    const root = project(t);
    run(root);
    const original = `let enabled = ${platform === "android"}`;
    edit(root, `Button.${platform}.res`, original, "let enabled = 123");
    const output = run(root, "build", false);
    assert.match(output, /enabled/);
    assert.match(output, /bool/);
    assert.match(output, /int/);
    edit(root, `Button.${platform}.res`, "let enabled = 123", original);
    run(root);
    check(root, "android");
    check(root, "ios");
  });
}

test("editing the shared interface invalidates the implementation checks", t => {
  const root = project(t);
  run(root);
  edit(root, "Button.resi", "let enabled: bool", "let enabled: int");
  const output = run(root, "build", false);
  assert.match(output, /enabled/);
  assert.match(output, /bool/);
  assert.match(output, /int/);
});

test("clean removes both platform outputs and a subsequent build recreates them", t => {
  const root = project(t);
  run(root);
  const files = ["android", "ios"].flatMap(platform => [
    js(root, "Button", platform),
    js(root, "App", platform),
  ]);
  run(root, "clean");
  for (const file of files) assert(!existsSync(file), `Clean left ${file}`);
  run(root);
  check(root, "android");
  check(root, "ios");
});

test("clean succeeds after a configured platform implementation is deleted", t => {
  const root = project(t);
  updateConfig(root, config => {
    config["package-specs"] = { module: "commonjs", "in-source": true };
  });
  run(root);

  rmSync(join(root, "src/Button.ios.res"));
  run(root, "clean");

  assert(!existsSync(join(root, "src/App.js")));
  assert(!existsSync(join(root, "src/Button.android.js")));
  assert(!existsSync(join(root, "src/Button.ios.js")));
});

test("clean succeeds after a shared platform interface is deleted", t => {
  const root = project(t);
  updateConfig(root, config => {
    config["package-specs"] = { module: "commonjs", "in-source": true };
  });
  run(root);

  rmSync(join(root, "src/Button.resi"));
  run(root, "clean");

  assert(!existsSync(join(root, "src/App.js")));
  assert(!existsSync(join(root, "src/Button.android.js")));
  assert(!existsSync(join(root, "src/Button.ios.js")));
});

test("platform families require a shared interface", t => {
  const root = project(t);
  rmSync(join(root, "src/Button.resi"));
  const output = normalizePaths(run(root, "build", false));
  assert.match(output, /requires a shared interface 'src\/Button\.resi'/);
});

test("platform families require every configured implementation", t => {
  const root = project(t);
  rmSync(join(root, "src/Button.ios.res"));
  const output = normalizePaths(run(root, "build", false));
  assert.match(output, /missing implementation 'src\/Button\.ios\.res'/);
});

test("generic implementations are rejected until fallback semantics are defined", t => {
  const root = project(t);
  copyFileSync(join(root, "src/Button.android.res"), join(root, "src/Button.res"));
  const output = normalizePaths(run(root, "build", false));
  assert.match(output, /Generic fallbacks are not supported yet/);
});

test("platform-specific interfaces point to the shared interface contract", t => {
  const root = project(t);
  copyFileSync(join(root, "src/Button.resi"), join(root, "src/Button.android.resi"));
  const output = normalizePaths(run(root, "build", false));
  assert.match(output, /Platform-specific interface 'src\/Button\.android\.resi' is not supported/);
  assert.match(output, /Use the shared interface 'src\/Button\.resi'/);
});

test("ES modules keep platform filenames, extensionless imports, and source maps", t => {
  const root = project(t);
  updateConfig(root, config => {
    config["package-specs"] = { module: "esmodule", "in-source": false };
    config.suffix = ".mjs";
    config.sourceMap = { enabled: "always", mode: "linked" };
  });
  run(root);

  const directory = join(root, "lib/es6/src");
  for (const platform of ["android", "ios"]) {
    const output = join(directory, `Button.${platform}.mjs`);
    assert(existsSync(output), `Missing ${output}`);
    const sourceMap = JSON.parse(readFileSync(`${output}.map`, "utf8"));
    assert(
      sourceMap.sources.some(source => source.endsWith(`Button.${platform}.res`)),
      `Incorrect sources for ${output}: ${sourceMap.sources}`,
    );
  }
  const app = readFileSync(join(directory, "App.mjs"), "utf8");
  assert.match(app, /from "\.\/Button"/);
  assert.doesNotMatch(app, /Button\.(android|ios)\.mjs/);
});

test("in-source CommonJS output uses platform filenames", t => {
  const root = project(t);
  updateConfig(root, config => {
    config["package-specs"] = { module: "commonjs", "in-source": true };
  });
  run(root);

  assert(existsSync(join(root, "src/App.js")));
  assert(existsSync(join(root, "src/Button.android.js")));
  assert(existsSync(join(root, "src/Button.ios.js")));
  assert.match(readFileSync(join(root, "src/App.js"), "utf8"), /require\("\.\/Button"\)/);
  run(root, "clean");
  assert(!existsSync(join(root, "src/App.js")));
  assert(!existsSync(join(root, "src/Button.android.js")));
  assert(!existsSync(join(root, "src/Button.ios.js")));
});

test("GenType wrappers import the logical platform module without an extension", t => {
  const root = project(t);
  updateConfig(root, config => {
    config.gentypeconfig = { generatedFileExtension: ".gen.tsx" };
  });
  edit(
    root,
    "Button.resi",
    "let platform: string",
    "@genType\nlet platform: string",
  );

  assert(compilerArgs(root, "Button.android.res").includes("-bs-gentype"));
  assert(!compilerArgs(root, "Button.ios.res").includes("-bs-gentype"));

  run(root, "build", true, true);

  const wrapper = join(root, "src/Button.gen.tsx");
  assert(existsSync(wrapper), `Missing ${wrapper}`);
  const output = readFileSync(wrapper, "utf8");
  assert.match(output, /generated from Button\.resi/);
  assert.match(output, /require\(['"]\.\.\/src\/Button['"]\)/);
  assert.doesNotMatch(output, /require\(['"][^'"]*Button\.js['"]\)/);
  for (const platform of ["android", "ios"]) {
    assert(!existsSync(join(root, `src/Button.${platform}.gen.tsx`)));
  }
});

test("namespaced platform families retain the logical import", t => {
  const root = project(t);
  updateConfig(root, config => {
    config.namespace = "PlatformTest";
  });
  run(root);
  check(root, "android");
  check(root, "ios");
  assert.match(readFileSync(js(root, "App", "android"), "utf8"), /require\("\.\/Button"\)/);
});

test("a platform family can be the namespace entry", t => {
  const root = project(t);
  rmSync(join(root, "src/App.res"));
  updateConfig(root, config => {
    config.namespace = "PlatformTest";
    config["namespace-entry"] = "Button";
  });
  run(root);
  assert(existsSync(js(root, "Button", "android")));
  assert(existsSync(js(root, "Button", "ios")));
});

test("dependencies publish both variants behind one package import", t => {
  const root = project(t);
  const dependency = join(root, "node_modules/platform-dep");
  mkdirSync(join(dependency, "src"), { recursive: true });
  for (const file of [
    "Button.resi",
    "Button.android.res",
    "Button.ios.res",
    "React.res",
    "ReactNative.res",
  ]) {
    copyFileSync(join(fixture, "src", file), join(dependency, "src", file));
  }
  const dependencyConfig = JSON.parse(
    readFileSync(join(fixture, "rescript.json"), "utf8"),
  );
  dependencyConfig.name = "platform-dep";
  writeFileSync(join(dependency, "rescript.json"), JSON.stringify(dependencyConfig));
  writeFileSync(join(dependency, "package.json"), '{"name":"platform-dep"}');

  writeFileSync(
    join(root, "rescript.json"),
    JSON.stringify({
      name: "platform-root",
      sources: ["src"],
      dependencies: ["platform-dep"],
      jsx: { version: 4 },
      "package-specs": { module: "commonjs", "in-source": false },
      suffix: ".js",
    }),
  );
  writeFileSync(join(root, "package.json"), '{"name":"platform-root"}');
  writeFileSync(
    join(root, "src/App.res"),
    "let platform = Button.platform\nlet details = Button.Details.label\n",
  );
  for (const file of [
    "Button.resi",
    "Button.android.res",
    "Button.ios.res",
    "React.res",
    "ReactNative.res",
  ]) {
    rmSync(join(root, "src", file));
  }

  run(root);
  const app = readFileSync(join(root, "lib/js/src/App.js"), "utf8");
  assert.match(app, /require\("platform-dep\/lib\/js\/src\/Button"\)/);
  assert.doesNotMatch(app, /Button\.(android|ios)\.js/);
  assert(existsSync(join(dependency, "lib/js/src/Button.android.js")));
  assert(existsSync(join(dependency, "lib/js/src/Button.ios.js")));
});

test("removing a configured target removes its recorded output", t => {
  const root = project(t);
  run(root);
  assert(existsSync(js(root, "Button", "ios")));

  rmSync(join(root, "src/Button.ios.res"));
  updateConfig(root, config => {
    config.platforms = ["android"];
  });
  run(root);

  assert(existsSync(js(root, "Button", "android")));
  assert(!existsSync(js(root, "Button", "ios")));
  check(root, "android");
});

test("adding a configured target creates its implementation output", t => {
  const root = project(t);
  rmSync(join(root, "src/Button.ios.res"));
  updateConfig(root, config => {
    config.platforms = ["android"];
  });
  run(root);
  assert(!existsSync(js(root, "Button", "ios")));

  copyFileSync(
    join(fixture, "src/Button.ios.res"),
    join(root, "src/Button.ios.res"),
  );
  updateConfig(root, config => {
    config.platforms.push("ios");
  });
  run(root);

  check(root, "android");
  check(root, "ios");
});

test("watch reports a removed platform implementation and recovers when it returns", async t => {
  const root = project(t);
  const child = spawn(executable, ["-vv", "watch"], {
    cwd: root,
    env,
    stdio: ["ignore", "pipe", "pipe"],
  });
  let output = "";
  child.stdout.setEncoding("utf8");
  child.stderr.setEncoding("utf8");
  child.stdout.on("data", chunk => {
    output += chunk;
  });
  child.stderr.on("data", chunk => {
    output += chunk;
  });

  try {
    await waitFor(
      () =>
        (existsSync(js(root, "Button", "ios")) &&
          output.includes("Finished initial compilation")) ||
        hasExited(child),
      `Initial watch build did not finish:\n${output}`,
    );
    assert.equal(hasExited(child), false, output);

    rmSync(join(root, "src/Button.ios.res"));
    await waitFor(
      () =>
        normalizePaths(output).includes(
          "missing implementation 'src/Button.ios.res'",
        ) || hasExited(child),
      `Watcher did not report the missing platform implementation:\n${output}`,
    );
    assert.equal(hasExited(child), false, output);

    rmSync(js(root, "Button", "ios"));
    writeFileSync(
      join(root, "src/Button.ios.res"),
      `${readFileSync(join(fixture, "src/Button.ios.res"), "utf8")}\n`,
    );
    await waitFor(
      () => existsSync(js(root, "Button", "ios")) || hasExited(child),
      `Watcher did not rebuild the restored platform implementation:\n${output}`,
    );
    assert.equal(hasExited(child), false, output);
    check(root, "ios");
  } finally {
    await stopWatcher(root, child);
  }
});

test("a failed build records newly emitted platform outputs for later removal", t => {
  const root = project(t);
  rmSync(join(root, "src/Button.ios.res"));
  updateConfig(root, config => {
    config.platforms = ["android"];
  });
  run(root);

  copyFileSync(
    join(fixture, "src/Button.ios.res"),
    join(root, "src/Button.ios.res"),
  );
  updateConfig(root, config => {
    config.platforms.push("ios");
    config["js-post-build"] = { cmd: "node fail-ios.cjs" };
  });
  const iosOutput = js(root, "Button", "ios");
  writeFileSync(
    join(root, "fail-ios.cjs"),
    'process.exit(process.argv[2].endsWith("Button.ios.js") ? 1 : 0);\n',
  );
  run(root, "build", false);
  assert(existsSync(iosOutput));

  rmSync(join(root, "fail-ios.cjs"));
  rmSync(join(root, "src/Button.ios.res"));
  updateConfig(root, config => {
    config.platforms = ["android"];
    delete config["js-post-build"];
  });
  run(root);

  assert(!existsSync(iosOutput));
  check(root, "android");
});

test("compiler-args reports the logical platform compilation", t => {
  const root = project(t);
  const args = compilerArgs(root, "Button.ios.res");
  assert(args.includes("-bs-platform-interface"));
  assert(
    args.includes("__platform/ios/Button.cmj"),
    args.join(" "),
  );
  assert(
    args.some(argument => argument.includes("commonjs:lib/js/src:.ios.js")),
    args.join(" "),
  );
  assert(args.includes("-bs-read-cmi"));
});
