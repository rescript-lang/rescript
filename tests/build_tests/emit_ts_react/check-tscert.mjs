import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { existsSync, realpathSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const [checkout] = process.argv.slice(2);
const installed =
  process.env.TSCERT_BIN ??
  (checkout || process.env.TSCERT_DIR
    ? undefined
    : ["node_modules/.bin/tscert", "../../../node_modules/.bin/tscert"]
        .map(path => join(import.meta.dirname, path))
        .find(path => existsSync(path)));
const driver = "pipeline/verify-source.mts";

function run(command, args, directory) {
  const result = spawnSync(command, args, { cwd: directory, stdio: "inherit" });
  if (result.error) throw result.error;
  if (result.status !== 0) process.exit(result.status ?? 1);
}

await import("./input.js");
let checker;
if (installed) {
  checker = {
    command: resolve(installed),
    prefix: [],
    cwd: import.meta.dirname,
  };
} else {
  const sibling = fileURLToPath(
    new URL("../../../../TScert/", import.meta.url),
  );
  const tscert = resolve(checkout ?? process.env.TSCERT_DIR ?? sibling);
  if (!existsSync(join(tscert, driver))) {
    console.error(
      `TScert checkout not found at ${tscert}. Set TSCERT_BIN to an installed CLI or TSCERT_DIR to a single-pass checkout.`,
    );
    process.exit(1);
  }
  const cwd = realpathSync(tscert);
  if (!existsSync(join(cwd, "tools/node_modules/typescript/package.json"))) {
    run("make", ["setup"], cwd);
  }
  run("lake", ["build", "verifier", "--log-level=warning"], join(cwd, "lean"));
  run("make", ["build-runner"], cwd);
  checker = { command: process.execPath, prefix: [driver], cwd };
}

const cases = {
  "contracts/verification-tsconfig.json": {
    "src/app/TodoLabel.res.ts": "verified",
    "src/app/TodoModel.res.ts": "verified",
    "src/app/TodoReact.res.ts": "verified",
    "src/app/TodoRow.res.ts": "verified",
    "src/forwarding/Forwarding.res.ts": "verified",
    "src/forwarding/ForwardingMutation.res.ts": "unverified",
  },
  "controls/tsconfig.json": {
    "controls/ConditionalHook.ts": "verified",
    "controls/ForgedElement.ts": "unverified",
    "controls/ImpureReducer.ts": "unverified",
    "controls/InvalidProps.ts": "unverified",
  },
};

for (const [config, files] of Object.entries(cases)) {
  const project = join(import.meta.dirname, config);
  const entries = Object.entries(files);
  const sources = entries.map(([file]) => join(import.meta.dirname, file));
  const result = spawnSync(
    checker.command,
    [...checker.prefix, "--batch", ...sources, "--project", project],
    { cwd: checker.cwd, encoding: "utf8" },
  );
  if (result.error) throw result.error;
  const exitCode = entries.some(([, expected]) => expected !== "verified")
    ? 1
    : 0;
  assert.equal(result.status, exitCode, result.stderr || result.stdout);
  const lines = result.stdout.trimEnd().split("\n");
  assert.equal(lines.length, entries.length, result.stderr || result.stdout);
  for (const [index, [file, expected]] of entries.entries()) {
    const verdict = JSON.parse(lines[index]);
    assert.equal(verdict.source, sources[index]);
    assert.equal(verdict.status, expected, verdict.detail);
    assert.ok(verdict.units?.length);
    if (file === "src/app/TodoModel.res.ts") {
      assert.ok(
        verdict.units.some(
          unit => /^reduce@\d+$/.test(unit.unit) && unit.status === "verified",
        ),
        "TScert must verify the emitted reducer",
      );
    }
    if (file === "controls/ImpureReducer.ts") {
      assert.match(
        verdict.detail,
        /footprint claimed for 'mutate' is not established by its body/,
      );
    }
    if (file === "src/forwarding/ForwardingMutation.res.ts") {
      assert.match(
        verdict.detail,
        /footprint claimed for 'incrementInPlace' is not established by its body/,
      );
    }
    if (file === "src/forwarding/Forwarding.res.ts") {
      assert.ok(
        verdict.units.some(
          unit =>
            /^increment@\d+$/.test(unit.unit) &&
            unit.depends.some(dependency =>
              /Forwarded\.res\.ts#increment@\d+$/.test(dependency),
            ),
        ),
        "The forwarding claim must use the effect verified from the helper",
      );
    }
    const verified = verdict.units.filter(
      unit => unit.status === "verified",
    ).length;
    console.log(`${file} ${verified}/${verdict.units.length} ${expected}`);
  }
}

const biome = join(import.meta.dirname, "../../../node_modules/.bin/biome");
const lint = [
  "lint",
  "--config-path=hooks.json",
  "--only=correctness/useHookAtTopLevel",
];
const app = spawnSync(biome, [...lint, "src/trusted"], {
  cwd: import.meta.dirname,
  encoding: "utf8",
});
assert.equal(app.status, 0, app.stderr || app.stdout);
const control = spawnSync(biome, [...lint, "controls/ConditionalHook.ts"], {
  cwd: import.meta.dirname,
  encoding: "utf8",
});
assert.equal(control.status, 1);
assert.match(control.stderr, /hook is being called conditionally/);
console.log("Biome rejected the conditional-hook control.");
