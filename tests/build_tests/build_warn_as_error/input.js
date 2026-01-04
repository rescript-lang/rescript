import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const o1 = await execBuild();

// biome-ignore lint/suspicious/noControlCharactersInRegex: strip ANSI color codes from output
const stripAnsi = s => s.replace(/\x1b\[[0-9;]*m/g, "");

const first_message = stripAnsi(o1.stdout)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110");

if (!first_message) {
  assert.fail(o1.stdout + o1.stderr);
}

// Second build using --warn-error +110
const o2 = await execBuild(["--warn-error", "+110"]);

const second_message = stripAnsi(o2.stderr)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (!second_message) {
  assert.fail(o2.stdout + o2.stderr);
}

// Third build, without --warn-error +110
// The result should not be a warning as error
const o3 = await execBuild();

const third_message = stripAnsi(o3.stderr)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (o3.status !== 0 || third_message) {
  assert.fail(o3.stdout + o3.stderr);
}

await execClean();
