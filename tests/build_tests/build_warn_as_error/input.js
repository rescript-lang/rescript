import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const o1 = await execBuild();

const first_message = stripVTControlCharacters(o1.stderr)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110");

if (!first_message) {
  assert.fail(o1.stdout + o1.stderr);
}

// Second build using --warn-error +110
const o2 = await execBuild(["--warn-error", "+110"]);

const second_message = stripVTControlCharacters(o2.stderr)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (!second_message) {
  assert.fail(o2.stdout + o2.stderr);
}

// Third build, without --warn-error +110
// The result should not be a warning as error
const o3 = await execBuild();

const third_message = stripVTControlCharacters(o3.stderr)
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (o3.status !== 0 || third_message) {
  assert.fail(o3.stdout + o3.stderr);
}

await execClean();
