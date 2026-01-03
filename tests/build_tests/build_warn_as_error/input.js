import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy, execCleanLegacy } = setup(import.meta.dirname);

const o1 = await execBuildLegacy();

const first_message = o1.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110");

if (!first_message) {
  assert.fail(o1.stdout);
}

// Second build using -warn-error +110
const o2 = await execBuildLegacy(["-warn-error", "+110"]);

const second_message = o2.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (!second_message) {
  assert.fail(o2.stdout);
}

// Third build, without -warn-error +110
// The result should not be a warning as error
const o3 = await execBuildLegacy();

const third_message = o3.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Dependency Finished");

if (!third_message) {
  assert.fail(o3.stdout);
}

await execCleanLegacy();
