import assert from "node:assert/strict";
import test from "node:test";
import { createReleasePlan } from "./releasePlan.js";

test("plans a maintenance release", () => {
  assert.deepEqual(createReleasePlan("12.3.0"), {
    version: "12.3.0",
    gitTag: "v12.3.0",
    currentStable: false,
    npmTag: "latest-12",
    publishTag: "latest-12",
  });
});

test("plans a current stable release", () => {
  const plan = createReleasePlan("13.0.0", true);
  assert.equal(plan.npmTag, "latest");
  assert.equal(plan.publishTag, null);
});

test("derives the prerelease tag", () => {
  assert.equal(createReleasePlan("13.0.0-alpha.6").npmTag, "next-13");
});

test("derives the prerelease tag for an older major", () => {
  assert.equal(createReleasePlan("12.4.0-beta.1").npmTag, "next-12");
});

test("rejects promoting a prerelease to current stable", () => {
  assert.throws(
    () => createReleasePlan("13.0.0-alpha.6", true),
    /prerelease cannot be the current stable/,
  );
});

test("rejects non-canonical and invalid versions", () => {
  assert.throws(() => createReleasePlan("v13.0.0"));
  assert.throws(() => createReleasePlan("13.0"));
  assert.throws(() => createReleasePlan("13.0.0+build.1"));
});
