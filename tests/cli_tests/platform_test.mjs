import * as assert from "node:assert/strict";
import { describe, it } from "mocha";
import { getTarget } from "../../cli/common/platform.cjs";

describe("getTarget", () => {
  it("maps supported platforms to their own package", () => {
    for (const [platform, arch] of [
      ["darwin", "arm64"],
      ["darwin", "x64"],
      ["linux", "arm64"],
      ["linux", "x64"],
      ["win32", "x64"],
    ]) {
      assert.equal(getTarget(platform, arch, "10.0.0"), `${platform}-${arch}`);
    }
  });

  it("uses the x64 package on Windows 11 on ARM", () => {
    assert.equal(getTarget("win32", "arm64", "10.0.22000"), "win32-x64");
    assert.equal(getTarget("win32", "arm64", "10.0.26100"), "win32-x64");
  });

  it("rejects Windows 10 on ARM, which cannot emulate x64", () => {
    assert.equal(getTarget("win32", "arm64", "10.0.19045"), undefined);
    assert.equal(getTarget("win32", "arm64", "unknown"), undefined);
  });

  it("rejects unsupported platforms", () => {
    assert.equal(getTarget("win32", "ia32", "10.0.26100"), undefined);
    assert.equal(getTarget("linux", "ia32", "6.0.0"), undefined);
    assert.equal(getTarget("freebsd", "x64", "14.0"), undefined);
  });
});
