import assert from "node:assert/strict";
import test from "node:test";

import {
  decode,
  generatedForOriginal,
} from "../src/SourceMapNavigation.res.mjs";

const sourceMap = JSON.stringify({
  version: 3,
  file: "Playground.js",
  sources: ["Playground.res"],
  names: [],
  mappings: "AAAA;AACA",
});

test("decodes source map positions", () => {
  assert.deepEqual(decode(sourceMap), [
    {
      generated: { line: 1, col: 0 },
      original: {
        source: "Playground.res",
        position: { line: 1, col: 0 },
      },
    },
    {
      generated: { line: 2, col: 0 },
      original: {
        source: "Playground.res",
        position: { line: 2, col: 0 },
      },
    },
  ]);
});

test("finds the closest generated position for a source position", () => {
  const mapping = generatedForOriginal(decode(sourceMap), { line: 2, col: 5 });

  assert.deepEqual(mapping?.generated, { line: 2, col: 0 });
});

test("does not carry a generated position onto an unmapped source line", () => {
  const mapping = generatedForOriginal(decode(sourceMap), { line: 3, col: 0 });

  assert.equal(mapping, undefined);
});

test("handles invalid source maps", () => {
  assert.deepEqual(decode("not a source map"), []);
});
