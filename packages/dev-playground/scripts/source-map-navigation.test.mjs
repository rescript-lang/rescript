import assert from "node:assert/strict";
import test from "node:test";

import {
  decode,
  decodeForSource,
  generatedForOriginal,
  groupByGeneratedLine,
  isCollapsedSelection,
  isCurrentSource,
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

test("disables mappings when the result belongs to stale source", () => {
  assert.equal(isCurrentSource("let value = 1", "let value = 2"), false);
  assert.deepEqual(
    decodeForSource(sourceMap, "let value = 1", "let value = 2"),
    [],
  );
  assert.deepEqual(
    decodeForSource(sourceMap, "let value = 2", "let value = 2"),
    decode(sourceMap),
  );
});

test("navigates only when the source selection is collapsed", () => {
  assert.equal(isCollapsedSelection(4, 4), true);
  assert.equal(isCollapsedSelection(4, 12), false);
});

test("groups ordered mappings by generated line in one pass", () => {
  const first = { generated: { line: 1, col: 0 } };
  const second = { generated: { line: 1, col: 8 } };
  const third = { generated: { line: 3, col: 2 } };

  assert.deepEqual(groupByGeneratedLine([first, second, third], 3), [
    [first, second],
    [],
    [third],
  ]);
});

test("does not carry a generated position onto an unmapped source line", () => {
  const mapping = generatedForOriginal(decode(sourceMap), { line: 3, col: 0 });

  assert.equal(mapping, undefined);
});

test("handles invalid source maps", () => {
  assert.deepEqual(decode("not a source map"), []);
});
