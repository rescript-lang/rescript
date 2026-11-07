import assert from "node:assert/strict";
import test from "node:test";
import {
  compareResults,
  parseAllocatedWords,
  renderSummary,
  stressInputs,
} from "./allocBenchmarks.js";

const stats = `allocated_words: 628461
minor_words: 593681
promoted_words: 113760
major_words: 148540
minor_collections: 3
top_heap_words: 208279
`;

test("reads the allocated words from the runtime's statistics", () => {
  assert.equal(parseAllocatedWords(`Warning 26\n${stats}`), 628461);
  assert.equal(parseAllocatedWords("minor_words: 1\n"), undefined);
});

test("alerts on a growth above the threshold only", () => {
  const base = {
    a: { allocated: 1000, inputs: "x" },
    b: { allocated: 1000, inputs: "x" },
    c: { allocated: 1000, inputs: "x" },
  };
  const head = {
    a: { allocated: 1011, inputs: "x" },
    b: { allocated: 1010, inputs: "x" },
    c: { allocated: 900, inputs: "x" },
  };
  const { changes, regressions, total } = compareResults(base, head, 1);
  assert.deepEqual(
    changes.map(c => c.name),
    ["c", "a", "b"],
  );
  assert.deepEqual(
    regressions.map(c => c.name),
    ["a"],
  );
  assert.deepEqual(total, { base: 3000, head: 2921 });
});

test("reports an improvement above the threshold", () => {
  const comparison = compareResults(
    {
      a: { allocated: 1000, inputs: "x" },
      b: { allocated: 1000, inputs: "x" },
      c: { allocated: 1000, inputs: "x" },
    },
    {
      a: { allocated: 800, inputs: "x" },
      b: { allocated: 990, inputs: "x" },
      c: { allocated: 500, inputs: "y" },
    },
    1,
  );
  assert.deepEqual(
    comparison.improvements.map(c => c.name),
    ["a"],
  );
  assert.deepEqual(comparison.regressions, []);
  const summary = renderSummary(comparison, 1);
  assert.match(
    summary,
    /\*\*🎉 Improvement: 1 benchmark allocates more than 1% less\.\*\*/,
  );
  assert.match(summary, /^## 🎉 Allocation benchmarks$/m);
  assert.doesNotMatch(summary, /Regression/);
  assert.match(summary, /\| `a` \| 1,000 \| 800 \| -20\.00% \|/);
});

test("does not alert when the inputs changed", () => {
  const { changes, regressions } = compareResults(
    { a: { allocated: 1000, inputs: "x" } },
    { a: { allocated: 2000, inputs: "y" } },
    1,
  );
  assert.equal(changes.length, 1);
  assert.equal(changes[0].inputsChanged, true);
  assert.deepEqual(regressions, []);
});

test("lists benchmarks that only one measurement has", () => {
  const comparison = compareResults(
    { old: { allocated: 5, inputs: "x" }, kept: { allocated: 7, inputs: "x" } },
    {
      kept: { allocated: 7, inputs: "x" },
      fresh: { allocated: 9, inputs: "x" },
    },
    1,
  );
  assert.deepEqual(comparison.added, ["fresh"]);
  assert.deepEqual(comparison.removed, ["old"]);
  // Only benchmarks both measurements have count towards the totals.
  assert.deepEqual(comparison.total, { base: 7, head: 7 });
  const summary = renderSummary(comparison, 1);
  assert.match(summary, /New: fresh\./);
  assert.match(summary, /Removed: old\./);
  assert.match(summary, /^The benchmark didn't change by 0\.1% or more\.$/m);
  assert.match(summary, /\(±0\.00%\)/);
});

test("leaves changes below 0.1% out of the table", () => {
  const comparison = compareResults(
    {
      big: { allocated: 100000, inputs: "x" },
      path: { allocated: 100000, inputs: "x" },
      zero: { allocated: 0, inputs: "x" },
    },
    {
      big: { allocated: 103000, inputs: "x" },
      path: { allocated: 100030, inputs: "x" },
      zero: { allocated: 10, inputs: "y" },
    },
    1,
  );
  const summary = renderSummary(comparison, 1);
  assert.match(
    summary,
    /\*\*⚠️ Regression: 1 benchmark allocates more than 1% more\.\*\*/,
  );
  assert.match(summary, /^## ⚠️ Allocation benchmarks$/m);
  assert.match(summary, /\| `big` \| 100,000 \| 103,000 \| \+3\.00% \|/);
  assert.match(summary, /\| `zero` \(inputs changed\) \| 0 \| 10 \| new \|/);
  assert.match(summary, /^\(inputs changed\): this pull request changes/m);
  assert.doesNotMatch(summary, /`path`/);
  assert.doesNotMatch(summary, /less than/);
});

test("counts several benchmarks in the plural", () => {
  const summary = renderSummary(
    compareResults(
      {
        a: { allocated: 1000, inputs: "x" },
        b: { allocated: 1000, inputs: "x" },
      },
      {
        a: { allocated: 1100, inputs: "x" },
        b: { allocated: 1200, inputs: "x" },
      },
      1,
    ),
    1,
  );
  assert.match(
    summary,
    /\*\*⚠️ Regression: 2 benchmarks allocate more than 1% more\.\*\*/,
  );
  // No row has changed inputs, so there is no note.
  assert.doesNotMatch(summary, /^\(inputs changed\)/m);
});

test("says how many benchmarks it compared when none changed", () => {
  const summary = renderSummary(
    compareResults(
      {
        a: { allocated: 100000, inputs: "x" },
        b: { allocated: 5000, inputs: "x" },
      },
      {
        // A path-length difference only.
        a: { allocated: 100030, inputs: "x" },
        b: { allocated: 5000, inputs: "x" },
      },
      1,
    ),
    1,
  );
  assert.match(
    summary,
    /^None of the 2 benchmarks changed by 0\.1% or more\.$/m,
  );
  assert.match(summary, /^## ✅ Allocation benchmarks$/m);
  // The title's symbol is the only one.
  assert.equal(summary.match(/✅|⚠️|🎉/gu)?.length, 1);
  assert.doesNotMatch(summary, /\|/);
  assert.doesNotMatch(summary, /less than/);
});

test("shows the overall status in the title", () => {
  const base = {
    a: { allocated: 1000, inputs: "x" },
    b: { allocated: 1000, inputs: "x" },
  };
  /** @param {Record<string, {allocated: number, inputs: string}>} head */
  const title = head =>
    renderSummary(compareResults(base, head, 1), 1).split("\n")[0];
  // A regression outweighs an improvement.
  assert.equal(
    title({
      a: { allocated: 1100, inputs: "x" },
      b: { allocated: 800, inputs: "x" },
    }),
    "## ⚠️ Allocation benchmarks",
  );
  // Changed inputs alone are neither.
  assert.equal(
    title({
      a: { allocated: 2000, inputs: "y" },
      b: { allocated: 1000, inputs: "x" },
    }),
    "## ✅ Allocation benchmarks",
  );
});

test("folds all but the 10 largest changes away", () => {
  /** @type {Record<string, {allocated: number, inputs: string}>} */
  const base = {};
  /** @type {Record<string, {allocated: number, inputs: string}>} */
  const head = {};
  for (let i = 0; i < 253; i++) {
    base[`b${i}`] = { allocated: 1000, inputs: "x" };
    head[`b${i}`] = { allocated: 1010 + i, inputs: "x" };
  }
  const summary = renderSummary(compareResults(base, head, 1), 1);
  assert.match(
    summary,
    /\*\*⚠️ Regression: 252 benchmarks allocate more than 1% more\.\*\*/,
  );
  const [visible, folded] = summary.split("<details>");
  // The largest changes first, and every benchmark in one of the tables.
  assert.equal(visible.match(/^\| `b/gm)?.length, 10);
  assert.match(visible, /^\| `b252` \|/m);
  assert.match(folded, /^<summary>243 more changes<\/summary>$/m);
  assert.equal(folded.match(/^\| `b/gm)?.length, 243);
  assert.match(folded, /^<\/details>$/m);
});

test("generates the same stress inputs every time", () => {
  for (const generate of Object.values(stressInputs)) {
    assert.equal(generate(), generate());
  }
});
