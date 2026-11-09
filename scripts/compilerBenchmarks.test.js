import assert from "node:assert/strict";
import test from "node:test";
import {
  compareAll,
  compareResults,
  generatedProjects,
  parseAllocatedWords,
  parseInstructions,
  renderSummary,
  stressInputs,
} from "./compilerBenchmarks.js";

const stats = `allocated_words: 628461
minor_words: 593681
promoted_words: 113760
major_words: 148540
minor_collections: 3
top_heap_words: 208279
`;

const cachegrind = `==4242== Cachegrind, a high-precision tracing profiler
==4242== Command: bsc.exe -format Napkinscript.res
==4242==
==4242== I refs:        1,234,567,890
`;

test("reads the allocated words from the runtime's statistics", () => {
  assert.equal(parseAllocatedWords(`Warning 26\n${stats}`), 628461);
  assert.equal(parseAllocatedWords("minor_words: 1\n"), undefined);
});

test("adds the words the build system's workers allocated", () => {
  assert.equal(
    parseAllocatedWords(`worker_allocated_words: 1000\n${stats}`),
    629461,
  );
});

test("reads the instructions from cachegrind's report", () => {
  assert.equal(parseInstructions(`${stats}${cachegrind}`), 1234567890);
  assert.equal(parseInstructions(stats), undefined);
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
  const summary = renderSummary([comparison], 1);
  assert.match(
    summary,
    /\*\*🎉 Improvement: 1 benchmark allocates more than 1% less\.\*\*/,
  );
  assert.match(summary, /^## 🎉 Compiler benchmarks$/m);
  assert.match(summary, /^### Allocated words$/m);
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
  const summary = renderSummary([comparison], 1);
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
  const summary = renderSummary([comparison], 1);
  assert.match(
    summary,
    /\*\*⚠️ Regression: 1 benchmark allocates more than 1% more\.\*\*/,
  );
  assert.match(summary, /^## ⚠️ Compiler benchmarks$/m);
  assert.match(summary, /\| `big` \| 100,000 \| 103,000 \| \+3\.00% \|/);
  assert.match(summary, /\| `zero` \(inputs changed\) \| 0 \| 10 \| new \|/);
  assert.match(summary, /^\(inputs changed\): this pull request changes/m);
  assert.doesNotMatch(summary, /`path`/);
  assert.doesNotMatch(summary, /less than/);
});

test("counts several benchmarks in the plural", () => {
  const summary = renderSummary(
    [
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
    ],
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
    [
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
    ],
    1,
  );
  assert.match(
    summary,
    /^None of the 2 benchmarks changed by 0\.1% or more\.$/m,
  );
  assert.match(summary, /^## ✅ Compiler benchmarks$/m);
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
    renderSummary([compareResults(base, head, 1)], 1).split("\n")[0];
  // A regression outweighs an improvement.
  assert.equal(
    title({
      a: { allocated: 1100, inputs: "x" },
      b: { allocated: 800, inputs: "x" },
    }),
    "## ⚠️ Compiler benchmarks",
  );
  // Changed inputs alone are neither.
  assert.equal(
    title({
      a: { allocated: 2000, inputs: "y" },
      b: { allocated: 1000, inputs: "x" },
    }),
    "## ✅ Compiler benchmarks",
  );
});

test("compares instructions where both measurements have them", () => {
  const base = {
    a: { allocated: 1000, instructions: 50000, inputs: "x" },
    b: { allocated: 1000, inputs: "x" },
    c: { allocated: 1000, instructions: 70000, inputs: "x" },
  };
  const head = {
    a: { allocated: 1000, instructions: 60000, inputs: "x" },
    b: { allocated: 1000, instructions: 9, inputs: "x" },
    c: { allocated: 1000, instructions: 70000, inputs: "x" },
  };
  const [allocated, instructions] = compareAll(base, head, 1);
  assert.equal(allocated.compared, 3);
  assert.deepEqual(allocated.changes, []);
  // b has no instructions on the base side.
  assert.equal(instructions.compared, 2);
  assert.deepEqual(instructions.added, ["b"]);
  assert.deepEqual(
    instructions.regressions.map(c => c.name),
    ["a"],
  );
  assert.deepEqual(instructions.total, { base: 120000, head: 130000 });

  const summary = renderSummary([allocated, instructions], 1);
  // A regression in either section shows in the title.
  assert.match(summary, /^## ⚠️ Compiler benchmarks$/m);
  assert.match(summary, /^### Allocated words$/m);
  assert.match(summary, /^### Instructions$/m);
  // One symbol for the title and one for the instructions' headline.
  assert.equal(summary.match(/✅|⚠️|🎉/gu)?.length, 2);
  assert.match(
    summary,
    /\*\*⚠️ Regression: 1 benchmark executes more than 1% more instructions\.\*\*/,
  );
  assert.match(summary, /\| `a` \| 50,000 \| 60,000 \| \+20\.00% \|/);
});

test("leaves out the instructions section when nothing measured them", () => {
  const results = { a: { allocated: 1000, inputs: "x" } };
  const summary = renderSummary(compareAll(results, results, 1), 1);
  assert.match(summary, /^### Allocated words$/m);
  assert.doesNotMatch(summary, /Instructions/);
});

test("words an improvement in instructions", () => {
  const [, instructions] = compareAll(
    {
      a: { allocated: 1, instructions: 1000, inputs: "x" },
      b: { allocated: 1, instructions: 1000, inputs: "x" },
    },
    {
      a: { allocated: 1, instructions: 900, inputs: "x" },
      b: { allocated: 1, instructions: 800, inputs: "x" },
    },
    1,
  );
  assert.match(
    renderSummary([instructions], 1),
    /\*\*🎉 Improvement: 2 benchmarks execute more than 1% fewer instructions\.\*\*/,
  );
});

test("compares output sizes in a section of their own", () => {
  const base = {
    a: { allocated: 1000, inputs: "x" },
    "output/runtime/*.cmt": { bytes: 2000, inputs: "x" },
    "output/runtime/*.cmi": { bytes: 1000, inputs: "x" },
  };
  const head = {
    a: { allocated: 1000, inputs: "x" },
    "output/runtime/*.cmt": { bytes: 2100, inputs: "x" },
    "output/runtime/*.cmi": { bytes: 1000, inputs: "x" },
  };
  const [allocated, , bytes] = compareAll(base, head, 1);
  assert.equal(allocated.compared, 1);
  assert.equal(bytes.compared, 2);
  const summary = renderSummary(compareAll(base, head, 1), 1);
  assert.match(summary, /^### Output size$/m);
  assert.doesNotMatch(summary, /Instructions/);
  assert.match(
    summary,
    /\*\*⚠️ Regression: 1 output is more than 1% larger\.\*\*/,
  );
  assert.match(
    summary,
    /\| `output\/runtime\/\*\.cmt` \| 2,000 \| 2,100 \| \+5\.00% \|/,
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
  const summary = renderSummary([compareResults(base, head, 1)], 1);
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

test("adds up the generated projects apart from the total", () => {
  const base = {
    a: { allocated: 1000, inputs: "x" },
    "build/clean": { allocated: 2000, inputs: "x" },
    "build/Namespace/clean": { allocated: 100000, inputs: "x" },
    "build/WideDeps/edit": { allocated: 50000, inputs: "x" },
  };
  const head = {
    a: { allocated: 1000, inputs: "x" },
    "build/clean": { allocated: 2000, inputs: "x" },
    "build/Namespace/clean": { allocated: 40000, inputs: "x" },
    "build/WideDeps/edit": { allocated: 50000, inputs: "x" },
  };
  const comparison = compareResults(base, head, 1);
  assert.deepEqual(comparison.total, { base: 3000, head: 3000 });
  assert.deepEqual(comparison.generated, { base: 150000, head: 90000 });
  // The generated projects still have rows and thresholds of their own.
  assert.deepEqual(
    comparison.improvements.map(c => c.name),
    ["build/Namespace/clean"],
  );
  const summary = renderSummary([comparison], 1);
  assert.match(summary, /^In total: 3,000 → 3,000 \(±0\.00%\)\.$/m);
  assert.match(
    summary,
    /^Generated projects: 150,000 → 90,000 \(-40\.00%\)\.$/m,
  );
  // Without generated projects, there is no subtotal.
  assert.doesNotMatch(
    renderSummary([compareResults({ a: base.a }, { a: head.a }, 1)], 1),
    /Generated projects/,
  );
});

test("generates the same projects every time", () => {
  for (const generate of Object.values(generatedProjects)) {
    const project = generate();
    assert.deepEqual(generate(), project);
    // The edit changes one of the project's sources.
    assert.ok(project.edit.endsWith(".res"));
    assert.ok(project.edit in project.files);
    const config = JSON.parse(project.files["rescript.json"]);
    // JavaScript goes to lib/, which a clean build removes.
    assert.equal(config["package-specs"]["in-source"], false);
  }
});

test("namespaces only the namespace benchmark's project", () => {
  const { files } = generatedProjects.Namespace();
  assert.equal(JSON.parse(files["rescript.json"]).namespace, true);
  // Modules use others through the namespace's map.
  assert.match(files["src/Mod10.res"], /^let value = Mod4\.value \+ 10$/m);
  const wide = generatedProjects.WideDeps();
  assert.equal(JSON.parse(wide.files["rescript.json"]).namespace, undefined);
});
