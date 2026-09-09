import assert from "node:assert/strict";
import test from "node:test";
import {
  compare,
  maxCharacters,
  maxLines,
  sections,
} from "../src/LambdaDiff.res.mjs";
import * as Inline from "../src/LambdaInlineDiff.res.mjs";

function changes(before, after) {
  const result = compare(before, after);
  assert.equal(result.TAG, "Changes");
  const lines = result._0;
  for (const [side, text] of [
    ["before", before],
    ["after", after],
  ]) {
    const original = lines.filter(line => line[side] !== undefined);
    assert.equal(original.map(line => line.text).join("\n"), text);
    assert.deepEqual(
      original.map(line => line[side]),
      original.map((_, index) => index + 1),
    );
  }
  return lines;
}

test("identical dumps, including empty dumps, need no diff", () => {
  assert.equal(compare("", ""), "Identical");
  assert.equal(compare("(makeblock 42)\n", "(makeblock 42)\n"), "Identical");
});

test("replacement retains before and after line numbers", () => {
  assert.deepEqual(
    changes("(let\n  unused/1\n  result)\n", "(let\n  result)\n"),
    [
      { kind: "Same", text: "(let", before: 1, after: 1 },
      { kind: "Removed", text: "  unused/1", before: 2, after: undefined },
      { kind: "Same", text: "  result)", before: 3, after: 2 },
      { kind: "Same", text: "", before: 4, after: 3 },
    ],
  );
  assert.deepEqual(
    changes("before", "after").map(line => line.kind),
    ["Removed", "Added"],
  );
});

test("insertions, deletions, empty inputs and final newlines are lossless", () => {
  for (const [before, after] of [
    ["", "(block 42)"],
    ["(block 42)", ""],
    ["a\nb", "a\ninsert\nb"],
    ["a\nb", "insert\na\nb\nend"],
    ["a\n", "a"],
    ["a", "a\n"],
    ["\n\n", "\n"],
  ])
    changes(before, after);
});

test("repeated Lambda lines align deterministically without dropping text", () => {
  const before = "(let\n  x/1\n  (let\n  x/1\n  ))";
  const after = "(let\n  (let\n  x/1\n  ))";
  assert.equal(
    changes(before, after).filter(line => line.kind !== "Same").length,
    1,
  );
  assert.deepEqual(compare(before, after), compare(before, after));
  changes("x/1", "x/2"); // Identifier changes must not be normalized away.
});

test("exhaustive small dumps reconstruct both inputs", () => {
  const dumps = [""];
  for (let length = 1; length <= 4; length++) {
    for (let bits = 0; bits < 2 ** length; bits++) {
      dumps.push(
        Array.from({ length }, (_, i) =>
          (bits >> i) & 1 ? "(x)" : "(y)",
        ).join("\n"),
      );
    }
  }
  for (const before of dumps) {
    for (const after of dumps) {
      if (before !== after) changes(before, after);
    }
  }
});

test("bounds comparison work and output size", () => {
  assert.equal(compare("a\n".repeat(1100), "b\n".repeat(1100)), "TooLarge");
  assert.equal(compare("a".repeat(maxCharacters), "b"), "TooLarge");
  assert.equal(compare("\n".repeat(maxLines), ""), "TooLarge");
});

test("trims common edges before applying the comparison budget", () => {
  const edge = "(unchanged)\n".repeat(2000);
  const lines = changes(`${edge}before\n${edge}`, `${edge}after\n${edge}`);
  assert.equal(lines.filter(line => line.kind !== "Same").length, 2);
});

test("collapsed regions preserve every line and keep context around changes", () => {
  const context = Array.from({ length: 20 }, (_, i) => `line ${i}`).join("\n");
  const lines = changes(
    `${context}\nold\n${context}\nold\n${context}`,
    `${context}\nnew\n${context}\nnew\n${context}`,
  );
  const grouped = sections(lines);
  assert.deepEqual(
    grouped.flatMap(section => section._0),
    lines,
  );
  const hidden = grouped.filter(section => section.TAG === "Collapsed");
  assert.deepEqual(
    hidden.map(section => section._0.length),
    [17, 14, 17],
  );
  for (const section of hidden) {
    assert.ok(section._0.every(line => line.kind === "Same"));
  }
  assert.ok(
    sections(changes("a\nb\nc", "a\nB\nc")).every(
      section => section.TAG === "Visible",
    ),
  );
});

function inlineChanges(before, after) {
  const lines = changes(before, after);
  const highlights = Inline.highlight(lines);
  for (const line of lines) {
    const parts = Inline.forLine(highlights, line);
    if (parts) assert.equal(parts.map(part => part.text).join(""), line.text);
    if (line.kind === "Same") assert.equal(parts, undefined);
  }
  return highlights;
}

const changedText = parts =>
  parts.filter(part => part.changed).map(part => part.text);

test("inline diff highlights changed Lambda tokens, not the shared expression", () => {
  const result = inlineChanges("  (+ value/1 10)", "  (+ value/1 20)");
  assert.deepEqual(changedText(result.before[1]), ["10"]);
  assert.deepEqual(changedText(result.after[1]), ["20"]);
  const renamed = inlineChanges("(apply value/123 42)", "(apply value/124 42)");
  assert.deepEqual(changedText(renamed.before[1]), ["value/123"]);
  assert.deepEqual(changedText(renamed.after[1]), ["value/124"]);
});

test("inline diff preserves whitespace, Unicode, and escaped quoted constants", () => {
  const result = inlineChanges(
    '  (apply print/1 "a \\"quoted\\" 🙂")',
    '\t(apply print/1 "b \\"quoted\\" 🙂")',
  );
  assert.deepEqual(changedText(result.before[1]), ['"a \\"quoted\\" 🙂"']);
  assert.deepEqual(changedText(result.after[1]), ['"b \\"quoted\\" 🙂"']);
  const whitespace = inlineChanges(
    "  (apply café/1 42)",
    "\t(apply café/1 42)",
  );
  assert.deepEqual(changedText(whitespace.before[1]), []);
  assert.deepEqual(changedText(whitespace.after[1]), []);
});

test("inline diff handles token insertion, deletion and repeated tokens", () => {
  const result = inlineChanges("(apply f/1 x/2 x/2)", "(apply f/1 x/2)");
  assert.deepEqual(changedText(result.before[1]), ["x/2"]);
  assert.deepEqual(changedText(result.after[1]), []);
  const inserted = inlineChanges("(apply f/1 x/2)", "(apply f/1 x/2 42)");
  assert.deepEqual(changedText(inserted.before[1]), []);
  assert.deepEqual(changedText(inserted.after[1]), ["42"]);
});

test("inline pairing skips unmatched lines in an uneven changed block", () => {
  const result = inlineChanges(
    "(unused z/9)\n  (+ x/1 10)\n  (- y/2 30)",
    "  (+ x/1 20)\n  (- y/2 40)",
  );
  assert.equal(result.before[1], undefined);
  assert.deepEqual(changedText(result.before[2]), ["10"]);
  assert.deepEqual(changedText(result.after[1]), ["20"]);
  assert.deepEqual(changedText(result.before[3]), ["30"]);
  assert.deepEqual(changedText(result.after[2]), ["40"]);
});

test("inline pairing prefers a better later match over a weaker first match", () => {
  const result = inlineChanges(
    "(apply other/1 10)\n(apply f/2 20)",
    "(apply f/2 30)",
  );
  assert.equal(result.before[1], undefined);
  assert.deepEqual(changedText(result.before[2]), ["20"]);
  assert.deepEqual(changedText(result.after[1]), ["30"]);
});

test("unrelated expressions and one-sided changes retain only line highlighting", () => {
  for (const [before, after] of [
    ["  (foo/1 10)", "  (bar/2 20)"],
    ["((", "))"],
    ["(a)\n(b)", "(a)"],
    ["(a)", "(a)\n(b)"],
  ]) {
    assert.deepEqual(inlineChanges(before, after), { before: {}, after: {} });
  }
});

test("inline highlighting falls back for oversized blocks, lines and token counts", () => {
  const largeBlock = Array.from(
    { length: Inline.maxBlockLines },
    (_, i) => `(apply f/${i} 10)`,
  ).join("\n");
  assert.deepEqual(
    inlineChanges(largeBlock, largeBlock.replaceAll(" 10)", " 20)")),
    { before: {}, after: {} },
  );
  for (const body of [
    "x".repeat(Inline.maxLineCharacters),
    "x ".repeat(Inline.maxTokens),
  ]) {
    assert.deepEqual(
      inlineChanges(`(apply ${body} 10)`, `(apply ${body} 20)`),
      { before: {}, after: {} },
    );
  }
});

test("inline token work is budgeted across blocks and resets for each dump", () => {
  const body = "x ".repeat(110);
  const before = Array.from(
    { length: 30 },
    (_, i) => `(apply ${body} 10)\nseparator/${i}`,
  ).join("\n");
  const after = before.replaceAll(" 10)", " 20)");
  const result = inlineChanges(before, after);
  assert.ok(Object.keys(result.before).length > 0);
  assert.ok(Object.keys(result.before).length < 30);
  assert.deepEqual(inlineChanges(before, after), result);
});
