import assert from "node:assert/strict";
import test from "node:test";

import {
  current,
  finishDrag,
  make,
  moveDrag,
  nudge,
  orientationForWidth,
  ratioForPosition,
  setOrientation,
  startDrag,
} from "../src/PaneLayout.res.mjs";

const closeTo = (actual, expected) =>
  assert.ok(
    Math.abs(actual - expected) < 0.000001,
    `${actual} is not close to ${expected}`,
  );

test("switches to stacked panes from the container width", () => {
  assert.equal(orientationForWidth(901), "Columns");
  assert.equal(orientationForWidth(900), "Rows");
});

test("calculates a divider ratio and respects both pane minimums", () => {
  closeTo(ratioForPosition(500, 1000, 360, 420), 0.5);
  closeTo(ratioForPosition(0, 1000, 360, 420), 360 / 992);
  closeTo(ratioForPosition(1000, 1000, 360, 420), 1 - 420 / 992);
  closeTo(ratioForPosition(100, 300, 180, 180), 0.5);
});

test("keeps drag and ratio state isolated between layout instances", () => {
  const first = make();
  const second = make();

  assert.notEqual(first.containerId, second.containerId);
  startDrag(first, 7, 600, 1000, 360, 420);

  assert.equal(current(first).dragging, true);
  assert.equal(current(second).dragging, false);
  assert.equal(current(second).columnRatio, 0.5);

  const draggedRatio = current(first).columnRatio;
  moveDrag(first, 8, 450);
  assert.equal(current(first).columnRatio, draggedRatio);
  finishDrag(first, 8);
  assert.equal(current(first).dragging, true);
  finishDrag(first, 7);
  assert.equal(current(first).dragging, false);
});

test("stores independent ratios for columns and stacked rows", () => {
  const layout = make();

  nudge(layout, 0.025, 1000, 360, 420);
  closeTo(current(layout).columnRatio, 0.525);

  setOrientation(layout, orientationForWidth(600));
  nudge(layout, -0.025, 800, 180, 180);
  closeTo(current(layout).columnRatio, 0.525);
  closeTo(current(layout).rowRatio, 0.475);
});
