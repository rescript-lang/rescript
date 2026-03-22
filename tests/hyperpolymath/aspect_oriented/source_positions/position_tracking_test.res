// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Aspect-oriented test: Source position tracking
// Cross-cutting concern: __POS__, __LOC__, __MODULE__, __LINE__ must produce
// correct values throughout the compilation pipeline.

let eq = (a, b) => a == b

// ─── __POS__ tracking ──────────────────────────────────────────────
let (file, line, colStart, colEnd) = __POS__
Test.run(
  __POS_OF__("pos: file contains test name"),
  String.includes(file, "position_tracking_test"),
  eq,
  true,
)
Test.run(__POS_OF__("pos: line positive"), line > 0, eq, true)
Test.run(__POS_OF__("pos: colStart positive"), colStart >= 0, eq, true)
Test.run(__POS_OF__("pos: colEnd >= colStart"), colEnd >= colStart, eq, true)

// ─── __LOC__ tracking ──────────────────────────────────────────────
let loc = __LOC__
Test.run(
  __POS_OF__("loc: contains filename"),
  String.includes(loc, "position_tracking_test"),
  eq,
  true,
)

// ─── __MODULE__ tracking ───────────────────────────────────────────
let modName = __MODULE__
Test.run(
  __POS_OF__("module: not empty"),
  String.length(modName) > 0,
  eq,
  true,
)

// ─── __LINE__ tracking ─────────────────────────────────────────────
let line1 = __LINE__
let line2 = __LINE__
Test.run(__POS_OF__("line: sequential"), line2 > line1, eq, true)
Test.run(__POS_OF__("line: difference"), line2 - line1, eq, 1)

// ─── __POS_OF__ with named positions ────────────────────────────────
let (pos, name) = __POS_OF__("my test position")
let (posFile, posLine, _, _) = pos
Test.run(
  __POS_OF__("pos_of: name"),
  name,
  eq,
  "my test position",
)
Test.run(
  __POS_OF__("pos_of: file"),
  String.includes(posFile, "position_tracking_test"),
  eq,
  true,
)
Test.run(__POS_OF__("pos_of: line positive"), posLine > 0, eq, true)

// ─── Position in nested expressions ─────────────────────────────────
let nestedPos = {
  let inner = __POS__
  let (_, innerLine, _, _) = inner
  innerLine
}
Test.run(__POS_OF__("pos: nested block"), nestedPos > 0, eq, true)

// ─── Position in function body ──────────────────────────────────────
let fnWithPos = () => {
  let (_, line, _, _) = __POS__
  line
}
let fnLine = fnWithPos()
Test.run(__POS_OF__("pos: inside function"), fnLine > 0, eq, true)

// ─── Position in pattern match branches ─────────────────────────────
let matchPos = x =>
  switch x {
  | 1 =>
    let (_, line, _, _) = __POS__
    line
  | 2 =>
    let (_, line, _, _) = __POS__
    line
  | _ =>
    let (_, line, _, _) = __POS__
    line
  }

let branch1Line = matchPos(1)
let branch2Line = matchPos(2)
let branch3Line = matchPos(3)
Test.run(__POS_OF__("pos: match branch 1"), branch1Line > 0, eq, true)
Test.run(__POS_OF__("pos: match branch 2"), branch2Line > branch1Line, eq, true)
Test.run(__POS_OF__("pos: match branch 3"), branch3Line > branch2Line, eq, true)

// ─── Position in module ─────────────────────────────────────────────
module PosModule = {
  let modulePos = __POS__
  let moduleLine = __LINE__
  let moduleMod = __MODULE__
}

let (_, modPosLine, _, _) = PosModule.modulePos
Test.run(__POS_OF__("pos: in module"), modPosLine > 0, eq, true)
Test.run(__POS_OF__("pos: module line"), PosModule.moduleLine > 0, eq, true)
Test.run(
  __POS_OF__("pos: module name in module"),
  String.length(PosModule.moduleMod) > 0,
  eq,
  true,
)

// ─── Position consistency across calls ──────────────────────────────
let posArray = Array.fromInitializer(~length=5, _i => {
  let (_, line, _, _) = __POS__
  line
})
// All positions should be the same since they're in the same initializer
Test.run(
  __POS_OF__("pos: consistent in init"),
  posArray->Array.every(l => l == posArray[0]),
  eq,
  true,
)

// ─── Multiple __POS__ on same line ──────────────────────────────────
let (_, lineA, colA, _) = __POS__
Test.run(__POS_OF__("pos: same line col"), lineA > 0 && colA >= 0, eq, true)

// ─── Position in closures ──────────────────────────────────────────
let makePositionCapture = () => {
  let captured = __POS__
  () => captured
}
let getPos = makePositionCapture()
let (_, closureLine, _, _) = getPos()
Test.run(__POS_OF__("pos: captured in closure"), closureLine > 0, eq, true)

// ─── Position in exception handling ─────────────────────────────────
let exnPos =
  try {
    let (_, line, _, _) = __POS__
    raise(Failure(Int.toString(line)))
  } catch {
  | Failure(msg) => Int.fromString(msg)->Option.getOr(0)
  | _ => 0
  }
Test.run(__POS_OF__("pos: in try block"), exnPos > 0, eq, true)

// ─── Position comparison across files (self-referential) ────────────
// The line numbers should increase monotonically within a file
let earlyLine = __LINE__
// ... many lines of code above ...
let lateLine = __LINE__
Test.run(
  __POS_OF__("pos: monotonic lines"),
  lateLine > earlyLine,
  eq,
  true,
)
