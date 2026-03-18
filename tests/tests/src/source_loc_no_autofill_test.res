open Mocha
open Test_utils

let capture = (~pos: sourceLocPos=%autofill, ~valuePath: sourceLocValuePath=%autofill) => (
  pos,
  valuePath,
)

describe("SourceLoc without -implicit-source-loc", () => {
  test("missing args fall back to empty source loc values", () => {
    let (pos, valuePath) = capture()
    eq(__LOC__, SourceLoc.Pos.decode(pos), None)
    eq(__LOC__, SourceLoc.ValuePath.segments(valuePath), [])
    eq(__LOC__, SourceLoc.ValuePath.name(valuePath), "")
  })
})
