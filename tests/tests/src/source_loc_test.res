open Mocha
open Test_utils

external unsafeSourceLocPos: string => sourceLocPos = "%identity"
external unsafeSourceLocValuePath: string => sourceLocValuePath = "%identity"

describe("SourceLoc", () => {
  test("Pos.decode parses sourceLocPos", () => {
    let decoded = SourceLoc.Pos.decode(unsafeSourceLocPos("demo.res;1;2;3;4"))
    eq(__LOC__, decoded, Some({
      file: "demo.res",
      startLine: 1,
      startCol: 2,
      endLine: 3,
      endCol: 4,
    }))
  })

  test("Pos.decode rejects malformed sourceLocPos", () => {
    eq(__LOC__, SourceLoc.Pos.decode(unsafeSourceLocPos("bad")), None)
  })

  test("ValuePath helpers expose segments and name", () => {
    let valuePath = unsafeSourceLocValuePath("Demo.Nested.run")
    eq(__LOC__, SourceLoc.ValuePath.segments(valuePath), ["Demo", "Nested", "run"])
    eq(__LOC__, SourceLoc.ValuePath.name(valuePath), "run")
  })
})
