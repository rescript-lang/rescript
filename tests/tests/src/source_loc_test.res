@@config({flags: ["-implicit-source-loc"]})

open Mocha
open Test_utils

external unsafeSourceLocPos: string => sourceLocPos = "%identity"
external unsafeSourceLocValuePath: string => sourceLocValuePath = "%identity"

type sourceLocCapture = (option<sourceLocPos>, option<sourceLocValuePath>)

let capture = (~pos: option<sourceLocPos>=?, ~valuePath: option<sourceLocValuePath>=?) => (
  pos,
  valuePath,
)

let topLevelBinding = capture()

let topLevelExpressionCapture = ref((None: option<sourceLocCapture>))

let storeTopLevelExpression = (
  ~pos: option<sourceLocPos>=?,
  ~valuePath: option<sourceLocValuePath>=?,
) => {
  topLevelExpressionCapture.contents = Some((pos, valuePath))
}

storeTopLevelExpression()

module Nested = {
  let nestedBinding = capture()

  let topLevelExpressionCapture = ref((None: option<sourceLocCapture>))

  let storeTopLevelExpression = (
    ~pos: option<sourceLocPos>=?,
    ~valuePath: option<sourceLocValuePath>=?,
  ) => {
    topLevelExpressionCapture.contents = Some((pos, valuePath))
  }

  storeTopLevelExpression()
}

let expectCapture = (loc, (pos, valuePath): sourceLocCapture, expectedValuePath) => {
  switch pos {
  | Some(pos) =>
    switch SourceLoc.Pos.decode(pos) {
    | Some({file}) => eq(loc, file, "source_loc_test.res")
    | None => ok(loc, false)
    }
  | None => ok(loc, false)
  }

  switch valuePath {
  | Some(valuePath) => eq(loc, SourceLoc.ValuePath.toString(valuePath), expectedValuePath)
  | None => ok(loc, false)
  }
}

describe("SourceLoc", () => {
  test("Pos.decode parses sourceLocPos", () => {
    let decoded = SourceLoc.Pos.decode(unsafeSourceLocPos("demo.res;1;2;3;4"))
    eq(
      __LOC__,
      decoded,
      Some({
        file: "demo.res",
        startLine: 1,
        startCol: 2,
        endLine: 3,
        endCol: 4,
      }),
    )
  })

  test("Pos.decode rejects malformed sourceLocPos", () => {
    eq(__LOC__, SourceLoc.Pos.decode(unsafeSourceLocPos("bad")), None)
  })

  test("ValuePath helpers expose segments and name", () => {
    let valuePath = unsafeSourceLocValuePath("Demo.Nested.run")
    eq(__LOC__, SourceLoc.ValuePath.segments(valuePath), ["Demo", "Nested", "run"])
    eq(__LOC__, SourceLoc.ValuePath.name(valuePath), "run")
  })

  test("implicit source loc autofill works for a top-level let binding", () => {
    expectCapture(__LOC__, topLevelBinding, "Source_loc_test.topLevelBinding")
  })

  test("implicit source loc autofill works for a top-level expression", () => {
    switch topLevelExpressionCapture.contents {
    | Some(capture) => expectCapture(__LOC__, capture, "Source_loc_test")
    | None => ok(__LOC__, false)
    }
  })

  test("implicit source loc autofill works for a nested module let binding", () => {
    expectCapture(__LOC__, Nested.nestedBinding, "Source_loc_test.Nested.nestedBinding")
  })

  test("implicit source loc autofill works for a nested module expression", () => {
    switch Nested.topLevelExpressionCapture.contents {
    | Some(capture) => expectCapture(__LOC__, capture, "Source_loc_test.Nested")
    | None => ok(__LOC__, false)
    }
  })
})
