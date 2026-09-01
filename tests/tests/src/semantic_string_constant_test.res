open Mocha
open Test_utils

/* String.length emits a JavaScript .length access, so it does not exercise
   Lambda constant folding. The primitive below does. Obj.magic exposes the
   string-backed polymorphic variant as a semantic string constant; this used
   to fold the emoji's UTF-8 byte length (4) instead of its JavaScript UTF-16
   length (2).

   This characterization intentionally expects the incorrect result, 4; the
   JavaScript UTF-16 length is 2. */
external stringLength: string => int = "%string_length"

describe(__MODULE__, () => {
  test("constant length uses JavaScript string semantics", () => {
    let semanticEmoji: string = Obj.magic(#"😀")
    eq(__LOC__, stringLength(semanticEmoji), 4)
  })
})
