open Mocha

describe(__MODULE__, () => {
  test("equivalent string escape spellings in switch cases", () => {
    // Equivalent spellings become duplicate JavaScript cases. Only the first
    // emitted case can match, so guards on the other cases are skipped.
    assert(Test_string_switch.classifyEquivalentEscape("a", 0) == 5)
    assert(Test_string_switch.classifyEquivalentEscape("a", 1) == 5)
    assert(Test_string_switch.classifyEquivalentEscape("a", 2) == 2)
    assert(Test_string_switch.classifyEquivalentEscape("a", 3) == 5)
    assert(Test_string_switch.classifyEquivalentEscape("a", 4) == 5)
  })
})
