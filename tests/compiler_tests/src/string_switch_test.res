open Mocha

describe(__MODULE__, () => {
  test("equivalent string escape spellings in switch cases", () => {
    assert(Test_string_switch.classifyEquivalentEscape("a", 0) == 0)
    assert(Test_string_switch.classifyEquivalentEscape("a", 1) == 1)
    assert(Test_string_switch.classifyEquivalentEscape("a", 2) == 2)
    assert(Test_string_switch.classifyEquivalentEscape("a", 3) == 3)
    assert(Test_string_switch.classifyEquivalentEscape("a", 4) == 4)
  })

  test("equivalent surrogate-pair escape spellings in switch cases", () => {
    assert(Test_string_switch.classifyEquivalentSurrogateEscape("😀", 0) == 0)
    assert(Test_string_switch.classifyEquivalentSurrogateEscape("😀", 1) == 1)
    assert(Test_string_switch.classifyEquivalentSurrogateEscape("😀", 2) == 2)
  })
})
