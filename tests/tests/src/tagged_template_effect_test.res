open Mocha
open Test_utils

/***
A tagged template calls its tag function, so the call runs even when its result is unused.
*/

@module("./tagged_template_effect_lib.js") @taggedTemplate
external tag: (array<string>, array<int>) => int = "tag"

// Returns the template with its values, read from the strings it was passed
@module("./tagged_template_effect_lib.js") @taggedTemplate
external join: (array<string>, array<int>) => string = "join"

@module("./tagged_template_effect_lib.js")
external on: @string [#click(int) | #close(string)] => string = "on"

@module("./tagged_template_effect_lib.js")
external onRenamed: @string [@as("renamed") #click(int) | #close(string)] => string = "on"

@module("./tagged_template_effect_lib.js")
external ignoreFirst: (@ignore int, int) => int = "ignoreFirst"

@module("./tagged_template_effect_lib.js") external calls: unit => int = "calls"

@module("./tagged_template_effect_lib.js") external reset: unit => unit = "reset"

ignore(tag`toplevel ${1}`)

let callsAtLoad = calls()

let unusedResult = () => {
  let _ = tag`local ${2}`
}

let unusedBinding = () => {
  let unused = tag`binding ${3}`
  calls() + 10
}

let ignored = () => ignore(tag`ignored ${4}`)

let pipedToIgnore = () => tag`piped ${5}`->ignore

let discardedArray = () => ignore([tag`array ${6}`])

let enabled = ref(true)

let inBranch = cond =>
  if cond {
    ignore(tag`branch ${7}`)
  }

let payload = () => on(#click(tag`payload`))

let renamedPayload = () => onRenamed(#click(tag`payload`))

let nextPayload = ref(0)

let next = () => {
  nextPayload := nextPayload.contents + 1
  nextPayload.contents
}

let callPayload = () => on(#click(next()))

let renamedCallPayload = () => onRenamed(#click(next()))

let ignoredArgument = () => ignoreFirst(tag`ignored argument`, 8)

let forStart = () => {
  let sum = ref(0)
  for i in tag`start` to 3 {
    sum := sum.contents + i
  }
  sum.contents
}

let andOperand = cond => cond && tag`operand` === 1

let orOperand = cond => cond || tag`operand` === 1

let conditionalValue = cond => join`value ${cond ? 1 : 2}` ++ "!"

let backtickParts = () => join(["a`b", "${c}", "\n\\`\"\`"], [1, 2])

let dollarParts = () => join(["$", "$a{", "a${b", "${", "$${$"], [1, 2, 3, 4])

let backslashParts = () => join(["a\\b", "\\", "\\`", "\\${"], [1, 2, 3])

let escapedParts = () => join([`a\`b`, `\${c}`], [1])

let moduleNamePart = () => join([__MODULE__, ""], [1])

let jsonParts = () => join([json`"a\n`, json`\`\${c}`], [1])

let dynamicPart = s => join([s, "!"], [1])

let unusedDynamicPart = s => ignore(join([s, "!"], [1]))

let tooFewValues = () => join(["a", "b", "c"], [1])

let tooManyValues = () => join(["a", "b"], [1, 2])

describe(__MODULE__, () => {
  test("a top-level unused tagged template", () => eq(__LOC__, callsAtLoad, 1))

  test("an unused tagged template", () => {
    reset()
    unusedResult()
    eq(__LOC__, calls(), 1)
  })

  test("an unused named binding of a tagged template", () => {
    reset()
    eq(__LOC__, unusedBinding(), 11)
  })

  test("an ignored tagged template", () => {
    reset()
    ignored()
    eq(__LOC__, calls(), 1)
  })

  test("a tagged template piped to ignore", () => {
    reset()
    pipedToIgnore()
    eq(__LOC__, calls(), 1)
  })

  test("a tagged template in a discarded array", () => {
    reset()
    discardedArray()
    eq(__LOC__, calls(), 1)
  })

  test("a tagged template in an if branch", () => {
    reset()
    enabled := false
    inBranch(enabled.contents)
    eq(__LOC__, calls(), 0)
    enabled := true
    inBranch(enabled.contents)
    eq(__LOC__, calls(), 1)
  })

  test("a @string payload is evaluated once", () => {
    reset()
    eq(__LOC__, payload(), "click:1")
    eq(__LOC__, calls(), 1)
    eq(__LOC__, renamedPayload(), "renamed:1")
    eq(__LOC__, calls(), 2)
    nextPayload := 0
    eq(__LOC__, callPayload(), "click:1")
    eq(__LOC__, renamedCallPayload(), "renamed:2")
    eq(__LOC__, nextPayload.contents, 2)
  })

  test("a tagged template as an @ignore argument", () => {
    reset()
    eq(__LOC__, ignoredArgument(), 8)
    eq(__LOC__, calls(), 1)
  })

  test("a tagged template as a for loop start", () => {
    reset()
    eq(__LOC__, forStart(), 6)
    eq(__LOC__, calls(), 1)
  })

  test("a tagged template as a boolean operand", () => {
    reset()
    eq(__LOC__, andOperand(!enabled.contents), false)
    eq(__LOC__, orOperand(enabled.contents), true)
    eq(__LOC__, calls(), 0)
    eq(__LOC__, andOperand(enabled.contents), true)
    eq(__LOC__, orOperand(!enabled.contents), true)
    eq(__LOC__, calls(), 2)
  })

  test("an interpolated conditional", () => {
    eq(__LOC__, conditionalValue(enabled.contents), "value 1!")
    eq(__LOC__, conditionalValue(!enabled.contents), "value 2!")
  })

  test("string literals passed in function form", () => {
    eq(__LOC__, backtickParts(), "a`b1${c}2\n\\`\"`")
    eq(__LOC__, dollarParts(), "$1$a{2a${b3${4$${$")
    eq(__LOC__, backslashParts(), "a\\b1\\2\\`3\\${")
    eq(__LOC__, escapedParts(), "a`b1${c}")
    eq(__LOC__, moduleNamePart(), "Tagged_template_effect_test1")
    eq(__LOC__, jsonParts(), "\"a\\n1\\`\\${c}")
  })

  test("strings that are not the parts of a template", () => {
    eq(__LOC__, dynamicPart("a"), "a1!")
    reset()
    unusedDynamicPart("a")
    eq(__LOC__, calls(), 1)
    eq(__LOC__, tooFewValues(), "a1bundefinedc")
    eq(__LOC__, tooManyValues(), "a1b")
  })
})
