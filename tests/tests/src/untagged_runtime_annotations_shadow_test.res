open Mocha
open Test_utils

%%raw(`
globalThis.UntaggedShadowed = class UntaggedShadowed {}
globalThis.UntaggedNs = {Klass: class Klass {}}
`)

// Bindings named like the globals the checks below refer to
module UntaggedShadowed = {
  let name = "local module"
}
module UntaggedNs = {
  let name = "local module"
}

@instanceOf("UntaggedShadowed") type shadowed
@new external makeShadowed: unit => shadowed = "UntaggedShadowed"

@instanceOf("UntaggedNs.Klass") type klass
@new @scope(("globalThis", "UntaggedNs")) external makeKlass: unit => klass = "Klass"

@unboxed
type value = Shadowed(shadowed) | Klass(klass) | Text(string)

let kind = value =>
  switch value {
  | Shadowed(_) => "shadowed"
  | Klass(_) => "klass"
  | Text(text) => text
  }

describe(__MODULE__, () => {
  test("a class check reads a shadowed global", () => {
    eq(__LOC__, "local module", UntaggedShadowed.name ++ "")
    eq(__LOC__, "shadowed", kind(Shadowed(makeShadowed())))
  })

  test("a class check at a dotted path reads a shadowed global", () => {
    eq(__LOC__, "local module", UntaggedNs.name ++ "")
    eq(__LOC__, "klass", kind(Klass(makeKlass())))
    eq(__LOC__, "text", kind(Text("text")))
  })
})
