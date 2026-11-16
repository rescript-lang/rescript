open Mocha
open Test_utils

%%raw(`
globalThis.ClassIdentityNode = class ClassIdentityNode {}
globalThis.ClassIdentityElement = class ClassIdentityElement extends globalThis.ClassIdentityNode {
  focus() { this.focused = true }
}
globalThis.ClassIdentityInput = class ClassIdentityInput extends globalThis.ClassIdentityElement {}
globalThis.ClassIdentityEvent = class ClassIdentityEvent {}
`)

// Another package's declarations of the same classes
module View = {
  @instanceOf("ClassIdentityNode") type node
  @instanceOf("ClassIdentityElement") type element = private node
}

let input = Class_identity_dom.makeInput()

// Whether two values of any types are the same JavaScript value
let same = (a: 'a, b: 'b) => a === Obj.magic(b)

describe(__MODULE__, () => {
  test("a class type coerces to another declaration of its class", () => {
    let element = (input :> Class_identity_dom.element)
    ok(__LOC__, same(element :> View.element, input))
    ok(__LOC__, same(input :> View.element, input))
    ok(__LOC__, same(input :> View.node, input))
  })

  test("two declarations of a class are equal inside other types", () => {
    let elements = [input :> Class_identity_dom.element]
    let viewElements = (elements :> array<View.element>)
    eq(__LOC__, 1, Array.length(viewElements))
    let cell = ref(Some(input :> View.element))
    let domCell = (cell :> ref<option<Class_identity_dom.element>>)
    ok(__LOC__, same(domCell, cell))
  })
})
