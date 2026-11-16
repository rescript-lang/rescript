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

// Another package's declarations of the same classes, with what it uses them
// for
module View = {
  @instanceOf("ClassIdentityNode") type node
  @instanceOf("ClassIdentityElement") type element = private node
  @instanceOf("ClassIdentityEvent") type event

  type props = {container: element, label?: string}
  type mounted = Mounted(element)

  @unboxed type child = Element(element) | Text(string)

  let render = child =>
    switch child {
    | Element(_) => "element"
    | Text(text) => text
    }

  let mount = (container: element) => Mounted(container)
  let containerOf = ({container}) => container
  let handle = (listener: event => unit, event: event) => listener(event)
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

  test("an argument is accepted for the parameter's declaration of its class", () => {
    let View.Mounted(mounted) = View.mount(input)
    Class_identity_dom.focus(mounted)
    ok(__LOC__, Class_identity_dom.focused(mounted))
    ok(__LOC__, same(View.containerOf({container: input}), input))
  })

  test("a constructor payload is checked by the class it is declared with", () => {
    eq(__LOC__, "element", View.render(Element(input)))
    eq(__LOC__, "text", View.render(Text("text")))
  })

  test("a function over one declaration is accepted for another", () => {
    let received = ref(false)
    let listener = (_: Class_identity_dom.event) => received := true
    View.handle(listener, Class_identity_dom.makeEvent())
    ok(__LOC__, received.contents)
  })
})
