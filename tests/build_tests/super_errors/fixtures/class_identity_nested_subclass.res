module Dom = {
  @instanceOf("Element") type element
  @instanceOf("HTMLInputElement") type input = private element
}

module View = {
  @instanceOf("Element") type element
  let mount = (_: option<element>) => ()
}

// Without `:>`, a subclass is accepted only as
// the argument's own type
let mountInput = (input: option<Dom.input>) => View.mount(input)
