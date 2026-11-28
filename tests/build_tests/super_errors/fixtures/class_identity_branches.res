module Dom = {
  @instanceOf("Element") type element
}

module View = {
  @instanceOf("Element") type element
}

// A branch has no declared type: the second is
// not coerced to the type of the first
let pick = (first, a: View.element, b: Dom.element) => first ? a : b
