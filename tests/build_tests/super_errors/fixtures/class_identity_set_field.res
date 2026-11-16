module Dom = {
  @instanceOf("Element") type element
}

module View = {
  @instanceOf("Element") type element
  type ref = {mutable current: element}
}

// An assignment needs a written `:>`
let set = (ref: View.ref, el: Dom.element) => ref.current = el
