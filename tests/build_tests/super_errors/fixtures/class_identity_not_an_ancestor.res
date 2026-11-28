module Dom = {
  @instanceOf("Node") type node
  @instanceOf("Element") type element = private node
  @instanceOf("HTMLInputElement") type input = private element
}

module View = {
  @instanceOf("Node") type node
  @instanceOf("Element") type element = private node
}

// An element is not always an input
let asInput = (element: View.element) => (element :> Dom.input)
