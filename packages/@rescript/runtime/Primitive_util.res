module Js = Primitive_js_extern

let raiseWhenNotFound = x =>
  if Js.testAny(x) {
    throw(Not_found)
  } else {
    x
  }
