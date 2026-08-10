let raiseWhenNotFound = x =>
  if Primitive_js_extern.testAny(x) {
    throw(Not_found)
  } else {
    x
  }
