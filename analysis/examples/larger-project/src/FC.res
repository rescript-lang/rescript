module type ReplacebleComponent = {
  @react.component
  let make: unit => React.element
}

let foo = (~impl: module(ReplacebleComponent)) => {
  let module(X) = impl
  X.make
}

Js.log(foo)
