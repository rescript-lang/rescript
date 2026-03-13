module React = {
  type element
  type component<'props> = 'props => element
}

module Test = {
  @module @react.component
  external make: (~className: string=?) => React.element = "Foo"
}

let test = (~className) => Test.make({className})
