module React = {
  type element
  type componentLike<'props, 'return> = 'props => 'return
}

module Test = {
  @module @react.component
  external make: (~className: string=?) => React.element = "Foo"
}

let test = (~className) => Test.make({className})
