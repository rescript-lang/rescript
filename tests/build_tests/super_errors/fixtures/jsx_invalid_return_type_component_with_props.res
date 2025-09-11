@@config({
  flags: ["-bs-jsx", "4"],
})

module React = {
  type element = Jsx.element
  type componentLike<'props, 'return> = 'props => 'return
  type component<'props> = Jsx.component<'props>
}

module BadComponent = {
  @react.componentWithProps
  let make = _props => {
    42 // This should be an error - returning int instead of React.element
  }
}
