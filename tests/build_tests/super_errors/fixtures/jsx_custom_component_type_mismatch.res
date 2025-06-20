@@config({
  flags: ["-bs-jsx", "4"],
})

module React = {
  type element = Jsx.element
  type componentLike<'props, 'return> = 'props => 'return
  type component<'props> = Jsx.component<'props>

  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"

  type fragmentProps = {children?: element}
  @module("react/jsx-runtime") external jsxFragment: component<fragmentProps> = "Fragment"

  external float: float => element = "%identity"
}

module CustomComponent = {
  @react.component
  let make = (~someOpt=?) => {
    React.float(
      switch someOpt {
      | Some(5.) => 1.
      | _ => 2.
      },
    )
  }
}

let x = <CustomComponent someOpt="hello" />
