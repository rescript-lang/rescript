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
}

module CustomComponent = {
  @react.component
  let make = (~children) => {
    <> {children} </>
  }
}

let x = <CustomComponent> {1.} </CustomComponent>
