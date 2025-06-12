module React = {
  type element = Jsx.element
  @val external null: element = "null"
  type componentLike<'props, 'return> = Jsx.componentLike<'props, 'return>
  type component<'props> = Jsx.component<'props>
  external component: componentLike<'props, element> => component<'props> = "%identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
  external string: string => element = "%identity"
}
module ReactDOM = {
  external someElement: React.element => option<React.element> = "%identity"
  @module("react/jsx-runtime")
  external jsx: (string, JsxDOM.domProps) => Jsx.element = "jsx"
}

module Wrapper = {
  @react.component
  let make = (~value: 'value, ~children: React.element) => {
    <div> {children} </div>
  }
}

module SomeComponent = {
  @react.component
  let make = () => {
    <Wrapper>
      <div> {""->React.string} </div>
    </Wrapper>
  }
}
