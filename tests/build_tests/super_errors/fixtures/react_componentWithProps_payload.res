module React = {
  type element = Jsx.element
  type componentLike<'props, 'return> = 'props => 'return
  type component<'props> = Jsx.component<'props>
  external component: componentLike<'props, element> => component<'props> = "%component_identity"
  external string: string => element = "%identity"
}

type props = {label: string}

module C = {
  @react.componentWithProps(: props)
  let make = (props: props) => React.string(props.label)
}

let c = C.make
