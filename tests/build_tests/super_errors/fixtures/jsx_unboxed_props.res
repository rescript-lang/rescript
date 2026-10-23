module React = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>
  external string: string => element = "%identity"
  @module("react/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"
}

@unboxed type props = {title: string}

module Title = {
  @val external make: React.component<props> = "Title"
}

let el = <Title title="x" />
