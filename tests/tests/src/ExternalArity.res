@@uncurried

@@config({
  flags: ["-bs-jsx", "4"],
})

type u = (int, int) => int
@val external v1: u = "v" // arity from u is implicit
@val external v2: (int, int) => int = "v" // arity is explicit

let f1 = x => v1(x, x)
let f2 = x => v2(x, x)

module FromTypeConstructor = {
  type fn<'props, 'return> = 'props => 'return

  external foo1: fn<int, int> = "foo1"
  let test1 = foo1(10)

  external foo2: int => int = "foo2"
  let test2 = foo2(20)

  external foo3: (~x: int=?, ~y: int) => int = "foo3"
  let test3 = foo3(~y=3)
}

module ReactTest = {
  module React = {
    type element = Jsx.element
    type componentLike<'props, 'return> = 'props => 'return
    type component<'props> = Jsx.component<'props>

    @module("react/jsx-runtime")
    external jsx: (component<'props>, 'props) => element = "jsx"
  }

  module FormattedMessage = {
    @react.component @module("react-intl")
    external make: (~id: string, ~defaultMessage: string) => React.element = "FormattedMessage"
  }
  let _ = <FormattedMessage id="test" defaultMessage="Test" />
}
