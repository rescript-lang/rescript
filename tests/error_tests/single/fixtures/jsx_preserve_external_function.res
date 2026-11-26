@@config({flags: ["-bs-jsx-preserve"]})
@@jsxConfig({version: 4, module_: "Preact"})

// With abstract components, the function-style external from #8047 is rejected.
module Preact = {
  type element = Jsx.element
  type component<'props> = Jsx.component<'props>

  @module("preact/jsx-runtime")
  external jsx: (component<'props>, 'props) => element = "jsx"

  type domProps = {children?: element}

  module Elements = {
    external someElement: element => option<element> = "%identity"

    @module("preact/jsx-runtime")
    external jsx: (string, domProps) => element = "jsx"
  }
}

module Head = {
  type props = {children?: Preact.element}

  @module("some-lib")
  external make: props => Preact.element = "Head"
}

let test =
  <Head>
    <div />
  </Head>
