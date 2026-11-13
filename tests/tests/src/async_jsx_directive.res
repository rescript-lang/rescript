@@config({
  flags: ["-bs-jsx", "4"],
})

// A directive on an async component belongs on the async function itself,
// not on the generated sync wrapper: 'use cache' on a sync function is
// rejected by Next.js.

module Cached = {
  @react.component
  let make =
    @directive("'use cache'") async (~id: string) => <div>{React.string(id)}</div>
}

module CachedWithProps = {
  type props = {id: string}

  @react.componentWithProps
  let make =
    @directive("'use cache'") async (props: props) => <div>{React.string(props.id)}</div>
}

// A sync component's wrapper inlines its body, so it keeps the directive.
module NoMemo = {
  @react.component
  let make =
    @directive("'use no memo'") (~id: string) => <div>{React.string(id)}</div>
}
