module Async = {
  @react.component
  let make =
    @directive("'use cache';") async (~id: string) => <div>{React.string(id)}</div>
}

module AsyncWithProps = {
  type props = {id: string}

  @react.componentWithProps
  let make =
    @directive("'use cache';") async (props: props) => <div>{React.string(props.id)}</div>
}

module Sync = {
  @react.component
  let make =
    @directive("'use no memo';") (~id: string) => <div>{React.string(id)}</div>
}
