module Outer = {
  @react.component
  let make = () => {
    module Inner = {
      @react.component
      let make = () => <div />
    }

    <Inner />
  }
}