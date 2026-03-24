module Internal = {
  @react.component
  let make = (~children) => {
    children
  }
}

module Group = {
  @react.component
  let make = (~children) => {
    <Internal> {children} </Internal>
  }
}

module Other = {
  @react.component
  let make = (~children) => {
    children
  }
}
