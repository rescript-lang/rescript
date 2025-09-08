@@config({
  flags: ["-w", "-16"],
})

module H: {
  @react.component
  let make: (~s: string=?) => React.element
} = {
  @react.component
  let make = (~s=?) =>
    switch s {
    | Some(s) => React.string(s)
    | None => React.null
    }
}
