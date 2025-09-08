// Entry point
@val external document: {..} = "document"

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
// time through the examples later.
let makeContainer = text => {
  let container = document["createElement"]("div")
  container["className"] = "container"

  let title = document["createElement"]("div")
  title["className"] = "containerTitle"
  title["innerText"] = text

  let content = document["createElement"]("div")
  content["className"] = "containerContent"

  let () = container["appendChild"](title)
  let () = container["appendChild"](content)
  let () = document["body"]["appendChild"](container)

  content
}

/* This uncurried prop definition compiles */
module Gpr3987ReproOk = {
  type props = {value: string, onChange: (string, int) => unit}

  let make = (_props: props) => React.null
}

let _ = <Gpr3987ReproOk value="test" onChange={(_, _) => ()} />

/* Extracted type for the uncurried prop compiles as well */
module Gpr3987ReproOk2 = {
  type onChange = (string, int) => unit

  @react.component
  let make = (~value as _, ~onChange as _: onChange) => React.null
}

let _ = <Gpr3987ReproOk2 value="test" onChange={(_, _) => ()} />

/* Inline uncurried prop type causes an error */
module Gpr3987ReproError = {
  @react.component
  let make = (~value as _: string, ~onChange as _: (string, int) => unit) => React.null
}

let _ = <Gpr3987ReproError value="test" onChange={(_, _) => ()} />
