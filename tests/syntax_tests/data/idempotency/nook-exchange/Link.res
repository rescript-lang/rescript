@react.component
let make = (~path, ~children, ~className=?, ~onClick=?, ()) =>
  <a
    href=path
    onClick={e => {
      switch onClick {
      | Some(onClick) => onClick()
      | None => ()
      }
      ReasonReactRouter.push(path)
      ReactEvent.Mouse.preventDefault(e)
    }}
    ?className>
    children
  </a>
