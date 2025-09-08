@ocaml.doc(
  " This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason "
)
@genType
@react.component
let make = (~title, ~description, ~button=?) => {
  <div>
    <div>
      title
      description
      {switch button {
      | Some(button) => button
      | None => React.null
      }}
    </div>
  </div>
}
