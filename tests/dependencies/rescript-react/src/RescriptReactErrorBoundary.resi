@ocaml.doc("
 * Important note on this module:
 * As soon as React provides a mechanism for error-catching using functional component,
 * this is likely to be deprecated and/or move to user space.
 ")
type info = {componentStack: string}

type params<'error> = {
  error: 'error,
  info: info,
}

@react.component
let make: (~children: React.element, ~fallback: params<'error> => React.element) => React.element
