@genType.import("./shims/JsxEvent.shim")
type inputFocusEvent = ReactDOMEvent.Focus.t

@genType.import("./MyInput") @react.component
external make: (~onFocus: inputFocusEvent => unit=?) => React.element = "default"
