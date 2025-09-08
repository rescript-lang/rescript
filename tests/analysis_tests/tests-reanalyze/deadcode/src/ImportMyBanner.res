@ocaml.doc("
  * Wrap component MyBanner to be used from Reason.
  ")
@genType
type message = {text: string}

@genType.import("./MyBanner")
external /* Module with the JS component to be wrapped. */
/* The make function will be automatically generated from the types below. */
make: (~show: bool, ~message: option<message>=?, 'a) => React.element = "make"

let make = make
