@@ocaml.doc("This comment is for the **toplevel** module.")

@ocaml.doc("This comment is for the first **nested** module.")
module Nested = {
  let x = "123"

  @ocaml.doc("This comment is for the inner **nested-again** module.")
  module NestedAgain = {
    let y = 123
  }
}

module M = Nested.NestedAgain
