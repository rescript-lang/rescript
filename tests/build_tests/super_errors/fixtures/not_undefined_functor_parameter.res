module Make = (
  X: {
    type t
  },
) => {
  module Checked: {
    @notUndefined type t
  } = {
    type t = X.t
  }
}
