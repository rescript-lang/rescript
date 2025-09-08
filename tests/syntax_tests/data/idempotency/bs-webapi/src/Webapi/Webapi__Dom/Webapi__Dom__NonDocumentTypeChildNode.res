/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @get @return(nullable) external previousElementSibling: T.t => option<Dom.element> = ""
  @get @return(nullable) external nextElementSibling: T.t => option<Dom.element> = ""
}
