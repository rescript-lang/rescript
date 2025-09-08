/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @get @return(nullable) external assignedSlot: T.t => option<Dom.htmlSlotElement> = ""
}
