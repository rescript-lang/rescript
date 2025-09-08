module Impl = (
  T: {
    type t
  },
) => {
  external asEventTarget: T.t => Dom.eventTarget = "%identity"
}

include Impl({
  type t = Dom.eventTarget
})

external unsafeAsDocument: Dom.eventTarget => Dom.document = "%identity"
external unsafeAsElement: Dom.eventTarget => Dom.element = "%identity"
external unsafeAsWindow: Dom.eventTarget => Dom.window = "%identity"
