type t = Dom.keyboardEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})

@new external make: string => t = "KeyboardEvent"
@new external makeWithOptions: (string, {..}) => t = "KeyboardEvent"

@get external altKey: t => bool = ""
@get external code: t => string = ""
@get external ctrlKey: t => bool = ""
@get external isComposing: t => bool = ""
@get external key: t => string = ""
@get external locale: t => string = ""
@get external location: t => int = ""
@get external metaKey: t => bool = ""
@get external repeat: t => bool = ""
@get external shiftKey: t => bool = ""

let getModifierState: (Webapi__Dom__Types.modifierKey, t) => bool = (key, self) =>
  getModifierState(Webapi__Dom__Types.encodeModifierKey(key), self)
