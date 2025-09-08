type synthetic<'a> = ReactEvent.synthetic<'a>

module MakeSyntheticWrapper = (
  Type: {
    type t
  },
) => {
  @get external bubbles: Type.t => bool = "bubbles"
  @get external cancelable: Type.t => bool = "cancelable"
  @get
  external currentTarget: Type.t => Dom.element = "currentTarget" /* Should return Dom.evetTarget */
  @get external defaultPrevented: Type.t => bool = "defaultPrevented"
  @get external eventPhase: Type.t => int = "eventPhase"
  @get external isTrusted: Type.t => bool = "isTrusted"
  @get external nativeEvent: Type.t => {..} = "nativeEvent" /* Should return Dom.event */
}

module Synthetic = {
  type tag = ReactEvent.Synthetic.tag
  type t = ReactEvent.Synthetic.t
  @get external bubbles: synthetic<'a> => bool = "bubbles"
  @get external cancelable: synthetic<'a> => bool = "cancelable"
  @get
  external currentTarget: synthetic<'a> => Dom.element =
    "currentTarget" /* Should return Dom.evetTarget */
  @get
  external defaultPrevented: synthetic<'a> => bool = "defaultPrevented"
  @get external eventPhase: synthetic<'a> => int = "eventPhase"
  @get external isTrusted: synthetic<'a> => bool = "isTrusted"
  @get
  external nativeEvent: synthetic<'a> => {..} = "nativeEvent" /* Should return Dom.event */
}

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic<'a> => Synthetic.t = "%identity"

module Clipboard = {
  type tag = ReactEvent.Clipboard.tag
  type t = ReactEvent.Clipboard.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external clipboardData: t => {..} = "clipboardData" /* Should return Dom.dataTransfer */
}

module Composition = {
  type tag = ReactEvent.Composition.tag
  type t = ReactEvent.Composition.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external data: t => string = "data"
}

module Keyboard = {
  type tag = ReactEvent.Keyboard.tag
  type t = ReactEvent.Keyboard.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external charCode: t => int = "charCode"
  @get external ctrlKey: t => bool = "ctrlKey"
  @get external key: t => string = "key"
  @get external keyCode: t => int = "keyCode"
  @get external locale: t => string = "locale"
  @get external location: t => int = "location"
  @get external metaKey: t => bool = "metaKey"
  @get external repeat: t => bool = "repeat"
  @get external shiftKey: t => bool = "shiftKey"
  @get external which: t => int = "which"
}

module Focus = {
  type tag = ReactEvent.Focus.tag
  type t = ReactEvent.Focus.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get
  external relatedTarget: t => Dom.element = "relatedTarget" /* Should return Dom.eventTarget */
}

module Form = {
  type tag = ReactEvent.Form.tag
  type t = ReactEvent.Form.t
  include MakeSyntheticWrapper({
    type t = t
  })
}

module Mouse = {
  type tag = ReactEvent.Mouse.tag
  type t = ReactEvent.Mouse.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external button: t => int = "button"
  @get external buttons: t => int = "buttons"
  @get external clientX: t => int = "clientX"
  @get external clientY: t => int = "clientY"
  @get external ctrlKey: t => bool = "ctrlKey"
  @get external metaKey: t => bool = "metaKey"
  @get external pageX: t => int = "pageX"
  @get external pageY: t => int = "pageY"
  @get
  external relatedTarget: t => Dom.element = "relatedTarget" /* Should return Dom.eventTarget */
  @get external screenX: t => int = "screenX"
  @get external screenY: t => int = "screenY"
  @get external shiftKey: t => bool = "shiftKey"
}

module Selection = {
  type tag = ReactEvent.Selection.tag
  type t = ReactEvent.Selection.t
  include MakeSyntheticWrapper({
    type t = t
  })
}

module Touch = {
  type tag = ReactEvent.Touch.tag
  type t = ReactEvent.Touch.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external changedTouches: t => {..} = "changedTouches" /* Should return Dom.touchList */
  @get external ctrlKey: t => bool = "ctrlKey"
  @get external metaKey: t => bool = "metaKey"
  @get external shiftKey: t => bool = "shiftKey"
  @get external targetTouches: t => {..} = "targetTouches" /* Should return Dom.touchList */
  @get external touches: t => {..} = "touches" /* Should return Dom.touchList */
}

module UI = {
  type tag = ReactEvent.UI.tag
  type t = ReactEvent.UI.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external detail: t => int = "detail"
  @get external view: t => Dom.window = "view" /* Should return DOMAbstractView/WindowProxy */
}

module Wheel = {
  type tag = ReactEvent.Wheel.tag
  type t = ReactEvent.Wheel.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external deltaMode: t => int = "deltaMode"
  @get external deltaX: t => float = "deltaX"
  @get external deltaY: t => float = "deltaY"
  @get external deltaZ: t => float = "deltaZ"
}

module Media = {
  type tag = ReactEvent.Media.tag
  type t = ReactEvent.Media.t
  include MakeSyntheticWrapper({
    type t = t
  })
}

module Image = {
  type tag = ReactEvent.Image.tag
  type t = ReactEvent.Image.t
  include MakeSyntheticWrapper({
    type t = t
  })
}

module Animation = {
  type tag = ReactEvent.Animation.tag
  type t = ReactEvent.Animation.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external animationName: t => string = "animationName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}

module Transition = {
  type tag = ReactEvent.Transition.tag
  type t = ReactEvent.Transition.t
  include MakeSyntheticWrapper({
    type t = t
  })
  @get external propertyName: t => string = "propertyName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}
