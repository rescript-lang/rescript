type synthetic<'a>

module MakeEventWithType = (
  Type: {
    type t
  },
) => {
  @get external bubbles: Type.t => bool = "bubbles"
  @get external cancelable: Type.t => bool = "cancelable"
  @get external currentTarget: Type.t => {..} = "currentTarget" /* Should return Dom.eventTarget */
  @get external defaultPrevented: Type.t => bool = "defaultPrevented"
  @get external eventPhase: Type.t => int = "eventPhase"
  @get external isTrusted: Type.t => bool = "isTrusted"
  @get external nativeEvent: Type.t => {..} = "nativeEvent" /* Should return Dom.event */
  @send external preventDefault: Type.t => unit = "preventDefault"
  @send
  external isDefaultPrevented: Type.t => bool = "isDefaultPrevented"
  @send external stopPropagation: Type.t => unit = "stopPropagation"
  @send
  external isPropagationStopped: Type.t => bool = "isPropagationStopped"
  @get external target: Type.t => {..} = "target" /* Should return Dom.eventTarget */
  @get external timeStamp: Type.t => float = "timeStamp"
  @get external type_: Type.t => string = "type"
  @send external persist: Type.t => unit = "persist"
}

module Synthetic = {
  type tag
  type t = synthetic<tag>
  @get external bubbles: synthetic<'a> => bool = "bubbles"
  @get external cancelable: synthetic<'a> => bool = "cancelable"
  @get
  external currentTarget: synthetic<'a> => {..} =
    "currentTarget" /* Should return Dom.eventTarget */
  @get
  external defaultPrevented: synthetic<'a> => bool = "defaultPrevented"
  @get external eventPhase: synthetic<'a> => int = "eventPhase"
  @get external isTrusted: synthetic<'a> => bool = "isTrusted"
  @get
  external nativeEvent: synthetic<'a> => {..} = "nativeEvent" /* Should return Dom.event */
  @send
  external preventDefault: synthetic<'a> => unit = "preventDefault"
  @send
  external isDefaultPrevented: synthetic<'a> => bool = "isDefaultPrevented"
  @send
  external stopPropagation: synthetic<'a> => unit = "stopPropagation"
  @send
  external isPropagationStopped: synthetic<'a> => bool = "isPropagationStopped"
  @get external target: synthetic<'a> => {..} = "target" /* Should return Dom.eventTarget */
  @get external timeStamp: synthetic<'a> => float = "timeStamp"
  @get external type_: synthetic<'a> => string = "type"
  @send external persist: synthetic<'a> => unit = "persist"
}

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic<'a> => Synthetic.t = "%identity"

module Clipboard = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external clipboardData: t => {..} = "clipboardData" /* Should return Dom.dataTransfer */
}

module Composition = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external data: t => string = "data"
}

module Keyboard = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external charCode: t => int = "charCode"
  @get external ctrlKey: t => bool = "ctrlKey"
  @send
  external getModifierState: (t, string) => bool = "getModifierState"
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
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get
  external relatedTarget: t => Js.nullable<{..}> =
    "relatedTarget" /* Should return Dom.eventTarget */
}

module Form = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
}

module Mouse = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external button: t => int = "button"
  @get external buttons: t => int = "buttons"
  @get external clientX: t => int = "clientX"
  @get external clientY: t => int = "clientY"
  @get external ctrlKey: t => bool = "ctrlKey"
  @send
  external getModifierState: (t, string) => bool = "getModifierState"
  @get external metaKey: t => bool = "metaKey"
  @get external pageX: t => int = "pageX"
  @get external pageY: t => int = "pageY"
  @get
  external relatedTarget: t => Js.nullable<{..}> =
    "relatedTarget" /* Should return Dom.eventTarget */
  @get external screenX: t => int = "screenX"
  @get external screenY: t => int = "screenY"
  @get external shiftKey: t => bool = "shiftKey"
}

module Selection = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
}

module Touch = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external altKey: t => bool = "altKey"
  @get external changedTouches: t => {..} = "changedTouches" /* Should return Dom.touchList */
  @get external ctrlKey: t => bool = "ctrlKey"
  @send
  external getModifierState: (t, string) => bool = "getModifierState"
  @get external metaKey: t => bool = "metaKey"
  @get external shiftKey: t => bool = "shiftKey"
  @get external targetTouches: t => {..} = "targetTouches" /* Should return Dom.touchList */
  @get external touches: t => {..} = "touches" /* Should return Dom.touchList */
}

module UI = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external detail: t => int = "detail"
  @get external view: t => Dom.window = "view" /* Should return DOMAbstractView/WindowProxy */
}

module Wheel = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external deltaMode: t => int = "deltaMode"
  @get external deltaX: t => float = "deltaX"
  @get external deltaY: t => float = "deltaY"
  @get external deltaZ: t => float = "deltaZ"
}

module Media = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
}

module Image = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
}

module Animation = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external animationName: t => string = "animationName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}

module Transition = {
  type tag
  type t = synthetic<tag>
  include MakeEventWithType({
    type t = t
  })
  @get external propertyName: t => string = "propertyName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}
