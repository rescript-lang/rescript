/* This is the whole synthetic event system of ReactJS/ReasonReact. The first module `Synthetic` represents
   the generic synthetic event. The rest are the specific ones.

   In each module, the type `t` commonly means "the type of that module" (OCaml convention). In our case, e.g.
   `ReactEvent.Mouse.t` represents a ReactJS synthetic mouse event. You'd use it to type your props:

   ```
   type props = {
     onClick: ReactEvent.Mouse.t => unit
   };
   ```

   All the methods and properties of a type of event are in the module, as seen below.

   Each module also has a `tag` type. You can ignore it; they're only needed by their `t` type. This way, we
   get to allow a base `Synthetic` event module with generic methods. So e.g. even a mouse event (`Mouse.t`)
   get to be passed to a generic handler:

   ```
   let handleClick = ({state, props}, event) => {
     ReactEvent.Mouse.preventDefault(event);
     ...
   };
   let handleSubmit = ({state, props}, event) => {
     /* this handler can be triggered by either a Keyboard or a Mouse event; conveniently use the generic
        preventDefault */
     ReactEvent.Synthetic.preventDefault(event);
     ...
   };

   let render = (_) => <Foo onSubmit=handleSubmit onEnter=handleSubmit .../>;
   ```

   How to translate idioms from ReactJS:

   1. myMouseEvent.preventDefault() -> ReactEvent.Mouse.preventDefault(myMouseEvent)
   2. myKeyboardEvent.which -> ReactEvent.Keyboard.which(myKeyboardEvent)
 */
type synthetic<'a>

module Synthetic: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: synthetic<'a> => bool = "bubbles"
  @get external cancelable: synthetic<'a> => bool = "cancelable"
  @get
  external currentTarget: synthetic<'a> => {..} = "currentTarget"
  @get
  external defaultPrevented: synthetic<'a> => bool = "defaultPrevented"
  @get external eventPhase: synthetic<'a> => int = "eventPhase"
  @get external isTrusted: synthetic<'a> => bool = "isTrusted"
  @get
  external nativeEvent: synthetic<'a> => {..} = "nativeEvent"
  @send
  external preventDefault: synthetic<'a> => unit = "preventDefault"
  @send
  external isDefaultPrevented: synthetic<'a> => bool = "isDefaultPrevented"
  @send
  external stopPropagation: synthetic<'a> => unit = "stopPropagation"
  @send
  external isPropagationStopped: synthetic<'a> => bool = "isPropagationStopped"
  @get external target: synthetic<'a> => {..} = "target"
  @get external timeStamp: synthetic<'a> => float = "timeStamp"
  @get external type_: synthetic<'a> => string = "type"
  @send external persist: synthetic<'a> => unit = "persist"
}

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
external toSyntheticEvent: synthetic<'a> => Synthetic.t = "%identity"

module Clipboard: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external clipboardData: t => {..} = "clipboardData" /* Should return Dom.dataTransfer */
}

module Composition: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external data: t => string = "data"
}

module Keyboard: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
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

module Focus: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get
  external relatedTarget: t => Js.nullable<{..}> =
    "relatedTarget" /* Should return Dom.eventTarget */
}

module Form: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
}

module Mouse: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
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

module Selection: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
}

module Touch: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
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

module UI: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external detail: t => int = "detail"
  @get external view: t => Dom.window = "view" /* Should return DOMAbstractView/WindowProxy */
}

module Wheel: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external deltaMode: t => int = "deltaMode"
  @get external deltaX: t => float = "deltaX"
  @get external deltaY: t => float = "deltaY"
  @get external deltaZ: t => float = "deltaZ"
}

module Media: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
}

module Image: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
}

module Animation: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external animationName: t => string = "animationName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}

module Transition: {
  type tag
  type t = synthetic<tag>
  @get external bubbles: t => bool = "bubbles"
  @get external cancelable: t => bool = "cancelable"
  @get external currentTarget: t => {..} = "currentTarget"
  @get external defaultPrevented: t => bool = "defaultPrevented"
  @get external eventPhase: t => int = "eventPhase"
  @get external isTrusted: t => bool = "isTrusted"
  @get external nativeEvent: t => {..} = "nativeEvent"
  @send external preventDefault: t => unit = "preventDefault"
  @send external isDefaultPrevented: t => bool = "isDefaultPrevented"
  @send external stopPropagation: t => unit = "stopPropagation"
  @send external isPropagationStopped: t => bool = "isPropagationStopped"
  @get external target: t => {..} = "target"
  @get external timeStamp: t => float = "timeStamp"
  @get external type_: t => string = "type"
  @send external persist: t => unit = "persist"
  @get external propertyName: t => string = "propertyName"
  @get external pseudoElement: t => string = "pseudoElement"
  @get external elapsedTime: t => float = "elapsedTime"
}
