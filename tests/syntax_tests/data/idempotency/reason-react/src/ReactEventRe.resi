/* Old code. See ReactEvent.re for documentation. */
@deprecated("Please use ReactEvent.synthetic")
type synthetic<'a> = ReactEvent.synthetic<'a>

module Synthetic: {
  @deprecated("Please use ReactEvent.Synthetic.tag")
  type tag = ReactEvent.Synthetic.tag
  @deprecated("Please use ReactEvent.Synthetic.t")
  type t = ReactEvent.Synthetic.t
  @deprecated("Please use ReactEvent.Synthetic.bubbles") @get
  external bubbles: ReactEvent.synthetic<'a> => bool = "bubbles"
  @deprecated("Please use ReactEvent.Synthetic.cancelable") @get
  external cancelable: ReactEvent.synthetic<'a> => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Synthetic.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.synthetic<'a> => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Synthetic.defaultPrevented") @get
  external defaultPrevented: ReactEvent.synthetic<'a> => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Synthetic.eventPhase") @get
  external eventPhase: ReactEvent.synthetic<'a> => int = "eventPhase"
  @deprecated("Please use ReactEvent.Synthetic.isTrusted") @get
  external isTrusted: ReactEvent.synthetic<'a> => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Synthetic.nativeEvent") @get
  external nativeEvent: ReactEvent.synthetic<'a> => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Synthetic.preventDefault")
  @get
  external target: ReactEvent.synthetic<'a> => Dom.element = "target"
  @deprecated("Please use ReactEvent.Synthetic.timeStamp") @get
  external timeStamp: ReactEvent.synthetic<'a> => float = "timeStamp"
  @deprecated("Please use ReactEvent.Synthetic.type_") @get
  external _type: ReactEvent.synthetic<'a> => string = "type"
}

/* Cast any event type to the general synthetic type. This is safe, since synthetic is more general */
@deprecated("Please use ReactEvent.toSyntheticEvent")
external toSyntheticEvent: ReactEvent.synthetic<'a> => ReactEvent.Synthetic.t = "%identity"

module Clipboard: {
  @deprecated("Please use ReactEvent.Clipboard.tag")
  type tag = ReactEvent.Clipboard.tag
  @deprecated("Please use ReactEvent.Clipboard.tag")
  type t = ReactEvent.Clipboard.t
  @deprecated("Please use ReactEvent.Clipboard.bubbles") @get
  external bubbles: ReactEvent.Clipboard.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Clipboard.cancelable") @get
  external cancelable: ReactEvent.Clipboard.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Clipboard.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Clipboard.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Clipboard.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Clipboard.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Clipboard.eventPhase") @get
  external eventPhase: ReactEvent.Clipboard.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Clipboard.isTrusted") @get
  external isTrusted: ReactEvent.Clipboard.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Clipboard.nativeEvent") @get
  external nativeEvent: ReactEvent.Clipboard.t => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Clipboard.preventDefault")
  @deprecated(
    "Please use ReactEvent.Clipboard.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Clipboard.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Clipboard.timeStamp") @get
  external timeStamp: ReactEvent.Clipboard.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Clipboard.type_") @get
  external _type: ReactEvent.Clipboard.t => string = "type"
  @deprecated("Please use myEvent->ReactEvent.Clipboard.persist")
  @deprecated("Please use ReactEvent.Clipboard.clipboardData") @get
  external clipboardData: ReactEvent.Clipboard.t => {..} =
    "clipboardData" /* Should return Dom.dataTransfer */
}

module Composition: {
  @deprecated("Please use ReactEvent.Composition.tag")
  type tag = ReactEvent.Composition.tag
  @deprecated("Please use ReactEvent.Composition.t")
  type t = ReactEvent.Composition.t
  @deprecated("Please use ReactEvent.Composition.bubbles") @get
  external bubbles: ReactEvent.Composition.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Composition.cancelable") @get
  external cancelable: ReactEvent.Composition.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Composition.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Composition.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Composition.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Composition.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Composition.eventPhase") @get
  external eventPhase: ReactEvent.Composition.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Composition.isTrusted") @get
  external isTrusted: ReactEvent.Composition.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Composition.nativeEvent") @get
  external nativeEvent: ReactEvent.Composition.t => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Composition.preventDefault")
  @get
  external target: ReactEvent.Composition.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Composition.timeStamp") @get
  external timeStamp: ReactEvent.Composition.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Composition.type_") @get
  external _type: ReactEvent.Composition.t => string = "type"
  @deprecated("Please use myEvent->ReactEvent.Composition.persist")
  @deprecated("Please use ReactEvent.Composition.data") @get
  external data: ReactEvent.Composition.t => string = "data"
}

module Keyboard: {
  @deprecated("Please use ReactEvent.Keyboard.tag")
  type tag = ReactEvent.Keyboard.tag
  @deprecated("Please use ReactEvent.Keyboard.t")
  type t = ReactEvent.Keyboard.t
  @deprecated("Please use ReactEvent.Keyboard.bubbles") @get
  external bubbles: ReactEvent.Keyboard.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Keyboard.cancelable") @get
  external cancelable: ReactEvent.Keyboard.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Keyboard.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Keyboard.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Keyboard.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Keyboard.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Keyboard.eventPhase") @get
  external eventPhase: ReactEvent.Keyboard.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Keyboard.isTrusted") @get
  external isTrusted: ReactEvent.Keyboard.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Keyboard.nativeEvent") @get
  external nativeEvent: ReactEvent.Keyboard.t => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Keyboard.isDefaultPrevented")
  @deprecated(
    "Please use ReactEvent.Keyboard.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Keyboard.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Keyboard.timeStamp") @get
  external timeStamp: ReactEvent.Keyboard.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Keyboard.type_") @get
  external _type: ReactEvent.Keyboard.t => string = "type"
  @deprecated("Please use ReactEvent.Keyboard.altKey") @get
  external altKey: ReactEvent.Keyboard.t => bool = "altKey"
  @deprecated("Please use ReactEvent.Keyboard.charCode") @get
  external charCode: ReactEvent.Keyboard.t => int = "charCode"
  @deprecated("Please use ReactEvent.Keyboard.ctrlKey") @get
  external ctrlKey: ReactEvent.Keyboard.t => bool = "ctrlKey"
  @deprecated("Please use ReactEvent.Keyboard.key") @get
  external key: ReactEvent.Keyboard.t => string = "key"
  @deprecated("Please use ReactEvent.Keyboard.keyCode") @get
  external keyCode: ReactEvent.Keyboard.t => int = "keyCode"
  @deprecated("Please use ReactEvent.Keyboard.locale") @get
  external locale: ReactEvent.Keyboard.t => string = "locale"
  @deprecated("Please use ReactEvent.Keyboard.location") @get
  external location: ReactEvent.Keyboard.t => int = "location"
  @deprecated("Please use ReactEvent.Keyboard.metaKey") @get
  external metaKey: ReactEvent.Keyboard.t => bool = "metaKey"
  @deprecated("Please use ReactEvent.Keyboard.repeat") @get
  external repeat: ReactEvent.Keyboard.t => bool = "repeat"
  @deprecated("Please use ReactEvent.Keyboard.shiftKey") @get
  external shiftKey: ReactEvent.Keyboard.t => bool = "shiftKey"
  @deprecated("Please use ReactEvent.Keyboard.which") @get
  external which: ReactEvent.Keyboard.t => int = "which"
}

module Focus: {
  @deprecated("Please use ReactEvent.Focus.tag")
  type tag = ReactEvent.Focus.tag
  @deprecated("Please use ReactEvent.Focus.t")
  type t = ReactEvent.Focus.t
  @deprecated("Please use ReactEvent.Focus.bubbles") @get
  external bubbles: ReactEvent.Focus.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Focus.cancelable") @get
  external cancelable: ReactEvent.Focus.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Focus.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Focus.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Focus.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Focus.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Focus.eventPhase") @get
  external eventPhase: ReactEvent.Focus.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Focus.isTrusted") @get
  external isTrusted: ReactEvent.Focus.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Focus.nativeEvent") @get
  external nativeEvent: ReactEvent.Focus.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Focus.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Focus.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Focus.timeStamp") @get
  external timeStamp: ReactEvent.Focus.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Focus.type_") @get
  external _type: ReactEvent.Focus.t => string = "type"
  @deprecated(
    "Please use ReactEvent.Focus.relatedTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external relatedTarget: ReactEvent.Focus.t => Dom.element =
    "relatedTarget" /* Should return Dom.eventTarget */
}

module Form: {
  @deprecated("Please use ReactEvent.Form.tag")
  type tag = ReactEvent.Form.tag
  @deprecated("Please use ReactEvent.Form.t")
  type t = ReactEvent.Form.t
  @deprecated("Please use ReactEvent.Form.bubbles") @get
  external bubbles: ReactEvent.Form.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Form.cancelable") @get
  external cancelable: ReactEvent.Form.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Form.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Form.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Form.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Form.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Form.eventPhase") @get
  external eventPhase: ReactEvent.Form.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Form.isTrusted") @get
  external isTrusted: ReactEvent.Form.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Form.nativeEvent") @get
  external nativeEvent: ReactEvent.Form.t => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Form.preventDefault")
  @deprecated(
    "Please use ReactEvent.Form.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Form.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Form.timeStamp") @get
  external timeStamp: ReactEvent.Form.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Form.type_") @get
  external _type: ReactEvent.Form.t => string = "type"
}

module Mouse: {
  @deprecated("Please use ReactEvent.Mouse.tag")
  type tag = ReactEvent.Mouse.tag
  @deprecated("Please use ReactEvent.Mouse.t")
  type t = ReactEvent.Mouse.t
  @deprecated("Please use ReactEvent.Mouse.bubbles") @get
  external bubbles: ReactEvent.Mouse.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Mouse.cancelable") @get
  external cancelable: ReactEvent.Mouse.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Mouse.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Mouse.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Mouse.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Mouse.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Mouse.eventPhase") @get
  external eventPhase: ReactEvent.Mouse.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Mouse.isTrusted") @get
  external isTrusted: ReactEvent.Mouse.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Mouse.nativeEvent") @get
  external nativeEvent: ReactEvent.Mouse.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Mouse.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Mouse.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Mouse.timeStamp") @get
  external timeStamp: ReactEvent.Mouse.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Mouse.type_") @get
  external _type: ReactEvent.Mouse.t => string = "type"
  @deprecated("Please use ReactEvent.Mouse.altKey") @get
  external altKey: ReactEvent.Mouse.t => bool = "altKey"
  @deprecated("Please use ReactEvent.Mouse.button") @get
  external button: ReactEvent.Mouse.t => int = "button"
  @deprecated("Please use ReactEvent.Mouse.buttons") @get
  external buttons: ReactEvent.Mouse.t => int = "buttons"
  @deprecated("Please use ReactEvent.Mouse.clientX") @get
  external clientX: ReactEvent.Mouse.t => int = "clientX"
  @deprecated("Please use ReactEvent.Mouse.clientY") @get
  external clientY: ReactEvent.Mouse.t => int = "clientY"
  @deprecated("Please use ReactEvent.Mouse.ctrlKey") @get
  external ctrlKey: ReactEvent.Mouse.t => bool = "ctrlKey"
  @deprecated("Please use ReactEvent.Mouse.metaKey") @get
  external metaKey: ReactEvent.Mouse.t => bool = "metaKey"
  @deprecated("Please use ReactEvent.Mouse.pageX") @get
  external pageX: ReactEvent.Mouse.t => int = "pageX"
  @deprecated("Please use ReactEvent.Mouse.pageY") @get
  external pageY: ReactEvent.Mouse.t => int = "pageY"
  @deprecated(
    "Please use ReactEvent.Mouse.relatedTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external relatedTarget: ReactEvent.Mouse.t => Dom.element =
    "relatedTarget" /* Should return Dom.eventTarget */
  @deprecated("Please use ReactEvent.Mouse.screenX") @get
  external screenX: ReactEvent.Mouse.t => int = "screenX"
  @deprecated("Please use ReactEvent.Mouse.screenY") @get
  external screenY: ReactEvent.Mouse.t => int = "screenY"
  @deprecated("Please use ReactEvent.Mouse.shiftKey") @get
  external shiftKey: ReactEvent.Mouse.t => bool = "shiftKey"
}

module Selection: {
  @deprecated("Please use ReactEvent.Selection.tag")
  type tag = ReactEvent.Selection.tag
  @deprecated("Please use ReactEvent.Selection.t")
  type t = ReactEvent.Selection.t
  @deprecated("Please use ReactEvent.Selection.bubbles") @get
  external bubbles: ReactEvent.Selection.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Selection.cancelable") @get
  external cancelable: ReactEvent.Selection.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Selection.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Selection.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Selection.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Selection.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Selection.eventPhase") @get
  external eventPhase: ReactEvent.Selection.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Selection.isTrusted") @get
  external isTrusted: ReactEvent.Selection.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Selection.nativeEvent") @get
  external nativeEvent: ReactEvent.Selection.t => {..} = "nativeEvent"
  external isPropagationStopped: bool = "isPropagationStopped"
  @deprecated(
    "Please use ReactEvent.Selection.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Selection.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Selection.timeStamp") @get
  external timeStamp: ReactEvent.Selection.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Selection.type_") @get
  external _type: ReactEvent.Selection.t => string = "type"
}

module Touch: {
  @deprecated("Please use ReactEvent.Touch.tag")
  type tag = ReactEvent.Touch.tag
  @deprecated("Please use ReactEvent.Touch.t")
  type t = ReactEvent.Touch.t
  @deprecated("Please use ReactEvent.Touch.bubbles") @get
  external bubbles: ReactEvent.Touch.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Touch.cancelable") @get
  external cancelable: ReactEvent.Touch.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Touch.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Touch.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Touch.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Touch.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Touch.eventPhase") @get
  external eventPhase: ReactEvent.Touch.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Touch.isTrusted") @get
  external isTrusted: ReactEvent.Touch.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Touch.nativeEvent") @get
  external nativeEvent: ReactEvent.Touch.t => {..} = "nativeEvent"
  @deprecated("Please use myEvent->ReactEvent.Touch.preventDefault")
  @deprecated(
    "Please use ReactEvent.Touch.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Touch.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Touch.timeStamp") @get
  external timeStamp: ReactEvent.Touch.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Touch.type_") @get
  external _type: ReactEvent.Touch.t => string = "type"
  @deprecated("Please use ReactEvent.Touch.altKey") @get
  external altKey: ReactEvent.Touch.t => bool = "altKey"
  @deprecated("Please use ReactEvent.Touch.changedTouches") @get
  external changedTouches: ReactEvent.Touch.t => {..} =
    "changedTouches" /* Should return Dom.touchList */
  @deprecated("Please use ReactEvent.Touch.ctrlKey") @get
  external ctrlKey: ReactEvent.Touch.t => bool = "ctrlKey"
  @deprecated("Please use ReactEvent.Touch.metaKey") @get
  external metaKey: ReactEvent.Touch.t => bool = "metaKey"
  @deprecated("Please use ReactEvent.Touch.shiftKey") @get
  external shiftKey: ReactEvent.Touch.t => bool = "shiftKey"
  @deprecated("Please use ReactEvent.Touch.targetTouches") @get
  external targetTouches: ReactEvent.Touch.t => {..} =
    "targetTouches" /* Should return Dom.touchList */
  @deprecated("Please use ReactEvent.Touch.touches") @get
  external touches: ReactEvent.Touch.t => {..} = "touches" /* Should return Dom.touchList */
}

module UI: {
  @deprecated("Please use ReactEvent.UI.tag")
  type tag = ReactEvent.UI.tag
  @deprecated("Please use ReactEvent.UI.t")
  type t = ReactEvent.UI.t
  @deprecated("Please use ReactEvent.UI.bubbles") @get
  external bubbles: ReactEvent.UI.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.UI.cancelable") @get
  external cancelable: ReactEvent.UI.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.UI.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.UI.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.UI.defaultPrevented") @get
  external defaultPrevented: ReactEvent.UI.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.UI.eventPhase") @get
  external eventPhase: ReactEvent.UI.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.UI.isTrusted") @get
  external isTrusted: ReactEvent.UI.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.UI.nativeEvent") @get
  external nativeEvent: ReactEvent.UI.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.UI.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.UI.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.UI.timeStamp") @get
  external timeStamp: ReactEvent.UI.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.UI.type_") @get
  external _type: ReactEvent.UI.t => string = "type"
  @deprecated("Please use ReactEvent.UI.detail") @get
  external detail: ReactEvent.UI.t => int = "detail"
  @deprecated("Please use ReactEvent.UI.view") @get
  external view: ReactEvent.UI.t => Dom.window =
    "view" /* Should return DOMAbstractView/WindowProxy */
}

module Wheel: {
  @deprecated("Please use ReactEvent.Wheel.tag")
  type tag = ReactEvent.Wheel.tag
  @deprecated("Please use ReactEvent.Wheel.t")
  type t = ReactEvent.Wheel.t
  @deprecated("Please use ReactEvent.Wheel.bubbles") @get
  external bubbles: ReactEvent.Wheel.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Wheel.cancelable") @get
  external cancelable: ReactEvent.Wheel.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Wheel.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Wheel.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Wheel.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Wheel.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Wheel.eventPhase") @get
  external eventPhase: ReactEvent.Wheel.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Wheel.isTrusted") @get
  external isTrusted: ReactEvent.Wheel.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Wheel.nativeEvent") @get
  external nativeEvent: ReactEvent.Wheel.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Wheel.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Wheel.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Wheel.timeStamp") @get
  external timeStamp: ReactEvent.Wheel.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Wheel.type_") @get
  external _type: ReactEvent.Wheel.t => string = "type"
  @deprecated("Please use ReactEvent.Wheel.deltaMode") @get
  external deltaMode: ReactEvent.Wheel.t => int = "deltaMode"
  @deprecated("Please use ReactEvent.Wheel.deltaX") @get
  external deltaX: ReactEvent.Wheel.t => float = "deltaX"
  @deprecated("Please use ReactEvent.Wheel.deltaY") @get
  external deltaY: ReactEvent.Wheel.t => float = "deltaY"
  @deprecated("Please use ReactEvent.Wheel.deltaZ") @get
  external deltaZ: ReactEvent.Wheel.t => float = "deltaZ"
}

module Media: {
  @deprecated("Please use ReactEvent.Media.tag")
  type tag = ReactEvent.Media.tag
  @deprecated("Please use ReactEvent.Media.t")
  type t = ReactEvent.Media.t
  @deprecated("Please use ReactEvent.Media.bubbles") @get
  external bubbles: ReactEvent.Media.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Media.cancelable") @get
  external cancelable: ReactEvent.Media.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Media.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Media.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Media.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Media.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Media.eventPhase") @get
  external eventPhase: ReactEvent.Media.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Media.isTrusted") @get
  external isTrusted: ReactEvent.Media.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Media.nativeEvent") @get
  external nativeEvent: ReactEvent.Media.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Media.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Media.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Media.timeStamp") @get
  external timeStamp: ReactEvent.Media.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Media.type_") @get
  external _type: ReactEvent.Media.t => string = "type"
}

module Image: {
  @deprecated("Please use ReactEvent.Image.tag")
  type tag = ReactEvent.Image.tag
  @deprecated("Please use ReactEvent.Image.t")
  type t = ReactEvent.Image.t
  @deprecated("Please use ReactEvent.Image.bubbles") @get
  external bubbles: ReactEvent.Image.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Image.cancelable") @get
  external cancelable: ReactEvent.Image.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Image.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Image.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Image.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Image.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Image.eventPhase") @get
  external eventPhase: ReactEvent.Image.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Image.isTrusted") @get
  external isTrusted: ReactEvent.Image.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Image.nativeEvent") @get
  external nativeEvent: ReactEvent.Image.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Image.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Image.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Image.timeStamp") @get
  external timeStamp: ReactEvent.Image.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Image.type_") @get
  external _type: ReactEvent.Image.t => string = "type"
}

module Animation: {
  @deprecated("Please use ReactEvent.Animation.tag")
  type tag = ReactEvent.Animation.tag
  @deprecated("Please use ReactEvent.Animation.t")
  type t = ReactEvent.Animation.t
  @deprecated("Please use ReactEvent.Animation.bubbles") @get
  external bubbles: ReactEvent.Animation.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Animation.cancelable") @get
  external cancelable: ReactEvent.Animation.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Animation.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Animation.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Animation.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Animation.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Animation.eventPhase") @get
  external eventPhase: ReactEvent.Animation.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Animation.isTrusted") @get
  external isTrusted: ReactEvent.Animation.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Animation.nativeEvent") @get
  external nativeEvent: ReactEvent.Animation.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Animation.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Animation.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Animation.timeStamp") @get
  external timeStamp: ReactEvent.Animation.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Animation.type_") @get
  external _type: ReactEvent.Animation.t => string = "type"
  @deprecated("Please use ReactEvent.Animation.animationName") @get
  external animationName: ReactEvent.Animation.t => string = "animationName"
  @deprecated("Please use ReactEvent.Animation.pseudoElement") @get
  external pseudoElement: ReactEvent.Animation.t => string = "pseudoElement"
  @deprecated("Please use ReactEvent.Animation.elapsedTime") @get
  external elapsedTime: ReactEvent.Animation.t => float = "elapsedTime"
}

module Transition: {
  @deprecated("Please use ReactEvent.Transition.tag")
  type tag = ReactEvent.Transition.tag
  @deprecated("Please use ReactEvent.Transition.t")
  type t = ReactEvent.Transition.t
  @deprecated("Please use ReactEvent.Transition.bubbles") @get
  external bubbles: ReactEvent.Transition.t => bool = "bubbles"
  @deprecated("Please use ReactEvent.Transition.cancelable") @get
  external cancelable: ReactEvent.Transition.t => bool = "cancelable"
  @deprecated(
    "Please use ReactEvent.Transition.currentTarget and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external currentTarget: ReactEvent.Transition.t => Dom.element = "currentTarget"
  @deprecated("Please use ReactEvent.Transition.defaultPrevented") @get
  external defaultPrevented: ReactEvent.Transition.t => bool = "defaultPrevented"
  @deprecated("Please use ReactEvent.Transition.eventPhase") @get
  external eventPhase: ReactEvent.Transition.t => int = "eventPhase"
  @deprecated("Please use ReactEvent.Transition.isTrusted") @get
  external isTrusted: ReactEvent.Transition.t => bool = "isTrusted"
  @deprecated("Please use ReactEvent.Transition.nativeEvent") @get
  external nativeEvent: ReactEvent.Transition.t => {..} = "nativeEvent"
  @deprecated(
    "Please use ReactEvent.Transition.target and remove the surrounding ReactDOMRe.domElementToObj wrapper if any (no longer needed)"
  )
  @get
  external target: ReactEvent.Transition.t => Dom.element = "target"
  @deprecated("Please use ReactEvent.Transition.timeStamp") @get
  external timeStamp: ReactEvent.Transition.t => float = "timeStamp"
  @deprecated("Please use ReactEvent.Transition.type_") @get
  external _type: ReactEvent.Transition.t => string = "type"
  external persist: unit = "persist"
  @deprecated("Please use ReactEvent.Transition.propertyName") @get
  external propertyName: ReactEvent.Transition.t => string = "propertyName"
  @deprecated("Please use ReactEvent.Transition.pseudoElement") @get
  external pseudoElement: ReactEvent.Transition.t => string = "pseudoElement"
  @deprecated("Please use ReactEvent.Transition.elapsedTime") @get
  external elapsedTime: ReactEvent.Transition.t => float = "elapsedTime"
}
