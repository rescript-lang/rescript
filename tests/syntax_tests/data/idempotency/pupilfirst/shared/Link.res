let ctrlKey = Webapi.Dom.KeyboardEvent.ctrlKey
let metaKey = Webapi.Dom.KeyboardEvent.metaKey

external unsafeAsKeyboardEvent: ReactEvent.Mouse.t => Webapi.Dom.KeyboardEvent.t = "%identity"

let onConfirm = (href, onClick, event) => {
  event->ReactEvent.Mouse.preventDefault
  ReasonReactRouter.push(href)
  onClick->OptionUtils.mapWithDefault(onClick => onClick(event), ())
}

let onCancel = event => event->ReactEvent.Mouse.preventDefault

let handleOnClick = (href, confirm, onClick, event) => {
  let keyboardEvent = event->unsafeAsKeyboardEvent
  let modifierPressed = keyboardEvent->ctrlKey || keyboardEvent->metaKey

  switch (modifierPressed, confirm) {
  | (true, _) => ()
  | (false, Some(confirmationText)) =>
    WindowUtils.confirm(
      ~onCancel=() => onCancel(event),
      confirmationText,
      () => onConfirm(href, onClick, event),
    )
  | (false, None) => onConfirm(href, onClick, event)
  }
}

@react.component
let make = (
  ~href,
  ~ariaLabel=?,
  ~className=?,
  ~confirm=?,
  ~id=?,
  ~onClick=?,
  ~title=?,
  ~children,
) =>
  <a href ?ariaLabel ?className ?id ?title onClick={handleOnClick(href, confirm, onClick)}>
    children
  </a>
