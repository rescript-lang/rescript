// This function returns a prefix based on the page state. Now it is a boolean, in the future it may be more complicated.
let getPagePrefix = isExplorer => isExplorer ? "explorer/" : ""

let handleEvent = (action, event) => {
  ReactEvent.Form.preventDefault(event)
  action()
}
let handleMouseEvent = (action, event) => {
  ReactEvent.Mouse.preventDefault(event)
  action()
}

let onlyUpdateValueIfPositiveFloat = (currentValue, updateFunction, value) => {
  let (newValue, didUpdate: bool) = // IF the new number isn't a float, don't update.
  switch Float.fromString(value) {
  | Some(valueFloat) =>
    // IF the new number isn't positive, don't update.
    if valueFloat >= 0. {
      (value, true)
    } else {
      (currentValue, false)
    }
  | None =>
    // If the new value is an empty string let it through.
    if value == "" {
      ("0", true)
    } else {
      (currentValue, false)
    }
  }
  updateFunction(_ => newValue)
  (newValue, didUpdate)
}

let onlyUpdateValueIfInRangeFloat = (min, max, currentValue, updateFunction, value) => {
  let (newValue, didUpdate: bool) = // IF the new number isn't a float, don't update.
  switch Float.fromString(value) {
  | Some(valueFloat) =>
    // IF the new number isn't positive, don't update.
    if valueFloat >= min && valueFloat <= max {
      (value, true)
    } else {
      (currentValue, false)
    }
  | None =>
    // If the new value is empty, set it to the minimum
    if value == "" {
      (min->Js.Float.toString, true)
    } else {
      (currentValue, false)
    }
  }
  updateFunction(_ => newValue)
  (newValue, didUpdate)
}

let onlyUpdateIfPositiveFloat = (currentValue, updateFunction, event) => {
  ReactEvent.Form.preventDefault(event)

  let value = ReactEvent.Form.target(event)["value"]->Option.getWithDefault("")
  onlyUpdateValueIfPositiveFloat(currentValue, updateFunction, value)
}

let onlyUpdateIfInRangeFloat = (min, max, currentValue, updateFunction, event) => {
  ReactEvent.Form.preventDefault(event)

  let value = ReactEvent.Form.target(event)["value"]->Option.getWithDefault("")
  onlyUpdateValueIfInRangeFloat(min, max, currentValue, updateFunction, value)
}
