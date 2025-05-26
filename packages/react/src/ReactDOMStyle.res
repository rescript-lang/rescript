type t = JsxDOMStyle.t

/* CSS2Properties: https://www.w3.org/TR/DOM-Level-2-Style/css.html#CSS-CSS2Properties */
@val
external combine: (@as(json`{}`) _, t, t) => t = "Object.assign"

external _dictToStyle: Js.Dict.t<string> => t = "%identity"

let unsafeAddProp = (style, key, value) => {
  let dict = Js.Dict.empty()
  Js.Dict.set(dict, key, value)
  combine(style, _dictToStyle(dict))
}

@val
external unsafeAddStyle: (@as(json`{}`) _, t, {..}) => t = "Object.assign"
