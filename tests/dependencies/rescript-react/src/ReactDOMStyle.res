type t = JsxDOMStyle.t

/* CSS2Properties: https://www.w3.org/TR/DOM-Level-2-Style/css.html#CSS-CSS2Properties */
@val
external combine: (@as(json`{}`) _, t, t) => t = "Object.assign"

external _dictToStyle: dict<string> => t = "%identity"

let unsafeAddProp = (style, key, value) => {
  let dict = dict{}
  Dict.set(dict, key, value)
  combine(style, _dictToStyle(dict))
}

@val
external unsafeAddStyle: (@as(json`{}`) _, t, {..}) => t = "Object.assign"
