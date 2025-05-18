type t = float

module Constants = {
  @val external nan: float = "NaN"
  @val external epsilon: float = "Number.EPSILON"
  @val external positiveInfinity: float = "Number.POSITIVE_INFINITY"
  @val external negativeInfinity: float = "Number.NEGATIVE_INFINITY"
  @val external minValue: float = "Number.MIN_VALUE"
  @val external maxValue: float = "Number.MAX_VALUE"
}

external equal: (float, float) => bool = "%equal"

external compare: (float, float) => Stdlib_Ordering.t = "%compare"

@val external isNaN: float => bool = "isNaN"
@val external isFinite: float => bool = "isFinite"
@val external parseFloat: 'a => float = "parseFloat"
// parseInt's return type is a float because it can be NaN
@val external parseInt: ('a, ~radix: int=?) => float = "parseInt"
@deprecated("Use `parseInt` instead") @val
external parseIntWithRadix: ('a, ~radix: int) => float = "parseInt"

@send external toExponential: (float, ~digits: int=?) => string = "toExponential"
@deprecated("Use `toExponential` instead") @send
external toExponentialWithPrecision: (float, ~digits: int) => string = "toExponential"

@send external toFixed: (float, ~digits: int=?) => string = "toFixed"
@deprecated("Use `toFixed` instead") @send
external toFixedWithPrecision: (float, ~digits: int) => string = "toFixed"

@send external toPrecision: (float, ~digits: int=?) => string = "toPrecision"
@deprecated("Use `toPrecision` instead") @send
external toPrecisionWithPrecision: (float, ~digits: int) => string = "toPrecision"

@send external toString: (float, ~radix: int=?) => string = "toString"
@deprecated("Use `toString` instead") @send
external toStringWithRadix: (float, ~radix: int) => string = "toString"
@send external toLocaleString: float => string = "toLocaleString"

let fromString = i =>
  switch parseFloat(i) {
  | i if isNaN(i) => None
  | i => Some(i)
  }

external toInt: float => int = "%intoffloat"
external fromInt: int => float = "%identity"

external mod: (float, float) => float = "%modfloat"

let clamp = (~min=?, ~max=?, value): float => {
  let value = switch max {
  | Some(max) if max < value => max
  | _ => value
  }
  switch min {
  | Some(min) if min > value => min
  | _ => value
  }
}

external ignore: float => unit = "%ignore"
