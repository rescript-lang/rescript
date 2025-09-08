@@uncurried

type dateFormatOptions = {someOption?: bool}

@module("SomeModule")
external formatDate: (Js.Date.t, ~options: dateFormatOptions=?, ~done: bool=?) => string =
  "formatDate"

let x = formatDate(Js.Date.make())
let x = formatDate(Js.Date.make(), ~options={someOption: true})
let x = formatDate(Js.Date.make(), ~done=true)

@send external floatToString: (float, ~radix: int=?) => string = "toString"

let x = floatToString(42.)

@new external regExpFromString: (string, ~flags: string=?) => Js.Re.t = "RegExp"

let x = regExpFromString("ab+c")
