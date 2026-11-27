@@uncurried

type dateFormatOptions = {someOption?: bool}

@module("SomeModule")
external formatDate: (Date.t, ~options: dateFormatOptions=?, ~done: bool=?) => string = "formatDate"

let x = formatDate(Date.make())
let x = formatDate(Date.make(), ~options={someOption: true})
let x = formatDate(Date.make(), ~done=true)

@send external floatToString: (float, ~radix: int=?) => string = "toString"

let x = floatToString(42.)

@new external regExpFromString: (string, ~flags: string=?) => RegExp.t = "RegExp"

let x = regExpFromString("ab+c")
