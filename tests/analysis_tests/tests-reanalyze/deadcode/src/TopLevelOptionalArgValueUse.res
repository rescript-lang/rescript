/* Dead first-class escapes should not suppress optional-arg warnings, but live
   first-class escapes should suppress them even when there is also a direct call. */

let formatDate = (~fmt=?, s) => s
let formatDateEscapes = (~fmt=?, s) => s
let formatDateReturn = (~fmt=?, s) => s
let formatDateTuple = (~fmt=?, s) => s

let takesFn = _ => ()

let deadEscape = () => takesFn(formatDate)

let liveCaller = () => formatDate("2024-01-01")

let liveEscapeCaller = () => {
  takesFn(formatDateEscapes)
  formatDateEscapes("2024-01-01")
}

let liveReturnCaller = () => {
  formatDateReturn
}

let liveTupleCaller = () => {
  (formatDateTuple, "tuple")
}

let _ = liveCaller()
let _ = liveEscapeCaller()
let _ = liveReturnCaller()
let _ = liveTupleCaller()
