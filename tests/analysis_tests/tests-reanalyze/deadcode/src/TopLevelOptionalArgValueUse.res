/* Dead first-class escapes should not suppress optional-arg warnings, but live
   first-class escapes should suppress them even when there is also a direct call. */

let formatDate = (~fmt=?, s) => s
let formatDateEscapes = (~fmt=?, s) => s
let formatDateReturn = (~fmt=?, s) => s
let formatDateTuple = (~fmt=?, s) => s
let formatDateForwarded = (~fmt=?, s) => s
let formatDateTopLevelEscape = (~fmt=?, s) => s
let formatDateAlias = (~fmt=?, s) => s

let takesFn = _ => ()

takesFn(formatDateTopLevelEscape)

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

let liveForwardingCaller = (~fmt=?) => {
  formatDateForwarded(~fmt?, "2024-01-01")
}

let formatDateAlias2 = formatDateAlias
let liveAliasCaller = () => formatDateAlias2("2024-01-01")

let rec mutuallyRecursiveCaller = () => mutuallyRecursiveTarget(~fmt="ISO", "2024-01-01")
and mutuallyRecursiveTarget = (~fmt=?, s) => s

let returnsFunction = value => (~inner: option<string>=?) => value
let returnedFunction = returnsFunction("value")
let liveReturnedFunctionCaller = () => returnedFunction()

let _ = liveCaller()
let _ = liveEscapeCaller()
let _ = liveReturnCaller()
let _ = liveTupleCaller()
let _ = liveForwardingCaller()
let _ = liveAliasCaller()
let _ = mutuallyRecursiveCaller()
let _ = liveReturnedFunctionCaller()
