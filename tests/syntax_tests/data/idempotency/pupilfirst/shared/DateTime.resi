type t = Js.Date.t

type format =
  | OnlyDate
  | DateWithYearAndTime

let parse: string => t

let format: (format, t) => string

let stingToFormatedTime: (format, string) => string

let randomId: unit => string

let decode: Js.Json.t => t
