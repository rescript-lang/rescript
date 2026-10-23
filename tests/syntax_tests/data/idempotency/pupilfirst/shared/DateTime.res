type t = Date.t

@val @module("date-fns")
external dateFormat: (t, string) => string = "format"

@val @module("date-fns") external dateParse: string => t = "parse"

let parse = s => s->dateParse

type format =
  | OnlyDate
  | DateWithYearAndTime

let format = (f, t) => {
  let formatString = switch f {
  | OnlyDate => "MMM D, YYYY"
  | DateWithYearAndTime => "Do MMM YYYY HH:mm"
  }
  dateFormat(t, formatString)
}

let stingToFormatedTime = (f, t) => format(f, parse(t))

let randomId = () => {
  let number = Math.random()->Float.toString
  let time = Date.now()->Float.toString
  "I" ++ (time ++ number)->String.replace(".", "-")
}

let decode = json => json->Json.Decode.string->DateFns.parseString
