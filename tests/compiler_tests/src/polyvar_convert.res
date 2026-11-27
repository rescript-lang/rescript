type t = [
  | #a
  | #b
]

let revData = %raw(` {"x":"a","b":"b"} `)
let data = %raw(` {"a":"x","b":"b"} `)
@get_index external get: ('a, 'b) => 'c = ""

let tToJs = (x: t): string => get(data, x)

let vFromJsOpt = (s: string): option<t> => get(revData, s)

let vFromJs = (s: string): t => get(revData, s)
