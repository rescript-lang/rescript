type t = {
  id: string,
  title: string,
  createdAt: Js.Date.t,
  answersCount: int,
}

let id = t => t.id
let title = t => t.title
let createdAt = t => t.createdAt
let answersCount = t => t.answersCount

let makeFromJs = jsObject => {
  id: jsObject["id"],
  title: jsObject["title"],
  createdAt: jsObject["createdAt"]->Json.Decode.string->DateFns.parseString,
  answersCount: jsObject["answersCount"],
}
