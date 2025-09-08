type t = {
  id: string,
  endsAt: option<string>,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    endsAt: json->field("endsAt", nullable(string))->Js.Null.toOption,
  }
}

let endsAt = t => t.endsAt
let id = t => t.id

let hasEnded = t =>
  switch t.endsAt {
  | Some(date) => date->DateFns.parseString->DateFns.isPast
  | None => false
  }
