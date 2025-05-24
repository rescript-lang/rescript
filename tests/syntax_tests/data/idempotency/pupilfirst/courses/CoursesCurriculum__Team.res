type t = {
  @live
  name: string,
  levelId: string,
  accessEndsAt: option<string>,
}

let decode = json => {
  open Json.Decode
  {
    name: json->field("name", string),
    levelId: json->field("levelId", string),
    accessEndsAt: json->field("accessEndsAt", nullable(string))->Js.Null.toOption,
  }
}

let levelId = t => t.levelId
let accessEndsAt = t => t.accessEndsAt

let accessEnded = t =>
  switch t.accessEndsAt {
  | Some(date) => date->DateFns.parseString->DateFns.isPast
  | None => false
  }
