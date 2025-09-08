type t = {
  id: string,
  name: string,
  number: int,
  unlockOn: option<Js.Date.t>,
}

let id = t => t.id

let name = t => t.name

let number = t => t.number

let unlockOn = t => t.unlockOn

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    number: json->field("number", int),
    unlockOn: json->optional(field("unlockOn", string))->OptionUtils.map(DateFns.parseString),
  }
}

let selectLevel = (levels, level_name) => levels->List.find(q => q.name == level_name)

let create = (id, name, number, unlockOn) => {
  id: id,
  name: name,
  number: number,
  unlockOn: unlockOn,
}

let updateList = (levels, level) => {
  let oldLevels = levels->List.filter(l => l.id !== level.id)
  oldLevels->List.rev->List.append(list{level})->List.rev
}

let sort = levels => levels->List.sort((x, y) => x.number - y.number)
