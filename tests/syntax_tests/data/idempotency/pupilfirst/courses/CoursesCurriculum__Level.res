type t = {
  id: string,
  name: string,
  number: int,
  unlockOn: option<string>,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    number: json->field("number", int),
    unlockOn: json->field("unlockOn", nullable(string))->Js.Null.toOption,
  }
}

let id = t => t.id
let name = t => t.name
let number = t => t.number
let unlockOn = t => t.unlockOn

let isUnlocked = t =>
  switch t.unlockOn {
  | Some(date) => date->DateFns.parseString->DateFns.isPast
  | None => true
  }

let isLocked = t => !(t->isUnlocked)

let sort = levels => levels->List.sort((x, y) => x.number - y.number)

let first = levels =>
  switch levels->sort {
  | list{} =>
    Rollbar.error("Failed to find the first level from a course's levels.")
    throw(Not_found)
  | list{firstLevel, ..._rest} => firstLevel
  }

let unlockDateString = t =>
  switch t.unlockOn {
  | None =>
    Rollbar.error("unlockDateString was called for a CoursesCurriculum__Level without unlockOn")
    ""
  | Some(unlockOn) => unlockOn->DateFns.parseString->DateFns.format("MMM D")
  }

let findByLevelNumber = (levels, levelNumber) =>
  levels->List.find_opt(l => l.number == levelNumber)

let next = (levels, t) => t.number + 1->findByLevelNumber(levels)

let previous = (levels, t) => {
  let previousLevelNumber = t.number - 1

  if previousLevelNumber == 0 {
    None
  } else {
    previousLevelNumber->findByLevelNumber(levels)
  }
}
