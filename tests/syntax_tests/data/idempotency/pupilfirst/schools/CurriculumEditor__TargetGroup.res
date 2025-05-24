type t = {
  id: string,
  name: string,
  description: option<string>,
  milestone: bool,
  levelId: string,
  sortIndex: int,
  archived: bool,
}

let id = t => t.id

let name = t => t.name

let description = t => t.description

let milestone = t => t.milestone

let levelId = t => t.levelId

let archived = t => t.archived

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    description: json->field("description", nullable(string))->Js.Null.toOption,
    levelId: json->field("levelId", string),
    milestone: json->field("milestone", bool),
    sortIndex: json->field("sortIndex", int),
    archived: json->field("archived", bool),
  }
}

let create = (id, name, description, milestone, levelId, sortIndex, archived) => {
  id: id,
  name: name,
  description: description,
  milestone: milestone,
  levelId: levelId,
  sortIndex: sortIndex,
  archived: archived,
}

let updateList = (targetGroups, targetGroup) => {
  let oldTargetGroups = targetGroups->List.filter(tg => tg.id !== targetGroup.id)
  oldTargetGroups->List.rev->List.append(list{targetGroup})->List.rev
}

let sort = targetGroups => targetGroups->List.sort((x, y) => x.sortIndex - y.sortIndex)

let unarchive = t => {...t, archived: false}

let find = (id, targetGroups) => targetGroups->List.find(tg => tg.id == id)

let updateSortIndex = sortedTargetGroups =>
  sortedTargetGroups->List.mapi((sortIndex, t) =>
    create(t.id, t.name, t.description, t.milestone, t.levelId, sortIndex, t.archived)
  )
