exception InvalidVisibilityValue(string)

type visibility =
  | Draft
  | Live
  | Archived

type t = {
  id: string,
  targetGroupId: string,
  title: string,
  sortIndex: int,
  visibility: visibility,
}

let id = t => t.id

let title = t => t.title

let targetGroupId = t => t.targetGroupId

let sortIndex = t => t.sortIndex

let visibility = t => t.visibility

let decodeVisbility = visibilityString =>
  switch visibilityString {
  | "draft" => Draft
  | "live" => Live
  | "archived" => Archived
  | _ => raise(InvalidVisibilityValue("Unknown Value"))
  }

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    targetGroupId: json->field("targetGroupId", string),
    title: json->field("title", string),
    sortIndex: json->field("sortIndex", int),
    visibility: decodeVisbility(json->field("visibility", string)),
  }
}

let updateList = (targets, target) => {
  let oldTargets = targets->List.filter(t => t.id !== target.id)
  oldTargets->List.rev->List.append(list{target})->List.rev
}

let create = (~id, ~targetGroupId, ~title, ~sortIndex, ~visibility) => {
  id: id,
  targetGroupId: targetGroupId,
  title: title,
  sortIndex: sortIndex,
  visibility: visibility,
}

let sort = targets => targets->List.sort((x, y) => x.sortIndex - y.sortIndex)

let archive = t => {...t, visibility: Archived}

let archived = t =>
  switch t.visibility {
  | Archived => true
  | Live => false
  | Draft => false
  }

let removeTarget = (target, targets) => targets->List.filter(t => t.id != target.id)

let targetIdsInTargetGroup = (id, targets) =>
  targets->List.filter(t => t.targetGroupId == id)->List.map(t => t.id)

let updateSortIndex = sortedTargets =>
  sortedTargets->List.mapi((sortIndex, t) =>
    create(
      ~id=t.id,
      ~targetGroupId=t.targetGroupId,
      ~title=t.title,
      ~sortIndex,
      ~visibility=t.visibility,
    )
  )

let template = (id, targetGroupId, title) =>
  create(~id, ~targetGroupId, ~title, ~sortIndex=999, ~visibility=Draft)
