exception CannotParseUnknownRole(string)

type role =
  | Student
  | Team

type t = {
  id: string,
  role: role,
  title: string,
  targetGroupId: string,
  sortIndex: int,
  resubmittable: bool,
  prerequisiteTargetIds: list<string>,
  reviewed: bool,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    role: switch json->field("role", string) {
    | "student" => Student
    | "team" => Team
    | unknownRole => raise(CannotParseUnknownRole(unknownRole))
    },
    title: json->field("title", string),
    targetGroupId: json->field("targetGroupId", string),
    sortIndex: json->field("sortIndex", int),
    resubmittable: json->field("resubmittable", bool),
    prerequisiteTargetIds: json->field("prerequisiteTargetIds", list(string)),
    reviewed: json->field("reviewed", bool),
  }
}

let id = t => t.id
let role = t => t.role
let title = t => t.title
let sortIndex = t => t.sortIndex
let targetGroupId = t => t.targetGroupId
let prerequisiteTargetIds = t => t.prerequisiteTargetIds
let resubmittable = t => t.resubmittable
let reviewed = t => t.reviewed
