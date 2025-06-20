exception UnexpectedStatusValue(string)

type status =
  | MarkedAsComplete
  | Pending
  | Passed
  | Failed

type t = {
  id: string,
  createdAt: string,
  status: status,
  checklist: array<SubmissionChecklistItem.t>,
}

let id = t => t.id
let createdAt = t => t.createdAt
let status = t => t.status
let checklist = t => t.checklist
let createdAtDate = t => t->createdAt->DateFns.parseString

let pending = t =>
  switch t.status {
  | Pending => true
  | MarkedAsComplete
  | Passed
  | Failed => false
  }

let createdAtPretty = t => t->createdAtDate->DateFns.format("MMMM D, YYYY")

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    createdAt: json->field("createdAt", string),
    status: switch json->field("status", string) {
    | "marked_as_complete" => MarkedAsComplete
    | "pending" => Pending
    | "passed" => Passed
    | "failed" => Failed
    | unknownValue => throw(UnexpectedStatusValue(unknownValue))
    },
    checklist: json->field(
      "checklist",
      array(
        SubmissionChecklistItem.decode(
          json->field("files", array(SubmissionChecklistItem.decodeFile)),
        ),
      ),
    ),
  }
}

let make = (~id, ~createdAt, ~status, ~checklist) => {
  id: id,
  createdAt: createdAt,
  status: status,
  checklist: checklist,
}
