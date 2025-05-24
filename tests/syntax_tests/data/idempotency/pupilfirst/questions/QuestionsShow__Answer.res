type t = {
  id: string,
  description: string,
  creatorId: string,
  editorId: option<string>,
  createdAt: string,
  archived: bool,
  updatedAt: string,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    description: json->field("description", string),
    creatorId: json->field("creatorId", string),
    editorId: json->field("editorId", nullable(string))->Js.Null.toOption,
    createdAt: json->field("createdAt", string),
    archived: json->field("archived", bool),
    updatedAt: json->field("updatedAt", string),
  }
}

let id = t => t.id

let description = t => t.description

let createdAt = t => t.createdAt

let creatorId = t => t.creatorId

let editorId = t => t.editorId

let updatedAt = t => t.updatedAt

let addAnswer = (answers, answer) => list{answer, ...answers}

let updateAnswer = (answers, newAnswer) =>
  answers->List.map(answer => answer.id == newAnswer.id ? newAnswer : answer)

let answerFromUser = (userId, answers) =>
  answers->List.filter(answer => answer.creatorId == userId)

let archived = t => t.archived

let delete = (id, answers) => answers->List.filter(a => a.id != id)

let create = (id, description, creatorId, editorId, createdAt, updatedAt, archived) => {
  id: id,
  description: description,
  creatorId: creatorId,
  editorId: editorId,
  createdAt: createdAt,
  updatedAt: updatedAt,
  archived: archived,
}
