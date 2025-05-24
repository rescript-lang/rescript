type t = {
  id: string,
  title: string,
  description: string,
  creatorId: string,
  editorId: option<string>,
  createdAt: string,
  updatedAt: string,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    title: json->field("title", string),
    description: json->field("description", string),
    creatorId: json->field("creatorId", string),
    editorId: json->field("editorId", nullable(string))->Js.Null.toOption,
    createdAt: json->field("createdAt", string),
    updatedAt: json->field("updatedAt", string),
  }
}

let id = t => t.id

let description = t => t.description

let title = t => t.title

let creatorId = t => t.creatorId

let editorId = t => t.editorId

let createdAt = t => t.createdAt

let updatedAt = t => t.updatedAt

let create = (id, title, description, creatorId, editorId, createdAt, updatedAt) => {
  id: id,
  title: title,
  description: description,
  creatorId: creatorId,
  editorId: editorId,
  createdAt: createdAt,
  updatedAt: updatedAt,
}
