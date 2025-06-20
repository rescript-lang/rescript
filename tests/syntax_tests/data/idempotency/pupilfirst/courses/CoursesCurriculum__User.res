type t = {
  id: string,
  name: string,
  avatarUrl: string,
  title: option<string>,
}

let id = t => t.id
let name = t => t.name
let avatarUrl = t => t.avatarUrl
let title = t => t.title

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    avatarUrl: json->field("avatarUrl", string),
    title: json->field("title", nullable(string))->Js.Null.toOption,
  }
}
