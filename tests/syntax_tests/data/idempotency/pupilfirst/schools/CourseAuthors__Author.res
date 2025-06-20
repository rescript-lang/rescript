type t = {
  id: string,
  name: string,
  avatarUrl: option<string>,
  email: string,
}

let name = t => t.name
let avatarUrl = t => t.avatarUrl
let id = t => t.id
let email = t => t.email

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    email: json->field("email", string),
    name: json->field("name", string),
    avatarUrl: json->optional(field("avatarUrl", string)),
  }
}

let create = (~id, ~name, ~email, ~avatarUrl) => {
  id: id,
  name: name,
  email: email,
  avatarUrl: avatarUrl,
}

let sort = l => l->ArrayUtils.copyAndSort((x, y) => x.name < y.name ? -1 : 1)

let updateName = (name, t) => {...t, name: name}
