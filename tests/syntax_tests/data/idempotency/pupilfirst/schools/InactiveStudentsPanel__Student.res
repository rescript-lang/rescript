type t = {
  id: string,
  name: string,
  avatarUrl: string,
  teamId: string,
}

let id = t => t.id

let teamId = t => t.teamId

let name = t => t.name

let avatarUrl = t => t.avatarUrl

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    teamId: json->field("teamId", string),
    name: json->field("name", string),
    avatarUrl: json->field("avatarUrl", string),
  }
}
