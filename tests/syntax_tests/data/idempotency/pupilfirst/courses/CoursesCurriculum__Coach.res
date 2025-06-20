type t = {
  id: string,
  userId: string,
}

let id = t => t.id
let userId = t => t.userId

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    userId: json->field("userId", string),
  }
}
