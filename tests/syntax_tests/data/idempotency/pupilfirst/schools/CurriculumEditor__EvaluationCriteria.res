type t = {
  id: string,
  name: string,
}

let name = t => t.name

let id = t => t.id

let decode = json => {
  open Json.Decode
  {
    name: json->field("name", string),
    id: json->field("id", string),
  }
}
