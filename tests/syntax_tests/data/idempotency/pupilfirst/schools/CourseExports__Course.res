type id = string

type t = {id: id}

let id = t => t.id

let decode = json => {
  open Json.Decode
  {id: json->field("id", string)}
}
