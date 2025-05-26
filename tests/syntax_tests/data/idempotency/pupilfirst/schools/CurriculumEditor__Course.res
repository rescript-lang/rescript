type t = {id: string}

let id = t => t.id

let decode = json => {
  open Json.Decode
  {id: json->field("id", string)}
}
