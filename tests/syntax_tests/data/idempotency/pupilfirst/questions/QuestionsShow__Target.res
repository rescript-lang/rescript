type t = {
  id: option<string>,
  title: string,
}

let id = t => t.id
let title = t => t.title

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", nullable(string))->Null.toOption,
    title: json->field("title", string),
  }
}
