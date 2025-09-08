type id = string

type t = {
  id: id,
  name: string,
}

let id = t => t.id
let name = t => t.name
let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
  }
}

let sort = courses =>
  courses->List.sort((c1, c2) => Js.String.localeCompare(c2->name, c1->name)->int_of_float)
