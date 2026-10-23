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
  courses->List.sort((c1, c2) => String.localeCompare(c1->name, c2->name)->int_of_float)
