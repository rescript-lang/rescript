type t = {
  id: string,
  name: string,
  students: array<string>,
}

let name = t => t.name
let id = t => t.id

let students = t => t.students

let makeFromJS = teamData => {
  id: teamData["id"],
  name: teamData["name"],
  students: teamData["students"]->Array.map(student => student["name"]),
}

let makeArrayFromJs = detailsOfTeams =>
  detailsOfTeams->Belt.Array.keepMap(OptionUtils.map(makeFromJS))

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    students: json->field("students", array(string)),
  }
}
