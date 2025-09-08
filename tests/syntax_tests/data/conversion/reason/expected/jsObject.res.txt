let component = props["Component"]

let element = props["element"]

let y = {"age": 30}
let y = {"age": 30, "name": "steve"}

type propField<'a> = {.}
type propField<'a> = {..} as 'a
type propField<'a> = {..} as 'a
type propField<'a> = Js.nullable<{..} as 'a>

type propField<'a> = {"a": b}
type propField<'a> = {.."a": b}
type propField<'a> = {"a": {"b": c}}

user["address"]
user["address"]["street"]
user["address"]["street"]["log"]

user["address"] = "Avenue 1"
user["address"]["street"] = "Avenue"
user["address"]["street"]["number"] = "1"

school["print"](direction["name"], studentHead["name"])
city["getSchool"]()["print"](direction["name"], studentHead["name"])
