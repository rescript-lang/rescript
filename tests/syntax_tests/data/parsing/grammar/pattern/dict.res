let someDict = dict{
  "one": "one",
}

let dict{"one": ?one} = someDict

let foo = () => switch someDict {
| dict{"one": "one"} => Js.log("one")
| _ => Js.log("not one")
}

@unboxed
type rec json =
  | Boolean(bool)
  | @as(null) Null
  | String(string)
  | Number(float)
  | Object(dict<json>)
  | Array(array<t>)

type user = {
  name: string,
  age?: float,
}

let decodeUser = (json: json): option<user> => {
  switch json {
  | Object(dict{
    "name": String(name), 
    "age": ageJson
    }) =>
    Some({
      name,
      age: ?switch ageJson {
      | Number(age) => Some(age)
      | _ =>
        /* Invalid age JSON type */
        None
      },
    })
  | _ =>
    Js.log("Not an object.")
    None
  }
}

Js.log(decodeUser(jsonParse(`{"name": "John", "age": 30}`)))