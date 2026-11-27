@unboxed
type rec t =
  | Boolean(bool)
  | @as(null) Null
  | @as(undefined) Undefined
  | String(string)
  | Number(float)
  | Object(Dict.t<t>)
  | Array(array<t>)

type group = {
  id: string,
  name: string,
}

let decodeGroup = group => {
  switch group {
  | dict{"id": String(id), "name": String(name)} => (id, name)
  | _ => ("e", "f")
  }
}

let decodeNull = x =>
  switch x {
  | dict{"field": Null} => "yes it's null"
  | _ => "no"
  }

let decodeUndefined = x =>
  switch x {
  | dict{"field": Undefined} => "yes it's undefined"
  | _ => "no"
  }
