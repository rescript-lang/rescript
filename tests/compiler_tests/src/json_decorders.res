type user = {
  id: string,
  name: string,
  age: int,
  email: option<string>,
}

type group = {
  id: string,
  name: string,
  users: array<user>,
}

let decodeUser = json => {
  switch json {
  | JSON.Object(dict{
      "id": JSON.String(id),
      "name": String(name),
      "age": Number(age),
      "email": ?email,
    }) =>
    Some({
      id,
      name,
      age: age->Float.toInt,
      email: switch email {
      | Some(String(email)) => Some(email)
      | _ => None
      },
    })
  | _ => None
  }
}

let decodeGroup = json => {
  switch json {
  | JSON.Object(dict{"id": JSON.String(id), "name": String(name), "users": Array(users)}) =>
    Some({
      id,
      name,
      users: users->Array.filterMap(decodeUser),
    })
  | _ => None
  }
}
