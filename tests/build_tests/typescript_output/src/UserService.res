let createUser = (name: string, age: int): Types.user => {
  name,
  age,
}

let greetUser = (user: Types.user) => {
  "Hello, " + user.name
}

let getDefaultUser = (): Types.user => Types.defaultUser
