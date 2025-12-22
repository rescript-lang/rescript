type person = {
  name: string,
  age: int,
}

module X = {
  let ff = 15
}

let ff = 16

open X

let f2 = ff

module RecordExample = {
  type t = {
    name: string,
    age: int,
  }
  let person = {name: "John", age: 30}
}

open RecordExample

let p = {name: "Jane", age: 25}
