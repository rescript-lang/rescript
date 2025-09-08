type x = {name: string, age: int}

let x = {name: "123", age: 12}

let {name, } = x
//         ^com

// let {} = x
//      ^com

let f = (x: x) => {
  let {name, } = x
  //         ^com
  name
}

let f2 = (x: x) => {
  // let {} = x
  //      ^com
  ignore(x)
}

type recordWithOptField = {
  someField: int,
  someOptField?: bool
}

let x: recordWithOptField = {
  someField: 123
}

// let {} = x
//      ^com