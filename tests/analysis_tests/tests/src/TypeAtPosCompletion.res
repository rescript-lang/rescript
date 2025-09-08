type optRecord = {
  name: string,
  age?: int,
  online?: bool,
}

let optRecord = {
  name: "Hello",
  //             ^com
}

type someVariant = One(int, optRecord)

let x = One(
  1,
  {
    name: "What",
    //            ^com
  },
)

let arr = [
  optRecord,
  //        ^com
]
