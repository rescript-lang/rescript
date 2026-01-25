type someTyp = Variant | WithPayload(bool) | One | Two | Three | Four | Five | Six | Seven(int)

type someRecord = {
  someValue: string,
  otherValue: bool,
  typ: someTyp,
}

@val external y: someRecord = "otherVariable"

switch y {
| {otherValue: false} => Console.log("first")
}

switch y {
| {typ: WithPayload(true)} => Console.log("first")
}

let arr = [1]

switch arr {
| [] => Console.log("")
}

switch arr {
| [one] => Console.log(one)
}

switch arr {
| [1, 2] => ()
}

let lst = list{}

switch lst {
| list{} => ()
}

switch lst {
| list{1, 2} => ()
}

switch lst {
| list{1} => ()
}

switch "abc" {
| "" => ()
}

switch 0 {
| 1 => ()
}

let tuple = (1, true)

switch tuple {
| (_, false) => ()
}
