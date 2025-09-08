let add = (x, y) => x + y

let foo = (~age, ~name) => name ++ Int.toString(age)

let ff = (~opt1=0, ~a, ~b, (), ~opt2=0, (), ~c) => a + b + c + opt1 + opt2

let compFF = Completion.ff

@react.component
let make = (~name) => React.string(name)
//^cle
