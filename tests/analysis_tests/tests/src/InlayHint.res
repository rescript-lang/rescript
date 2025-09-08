let not_include = "Not Include"
let string = "ReScript"
let number = 1
let float = 1.1
let char = 'c'

let add = (x, y) => x + y

let my_sum = 3->add(1)->add(1)->add(1)->add(8)

let withAs = (~xx as yyy) => yyy + 1


@react.component
let make = (~name) => React.string(name)

let tuple = ("ReScript", "lol")

let (lang, _) = tuple

type foo = {
  name: string,
  age: int,
}

let bar = () => ({name: "ReScript", age: 2}, tuple)
let ({name:_, age:_}, t) = bar()

let alice = {
  name: "Alice",
  age: 42,
};

let {name, age} = alice;

//^hin