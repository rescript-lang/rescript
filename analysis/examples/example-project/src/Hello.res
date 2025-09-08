let someLongName = 10

let otherLongName = "string"

let x = {"a": 3}

let r = Other.something

let l = More.inner + More.n + Other.inner

let n = More.n

let _ = More.party
let _ = string_of_bool

/* let m = {More.a: 2, b: 32.}; */

module Something = {
  open Other

  let m = {name: "Me", age: 0}
  let animal = Things(10)
  let other = Things(2)
  let me: animals = People("Hie")
  let x = something + 10
  let r = m.name

  let awesome = 20
  if true {
    ()
  }
}

open! Something

let y = x + 10

switch me {
| Things(n) => ()
| _ => ()
}

let z = x * x

let aThing = 10 + Other.something

@ocaml.doc(" Some docs about this **awesome** thing. ")
let awesome =
  100 + m.age

let thing = "thing"

let transform = (x, y) => x ++ Js.Float.toString(y)

let z = transform("hello ", 5.)

let zzz = 1

let more = 20

@ocaml.doc(" Something here ")
let added =
  10 + awesome

open Other

open Hashtbl

@ocaml.doc(" Some more documentation about this ")
let awesome = x => x + 2

let a = list{"hello", "my fine" ++ "folks", "in boonville"}

let div = (~x, ~y, ~children, ()) => 10

let m = <div x="10" y="20" />

let something = animal =>
  switch animal {
  | blank => ()
  }

something(animal)

let someFunction = (memorableName, {contents}) => {
  let innerMemorable = 20
  memorableName + innerMemorable
}

/* let awesome = 10000; */

/* let awesome = 111; */

let z = 10

let z = find

let z = later

let m = Other.later

for _index in 0 to 10 {
  print_endline("hellO")
}

module OneOneOneOne = {
  module TwoTwoTwoTwo = {
    let xxxxxxxxxx = 10
  }
}
let r = OneOneOneOne.TwoTwoTwoTwo.xxxxxxxxxx

type awesome = {
  one: string,
  two: float,
}

open OneOneOneOne.TwoTwoTwoTwo

include OneOneOneOne.TwoTwoTwoTwo

include More

let _ = Other.oo.person.name

type lots =
  | Parties
  | Plutocrats(int, float)
  | Possums
  | Oppossums

let y = Some(10 + awesome(3))

let z = {contents: 30}
let party = {one: "one", two: 2.}

let {one, two} = party

let thing = () => 34 + 43

type more = awesome

let {contents} = z

switch y {
| Some(u) => ()
| None => ()
}

/* let x = [%raw " hello"]; */

let awesome = "hello"

type shortReference = (string, list<string>, string)

type reference = {
  uri: string,
  moduleName: string,
  modulePath: list<string>,
  name: string,
}

type typeSource =
  | Builtin(string)
  | Public(reference)
  | NotFound

type lockfile = {
  version: int,
  pastVersions: Belt.HashMap.Int.t<list<(shortReference, int)>>,
  current: list<(shortReference, int)>,
}

