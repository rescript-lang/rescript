type a = One | Two | Three
type b = | ...a | Four | Five
type c = Six | Seven
type d = | ...b | ...c

let doWithA = (a: a) => {
  switch a {
  | One => Console.log("aaa")
  | Two => Console.log("twwwoooo")
  | Three => Console.log("threeeee")
  }
}

let doWithB = (b: b) => {
  switch b {
  | One => Console.log("aaa")
  | _ => Console.log("twwwoooo")
  }
}

let lookup = (b: b) =>
  switch b {
  | ...a as a => doWithA(a)
  | Four => Console.log("four")
  | Five => Console.log("five")
  }

let lookup2 = (d: d) =>
  switch d {
  | ...a as a => doWithA(a)
  | ...b as b => doWithB(b)
  | Six | Seven => Console.log("Got rest of d")
  }

let lookupOpt = (b: option<b>) =>
  switch b {
  | Some(...a as a) => doWithA(a)
  | Some(Four) => Console.log("four")
  | Some(Five) => Console.log("five")
  | None => Console.log("None")
  }

module Foo = {
  type zz = First | Second
  type xx = | ...zz | Third
}

let doWithZ = (z: Foo.zz) =>
  switch z {
  | First => Console.log("First")
  | Second => Console.log("Second")
  }

let lookup3 = (d: Foo.xx) =>
  switch d {
  | ...Foo.zz as z => Console.log(z)
  | Third => Console.log("Third")
  }
