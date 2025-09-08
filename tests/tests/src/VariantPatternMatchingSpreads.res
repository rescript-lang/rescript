type a = One | Two | Three
type b = | ...a | Four | Five
type c = Six | Seven
type d = | ...b | ...c

let doWithA = (a: a) => {
  switch a {
  | One => Js.log("aaa")
  | Two => Js.log("twwwoooo")
  | Three => Js.log("threeeee")
  }
}

let doWithB = (b: b) => {
  switch b {
  | One => Js.log("aaa")
  | _ => Js.log("twwwoooo")
  }
}

let lookup = (b: b) =>
  switch b {
  | ...a as a => doWithA(a)
  | Four => Js.log("four")
  | Five => Js.log("five")
  }

let lookup2 = (d: d) =>
  switch d {
  | ...a as a => doWithA(a)
  | ...b as b => doWithB(b)
  | Six | Seven => Js.log("Got rest of d")
  }

let lookupOpt = (b: option<b>) =>
  switch b {
  | Some(...a as a) => doWithA(a)
  | Some(Four) => Js.log("four")
  | Some(Five) => Js.log("five")
  | None => Js.log("None")
  }

module Foo = {
  type zz = First | Second
  type xx = | ...zz | Third
}

let doWithZ = (z: Foo.zz) =>
  switch z {
  | First => Js.log("First")
  | Second => Js.log("Second")
  }

let lookup3 = (d: Foo.xx) =>
  switch d {
  | ...Foo.zz as z => Js.log(z)
  | Third => Js.log("Third")
  }
