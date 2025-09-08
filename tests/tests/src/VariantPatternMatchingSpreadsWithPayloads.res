type a = One | Two(int) | Three
type b = | ...a | Four | Five
type c = Six | Seven(string)
type d = | ...b | ...c

let doWithA = (a: a) => {
  switch a {
  | One => Js.log("aaa")
  | Two(_) => Js.log("twwwoooo")
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
  | Six | Seven(_) => Js.log("Got rest of d")
  }
