type a = One | Two(int) | Three
type b = | ...a | Four | Five
type c = Six | Seven(string)
type d = | ...b | ...c

let doWithA = (a: a) => {
  switch a {
  | One => Console.log("aaa")
  | Two(_) => Console.log("twwwoooo")
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
  | Six | Seven(_) => Console.log("Got rest of d")
  }
