type a = One | Two | Three
type b = | ...a | Four | Five
type c = Six | Seven

let lookup = (b: b) =>
  switch b {
  | ...c as c => Js.log(c)
  | Four => Js.log("four")
  | Five => Js.log("five")
  }
