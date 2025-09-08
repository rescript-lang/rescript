type a = One | Two | Three
type b = | ...a | Four | Five
type c = {name: string}

let lookup = (b: b) =>
  switch b {
  | ...c as c => Js.log(c)
  | Four => Js.log("four")
  | Five => Js.log("five")
  }
