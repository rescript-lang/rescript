type a = One | Two | Three
type b = | ...a | Four | Five
type c = {name: string}

let lookup = (b: b) =>
  switch b {
  | ...c as c => Console.log(c)
  | Four => Console.log("four")
  | Five => Console.log("five")
  }
