type student = {
  name: string,
  age: int,
}
let _ = switch Some(1) {
| Some(1) =>
  switch %raw("1") {
  | 1 => {name: "hi", age: 1}
  | 2 => {
      name: "bye",
      age: Js.Math.floor(1.),
    }
  | _ => assert(false)
  }
| _ => assert(false)
}
