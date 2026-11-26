type a = One({name: string, age: int}) | Two
type b = | ...a | Three

let b: b = One({name: "hello"})
