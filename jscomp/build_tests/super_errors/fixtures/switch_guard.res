@val external foo: string = "foo"
external someFunction: unit => string = "someFunction"

let bar = () => {
  switch foo {
  | "world" if "horse" => ()
  | _ => someFunction()
  }
}
