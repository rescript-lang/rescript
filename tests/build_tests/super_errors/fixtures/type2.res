@send external push: (array<'a>, 'a) => unit = "push"

let a = []
let () = {
  push(a, 3)->ignore
  push(a, "3")->ignore
}
