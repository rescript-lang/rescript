type someTyp = [#valid | #invalid]

let ff = (v1: someTyp, v2: someTyp) => {
  let x = switch (v1, v2) {
  //      ^c-a (#valid, #valid) | (#invalid, _)
  | (#valid, #invalid) => ()
  }
  x
}
