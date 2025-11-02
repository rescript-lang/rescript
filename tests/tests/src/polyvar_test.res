@@config(no_export)

let f = x =>
  switch x {
  | #A => "A"
  | #B => "B"
  }

let f1 = x =>
  switch x {
  | #A => "A"
  }

let ff = {
  let x = 66
  if x >= 66 {
    "B"
  } else {
    "A"
  }
}
let () = Console.log((f(#A), ff, f1(#A)))
