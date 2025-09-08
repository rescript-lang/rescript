foo()
bar()

let () = {
  foo()
  bar()
}

let () = {
  let x = 1
  sideEffect()
  let y = 2
  sideEffect2()
  let z = 3
  sideEffect3()
}

while true {
  sideEffect1()
  let x = 1
  sideEffect2()
  let y = 2
  sideEffect3()
}

switch color  {
  | Blue =>
    getResult()
    sideEffect()
    let x = 1
    sideEffect2()
}
