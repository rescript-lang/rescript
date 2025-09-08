let x = {
  let a = 1
  let b = 2
  sideEffect()
  sideEffect2()
}

let x = {
  let a = 1

  let b = 2

  a + b
}

let x = {
  exception Exit

  exception Terminate

  module B = Belt

  sideEffect()

  open React

  sideEffect2()
}

let x = {
  exception Exit
  exception Terminate
  module B = Belt
  sideEffect()
  open React
  sideEffect2()
}
