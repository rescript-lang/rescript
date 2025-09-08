let () = for i in 0 to 10 {
  ()
}

for i in 0 to 10 {
  let a = 1
  let b = 2
  sideEffect()
}

let x = @attr for i in 0 to 10 { () }
