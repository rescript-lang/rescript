module MM: {
  let x: int
  let y: int
} = {
  let y = 55
  let x = y
}

let _ = Js.log(MM.x)
