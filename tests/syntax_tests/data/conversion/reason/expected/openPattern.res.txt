let {T.a: a} = a()
let [Color.Blue] = a()
let list{Color.Blue} = a()
let (Color.Blue, Red) = a()

let Color.Blue = blue

module Color = {
  type t = Red | Blue | Green
  let red = Red
  let blue = Blue
  let green = Green
}

let () = switch (Color.red, Color.blue, Color.green) {
| (Color.Red, Blue, Green) => Js.log("hello world")
| _ => ()
}

let () = switch [Color.red, Color.blue, Color.green] {
| [Color.Red, Blue, Green] => Js.log("hello world")
| _ => ()
}
