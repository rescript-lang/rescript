@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = switch 1 {
| 1 => Ok(1)
| _ => Error(#Invalid)
}

type ff = Failed(int) | GoOn

let xx = Failed(1)

let ff = {
  let? Failed(x) = xx
  Ok(x)
}
