@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = Ok(1)

let f = {
  let Ok(_) = x
  Ok(1)
}
