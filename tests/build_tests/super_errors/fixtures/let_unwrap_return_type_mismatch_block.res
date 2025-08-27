@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = Some(1)

let fn = (): int => {
  let x = {
    if 1 > 2 {
      1
    } else {
      let? Some(x) = None
      Some(x)
    }
  }
  42
}
