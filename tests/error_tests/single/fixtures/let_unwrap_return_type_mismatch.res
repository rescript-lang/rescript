@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = Some(1)

let fn = (): int => {
  let? Some(x) = None
  42
}
