@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let fn = (): int => {
  let? Some(x) = None
  42
}
