@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = Ok(1)

let fn = (): int => {
  let? Ok(v) = Error("fail")
  42
}
