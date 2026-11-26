@@config({flags: ["-enable-experimental", "LetUnwrap"]})

let x = Ok(1)

let? Ok(_) = x
