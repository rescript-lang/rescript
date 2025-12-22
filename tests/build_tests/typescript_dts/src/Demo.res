let add = (a, b) => a + b

let greet = name => "Hello, " + name

let identity = x => x

let asyncIdentity = async x => x

let processOption = (opt: option<int>): int => {
  switch opt {
  | Some(v) => v
  | None => 0
  }
}

type record = {foo: string, bar: int}

let myNumber = 42

let myString = "hello"
