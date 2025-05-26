module SomeModule = {
  type someType = {
    one: bool,
    two: int,
  }
}

type someOtherType = {
  ...SomeModule.someType,
  three: string,
}

let v = {
  one: true,
  two: 1,
  three: "hello",
}

let takesSomeType = (s: SomeModule.someType) => {
  s.one
}

let x = takesSomeType(v)
