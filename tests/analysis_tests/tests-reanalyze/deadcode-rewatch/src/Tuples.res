open Belt

@genType
let testTuple = ((a, b)) => a + b

@genType
type coord = (int, int, option<int>)

@genType
let origin = (0, 0, Some(0))

@genType
let computeArea = ((x, y, z)) => {
  open Option
  x * y * z->mapWithDefault(1, n => n)
}

@genType
let computeAreaWithIdent = ((x, y, z): coord) => {
  open Option
  x * y * z->mapWithDefault(1, n => n)
}

@genType
let computeAreaNoConverters = ((x: int, y: int)) => x * y

@genType
let coord2d = (x, y) => (x, y, None)

@genType
type coord2 = (int, int, Js.Nullable.t<int>)

@genType
type person = {
  name: string,
  age: int,
}

@genType
type couple = (person, person)

@genType
let getFirstName = ((first, _second): couple) => first.name

@genType
let marry = (first, second): couple => (first, second)

@genType
let changeSecondAge = ((first, second): couple): couple => (first, {...second, age: second.age + 1})

