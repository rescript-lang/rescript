@genType
module CompV4 = {
  @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@genType
type person = {
  name: string,
  age: int,
}

@genType
type props2<'a> = {
  randomString: string,
  poly: 'a,
}

@genType type renderMe<'a> = React.component<props2<'a>>

@genType.import("./hookExample") @react.component
external make: (
  ~actions: React.element=?,
  ~person: person,
  ~children: React.element,
  ~renderMe: renderMe<'a>,
) => React.element = "make"
