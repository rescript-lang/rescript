type config = {
  name: string,
  version: string,
  debug: bool,
}

type subConfig = {
  version: string,
  debug: bool,
}

// Basic rest pattern in let binding
let {name, ...subConfig as rest} = ({name: "test", version: "1.0", debug: true}: config)
let _ = (name, rest)

// Rest pattern in match arm
let describe = (c: config) =>
  switch c {
  | {name, ...subConfig as rest} => (name, rest)
  }

// Rest pattern in function parameter
let getName = ({name, ...subConfig as _rest}: config) => name

// Optional field overlap: className is in both explicit (as optional) and rest type
type fullProps = {
  className?: string,
  style?: string,
  onClick: unit => unit,
}

type baseProps = {
  className?: string,
  style?: string,
  onClick: unit => unit,
}

let extractClassName = ({?className, ...baseProps as rest}: fullProps) => {
  let _ = className
  rest
}

// Polymorphic rest type
type container<'a> = {
  id: string,
  value: 'a,
}

type valueContainer<'a> = {
  value: 'a,
}

let {id, ...valueContainer<int> as intRest} = ({id: "1", value: 42}: container<int>)
let _ = (id, intRest)

// Polymorphic rest in function parameter
let getValue = ({id: _, ...valueContainer<'a> as rest}: container<'a>) => rest

type wrapped =
  | Wrap(config)
  | Mirror(config)

// Nested record rest in a tuple pattern
let getTupleRest = (({name: _, ...subConfig as rest}, _): (config, int)) => rest

let tupleRest = getTupleRest((({name: "tuple", version: "2.0", debug: false}: config), 1))

// Nested record rest in constructor and or-pattern matches
let getWrappedRest = wrapped =>
  switch wrapped {
  | Wrap({name: _, ...subConfig as rest})
  | Mirror({name: _, ...subConfig as rest}) => rest
  }

let wrappedRest = getWrappedRest(Wrap({name: "wrapped", version: "3.0", debug: true}))
