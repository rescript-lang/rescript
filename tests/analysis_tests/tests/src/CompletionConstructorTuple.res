type payload = {
  name: string,
  enabled: bool,
}

type t = Pair(int, payload) | Nested((int, payload), string)

let consume = (value: t) => ignore(value)

// consume(Pair((1, {})))
//                    ^com

let value = Pair(1, {name: "test", enabled: true})

// switch value { | Pair((_, {}))}
//                            ^com

// consume(Nested((1, {}), ""))
//                     ^com

// switch value { | Nested((_, {}), _) => ()}
//                              ^com

// consume(Nested(((1, {}), "")))
//                      ^com

// switch value { | Nested(((_, {}), _)) => ()}
//                               ^com
