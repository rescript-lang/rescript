type payload = {
  name: string,
  enabled: bool,
}

type t = Pair(int, payload)

let consume = (value: t) => ignore(value)

// consume(Pair((1, {})))
//                    ^com

let value = Pair(1, {name: "test", enabled: true})

// switch value { | Pair((_, {}))}
//                            ^com
