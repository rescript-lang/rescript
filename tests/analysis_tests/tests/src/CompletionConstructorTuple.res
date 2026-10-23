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

type gap = Gap(bool, bool, bool) | TupleGap((bool, bool), bool)
let consumeGap = (value: gap) => ignore(value)
let gap = Gap(true, false, true)

// consumeGap(Gap(true, , false))
//                      ^com

// consumeGap(Gap(true, false, ))
//                             ^com

// consumeGap(TupleGap((true, false), ))
//                                    ^com

// consumeGap(TupleGap((true, ), false))
//                            ^com

// switch gap { | Gap(true, , false) => ()}
//                          ^com

// switch gap { | Gap(true, false, ) => ()}
//                                 ^com

// switch gap { | TupleGap((true, false), ) => ()}
//                                        ^com

// switch gap { | TupleGap((true, ), _) => ()}
//                                ^com
