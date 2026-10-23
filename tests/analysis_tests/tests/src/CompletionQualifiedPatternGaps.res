module Qualified = {
  type t = Payload(bool) | Gap(bool, bool)
}

let value = Qualified.Gap(true, false)

// switch value { | Qualified.Payload() => () }
//                                    ^com

// switch value { | Qualified.Payload(t) => () }
//                                     ^com

// switch value { | Qualified.Gap(true,  false) => () }
//                                      ^com

// switch value { | Qualified.Payload(bound) => switch bound { | t => () } }
//                                                                ^com

type gap = Gap(bool, bool)
let ordinary = Gap(true, false)

// switch ordinary { | Gap(true,  false) => () }
//                               ^com

type poly = [#gap(bool, bool)]
let poly: poly = #gap(true, false)
let consumePoly = (value: poly) => ignore(value)

// consumePoly(#gap(true,  false))
//                        ^com

// switch poly { | #gap(true,  false) => () }
//                            ^com
