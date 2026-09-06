type payload = {name: string, enabled: bool}
type t = Unary((payload, payload)) | Pair(payload, payload)
let consume = (value: t) => ignore(value)
let value = Pair({name: "", enabled: true}, {name: "", enabled: false})
let consumeOption = (value: option<(payload, payload)>) => ignore(value)
let optionValue = Some(({name: "", enabled: true}, {name: "", enabled: false}))
type poly = [#Poly((int, payload))]
let consumePoly = (value: poly) => ignore(value)
let polyValue: poly = #Poly((1, {name: "", enabled: true}))

// consume(Unary({}, {}))
//                ^com

// consume(Unary({}, {}))
//                    ^com

// switch value { | Unary({}, _) => () }
//                         ^com

// switch value { | Unary(_, {}) => () }
//                            ^com

// consumeOption(Some({}, {}))
//                     ^com

// consumeOption(Some({}, {}))
//                         ^com

// switch optionValue { | Some(_, {}) => () | None => () }
//                                 ^com

// consumePoly(#Poly((1, {})))
//                        ^com

// switch polyValue { | #Poly((_, {})) => () }
//                                 ^com

// consumePoly(#Poly(1, {}))
//                       ^com

// switch polyValue { | #Poly(_, {}) => () }
//                                ^com

// switch value { | Pair((first, second)) => first. }
//                                                 ^com

// switch value { | Pair((first, second)) => second. }
//                                                  ^com

// switch value { | Unary(first, second) => first. }
//                                                ^com

// switch value { | Unary(first, second) => second. }
//                                                 ^com

// switch polyValue { | #Poly((_, record)) => record. }
//                                                   ^com

// switch polyValue { | #Poly(_, record) => record. }
//                                                 ^com

let consumeResult = (value: result<(payload, payload), (payload, payload)>) => ignore(value)
let resultValue: result<(payload, payload), (payload, payload)> = Ok(({name: "", enabled: true}, {name: "", enabled: false}))

// consumeResult(Ok({}, {}))
//                       ^com

// switch resultValue { | Ok(_, {}) => () | _ => () }
//                               ^com

// switch resultValue { | Ok(first, second) => second. | _ => () }
//                                                    ^com

// consumeResult(Error({}, {}))
//                          ^com

// switch resultValue { | Error(_, {}) => () | _ => () }
//                                  ^com

// switch resultValue { | Error(first, second) => second. | _ => () }
//                                                       ^com
