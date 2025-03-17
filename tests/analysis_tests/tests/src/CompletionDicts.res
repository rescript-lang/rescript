// let dict = Dict.fromArray([])
//                            ^com

// let dict = Dict.fromArray([()])
//                             ^com

// let dict = Dict.fromArray([("key", )])
//                                   ^com

// ^in+
let dict = Dict.fromArray([
  ("key", true),
  //  ("key2", )
  //          ^com
])
// ^in-
