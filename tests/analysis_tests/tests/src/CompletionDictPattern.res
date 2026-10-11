type color = Red | Blue

let colors: dict<color> = dict{"a": Red}

type point = {x: int, y: int}

let points: dict<point> = dict{"a": {x: 1, y: 2}}

// switch colors { | dict{"a": R} => () }
//                              ^com

// switch colors { | dict{"a": } => () }
//                             ^com

// switch colors { | dict{"a": ?Some(B)} => () }
//                                    ^com

// switch colors { | dict{"a": ?} => () }
//                              ^com

// switch colors { | dict{"a": Red, "b": } => () }
//                                       ^com

// switch points { | dict{"a": {}} => () }
//                              ^com

// switch points { | dict{"a": {x: 1, }} => () }
//                                    ^com

// switch colors { |  }
//                   ^com

// let _ = switch points { | dict{"a": p} => p. }
//                                             ^com

// let _ = switch points { | dict{"a": ?p} => p-> }
//                                               ^com
