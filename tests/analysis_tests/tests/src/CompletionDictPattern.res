type color = Red | Blue

let colors: dict<color> = dict{"a": Red}

// switch colors { | dict{"a": R} => () }
//                              ^com

// switch colors { | dict{"a": } => () }
//                             ^com

// switch colors { | dict{"a": ?Some(B)} => () }
//                                    ^com
