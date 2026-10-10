// Completion for a value missing before the comma of the next row

type color = Red | Blue

type pair = {a: color, b: color}

let pair = {a: Red, b: Blue}

let colors: dict<color> = dict{"a": Red}

let takesPair = (p: pair) => p

let takesColors = (d: dict<color>) => d

let someFn = (~isOff: bool, ()) => isOff

// let _ = takesPair({a: , b: Blue})
//                       ^com

// let _ = takesPair({a: Red, b: , })
//                               ^com

// let _ = takesColors(dict{"a": , "b": Blue})
//                               ^com

// let _ = switch pair { | {a: , b: Blue} => () }
//                             ^com

// let _ = switch colors { | dict{"a": , "b": Blue} => () }
//                                     ^com

// let _ = switch colors { | dict{"a": ?, "b": ?Some(Blue)} => () }
//                                      ^com

// let _ = someFn(~isOff=, ())
//                     ^com

// let _ = someFn(~isOff=, ())
//                       ^com
