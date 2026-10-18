let xs = [1, 2]
let ys = list{1, 2}

let a = [0, ...xs]
//      ^ast

// Spread literals get an inferred-type hint
let arrays = [...xs, 3]
let lists = list{...ys, 3, ...ys}
//^hin

// Only the spread identifiers get semantic tokens
let tokens = [...xs, ...xs]
//^hig

// let x = [...xs, 3]->le
//                      ^com

// let x = list{...ys, 3, ...ys}->le
//                                 ^com
