type kind = First | Second

let base: array<kind> = [First]
let baseList: list<kind> = list{First}
let takesArray = (xs: array<kind>) => ignore(xs)
let pair = (a: kind, b: kind) => [a, b]

// let ys: array<kind> = [...base, ]
//                                 ^com

// let ys: array<kind> = [...base, Fi]
//                                   ^com

// let ys: array<kind> = [ ...base]
//                        ^com

// let ys: array<kind> = [Se, ...base]
//                          ^com

// let ys: array<kind> = [First, ...base, ]
//                                        ^com

// let ys: array<kind> = [...base, First, ...base, ]
//                                                 ^com

// let ys: array<kind> = [...base, , ...base]
//                                ^com

// let ys: array<kind> = [...base, Fi, ...base]
//                                   ^com

// let ys: array<kind> = [...base, ...[Se]]
//                                       ^com

// let ys: array<kind> = [First, ...ba]
//                                    ^com

// let ys: array<kind> = [...pair(First, ), First]
//                                       ^com

// let _ = takesArray([...base, ])
//                              ^com

// let _ = takesArray([...base, Se, ...base])
//                                ^com

// let ys = [...base, Fi]
//                      ^com

// A list literal gets no typed completion, with or without a spread
// let ys: list<kind> = list{...baseList, Fi, ...baseList}
//                                          ^com
