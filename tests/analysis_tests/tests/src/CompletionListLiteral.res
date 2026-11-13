type kind = First | Second

let takesList = (xs: list<kind>) => ignore(xs)
let takesNested = (xs: list<list<kind>>) => ignore(xs)

// let ys: list<kind> = list{First, }
//                                 ^com

// let ys: list<kind> = list{First, Se}
//                                    ^com

// let ys: list<kind> = list{}
//                           ^com

// let ys: list<kind> = list{Fi, Second}
//                             ^com

// let _ = takesList(list{})
//                        ^com

// let _ = takesList(list{First, Se, Second})
//                                ^com

// let _ = takesNested(list{list{First}, list{Se}})
//                                              ^com

// let ys: list<kind> =
//                     ^com

// let ys: array<list<kind>> = [list{Fi}]
//                                     ^com
