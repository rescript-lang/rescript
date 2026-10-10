type color = Red | Blue

type point = {x: int, y: int}

let takesColors = (d: dict<color>) => d

// let a: dict<color> = dict{"a": }
//                                ^com

// let b: dict<color> = dict{"a": R}
//                                 ^com

// let c: dict<color> = dict{"a": Red, "b": B}
//                                           ^com

// let e: dict<array<color>> = dict{"a": [R]}
//                                         ^com

// let f: dict<point> = dict{"a": {}}
//                                 ^com

// let g = takesColors(dict{"a": })
//                               ^com

// let h = takesColors()
//                     ^com
