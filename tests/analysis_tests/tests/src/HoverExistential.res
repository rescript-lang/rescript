type rec any = Any(array<'a>, 'a => string): any

let showAll = (Any(xs, f)) => xs->Array.map(f)
//                 ^hov
