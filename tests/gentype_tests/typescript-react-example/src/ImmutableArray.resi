@ocaml.doc(" Immutable arrays are covariant. ")
type t<+'a>

@ocaml.doc(" Redefine the [_] syntax, and disable the assignment [_] = _. ")
module Array: {
  let get: (t<'a>, int) => option<'a>
}

@ocaml.doc(" Converting from/to normal arrays involves making a copy. ")
let fromArray: array<'a> => t<'a>

let toArray: t<'a> => array<'a>

@ocaml.doc(" Subset of the Belt.Array oprerations that do not mutate the array. ")
let length: t<'a> => int

let size: t<'a> => int

let get: (t<'a>, int) => option<'a>

let getExn: (t<'a>, int) => 'a

let getUnsafe: (t<'a>, int) => 'a

let getUndefined: (t<'a>, int) => Js.undefined<'a>

let shuffle: t<'a> => t<'a>

let reverse: t<'a> => t<'a>

let makeUninitialized: int => t<Js.undefined<'a>>

let makeUninitializedUnsafe: int => t<'a>

let make: (int, 'a) => t<'a>

let range: (int, int) => t<int>

let rangeBy: (int, int, ~step: int) => t<int>

let makeBy: (int, int => 'a) => t<'a>

let makeByAndShuffle: (int, int => 'a) => t<'a>

let zip: (t<'a>, t<'b>) => t<('a, 'b)>

let zipBy: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

let unzip: t<('a, 'a)> => (t<'a>, t<'a>)

let concat: (t<'a>, t<'a>) => t<'a>

let concatMany: t<t<'a>> => t<'a>

let slice: (t<'a>, ~offset: int, ~len: int) => t<'a>

let sliceToEnd: (t<'a>, int) => t<'a>

let copy: t<'a> => t<'a>

let forEach: (t<'a>, 'a => unit) => unit

let map: (t<'a>, 'a => 'b) => t<'b>

let keepWithIndex: (t<'a>, ('a, int) => bool) => t<'a>

let keepMap: (t<'a>, 'a => option<'b>) => t<'b>

let forEachWithIndex: (t<'a>, (int, 'a) => unit) => unit

let mapWithIndex: (t<'a>, (int, 'a) => 'b) => t<'b>

let partition: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let reduceReverse: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let reduceReverse2: (t<'a>, t<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

let some: (t<'a>, 'a => bool) => bool

let every: (t<'a>, 'a => bool) => bool

let every2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let some2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let cmp: (t<'a>, t<'a>, ('a, 'a) => int) => int

let eq: (t<'a>, t<'a>, ('a, 'a) => bool) => bool
