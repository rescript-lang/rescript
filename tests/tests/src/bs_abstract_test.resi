@deriving(abstract)
type rec linked_list<'a> = private {
  /* hd : 'a ; */
  tl: Js.null<linked_list<'a>>,
}

type rec t = (int, int) => bool
@deriving(abstract)
and x = private {
  k: t,
  /* y : string */
}

let f: x
type u
let uf: u => int
let uf1: u => int => int
let uf2: u => int

type u1
let uff: u1 => int
let uff2: u1 => int

let uff3: u1 => int

type u3
let fx: u3 => int
