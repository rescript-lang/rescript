@@warning("-a")

@deriving(abstract)
type t = {
  @as("Content-Type") mutable hi: int,
  @as("l") mutable low: int,
  @as("open") mutable x: int,
}
/* [@@bs.x] */
/*
external hiSet :
      t -> int -> unit = "Content-Type"
        "\132\149\166\190\000\000\000\019\000\000\000\004\000\000\000\014\000\000\000\012\176\145BE\167,Content-Type@"
*/

let v = t(~hi=3, ~low=2, ~x=2)

let (a, b, c) = (v->hiGet, v->lowGet, v->xGet)

/**
  v |. (hi, lo)
*/
let ff = () => {
  v->hiSet(3)
  v->lowSet(2)
}

@deriving(abstract)
type a = {
  @optional @as("lo-x") mutable low: string,
  hi: int,
}

/**
external a : ?low:int -> hi:int -> a
low: a -> int option [@@return undefined_to_opt]
lowSet : a -> int -> unit
*/
let h0 =
  a(~hi=2, ~low="x", ...)

let h1 = a(~hi=2, ~low="x", ())

let h2 = a(~hi=2, ())

let hh = x => {
  x->lowSet("3")
  x->lowGet
}

/* should we make the type of

    lowSet : a -> string option -> unit
    lowSet : a -> string -> unit
*/

let hh2 = x =>
  switch x->lowGet {
  | None => 0
  | Some(_) => 1
  }

@deriving(abstract)
type css = {
  @optional a0: int,
  @optional a1: int,
  @optional a2: int,
  @optional a3: int,
  @optional a4: int,
  @optional a5: int,
  @optional a6: int,
  @optional a7: int,
  @optional a8: int,
  @optional @as("xx-yy") a9: int,
  @optional a10: int,
  @optional a11: int,
  @optional a12: int,
  @optional a13: int,
  @optional a14: int,
  @optional a15: int,
}

let u = css(~a9=3, ())
let v = switch u->a9Get {
| None => 0
| Some(x) => x
}
