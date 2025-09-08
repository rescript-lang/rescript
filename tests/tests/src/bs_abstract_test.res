@deriving(abstract)
type rec linked_list<'a> = {
  hd: 'a,
  mutable tl: Js.null<linked_list<'a>>,
}

let v = linked_list(~hd=3, ~tl=Js.null)

tlSet(v, Js.Null.return(v))

type rec t = (int, int) => bool
@deriving(abstract)
and x = {
  k: t,
  y: string,
}

let x0 = k => x(~k, ~y="xx")
let x1 = k => x(~k, ~y="xx")

let f = x(~k=(x, y) => x == y, ~y="x")

@deriving(abstract)
type u = {
  x: int,
  y0: int => int,
  y1: (int, int) => int,
}

let uf = u => y0Get(u)(1)
let uf1 = u => y1Get(u)(1, ...)
let uf2 = u => y1Get(u)(1, 2)

@deriving(abstract)
type u1 = {
  x: int,
  yyyy: int => int,
  yyyy1: (int, int) => int,
  @optional yyyy2: int => int,
}

let uff = f => (f->yyyyGet)(1)

let uff2 = f => (f->yyyy1Get)(1, 2)

let uff3 = f =>
  switch f->yyyy2Get {
  | None => 0
  | Some(x) => x(0)
  }

@deriving({abstract: light})
type u3 = {
  x: int,
  yyyy: int => int,
  yyyy1: (int, int) => int,
  @optional yyyy2: int => int,
}

let fx = v => v->x
