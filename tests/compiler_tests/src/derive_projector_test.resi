@@warning("-30")
@deriving({accessors: accessors}) type a = {u_x: int}

type rec b<'a> = {b_x: int}
@deriving({accessors: accessors}) and c<'a> = {c_x: int}

type rec d =
  | D_empty
  | D_int(int)
  | D_tuple(int, string)
  | NewContent(string)
  | D_tweak((int, string))
  | Hei
and u = Hei
and h = {d: d, h: list<h>, u_X: int}

@deriving({accessors: accessors}) and e = {d: d}

let v: d
let h: list<d>

@deriving(accessors) type hh = Xx(int)

@deriving(accessors) type t = A(int => int)
