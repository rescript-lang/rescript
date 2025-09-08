@@warning("-104")
@deriving({accessors: accessors}) type a = {u_x: int}

type rec b<'a> = {b_x: int}
@deriving(accessors) and c<'a> = {c_x: int}

type rec d =
  | D_empty
  | D_int(int)
  | D_tuple(int, string)
  | NewContent(string)
  | D_tweak((int, string))
  | Hei /* two hei derived, however, this hei is not accessible any more */
and u = Hei
and h = {d: d, h: list<h>, u_X: int}
@deriving({accessors: accessors}) and e = {d: d}

let v = d({d: d_int(3)})

let g = u_X

let h = list{d_empty, d_int(3), d_tuple(3, "hgo"), d_tweak((3, "hgo")), newContent("3")}

@deriving(accessors) type hh = Xx(int)

@deriving(accessors) type xx = {"x": int}

@deriving(accessors) type t = A(int => int)

let f = a(x => x)
