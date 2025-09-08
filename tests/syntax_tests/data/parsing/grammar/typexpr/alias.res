type t = string as 's
type t = _ as 'underscore
type t = (parenthesizedType) as 'parens
type t = int => unit as 'arrow
type t = int => (unit as 'unitAlias)
type t = (int, float) => unit as 'arrowAlias
type t = (int, float) => (unit as 'unitAlias)
type t = int as 'myNumber
type t = Mod.Sub.t as 'longidentAlias
type t = color<int as 'r, int as 'g, int as 'b> as 'rgb
type t = Color.t<int as 'r, int as 'g, int as 'b> as 'rgb
type t = %t as 'extension
type t = %t.typ as 'extension
type t = %ext.foo("raw") as 'extension
type tup = (int as 'x, int as 'y) as 'tupleAlias

let t: string as 's = ()
let t: _ as 'underscore = ()
let t: (parenthesizedType) as 'parens = ()
let t: int => unit as 'arrow = ()
let t: int => (unit as 'unitAlias) = ()
let t: (int, float) => unit as 'arrowAlias = ()
let t: (int, float) => (unit as 'unitAlias) = ()
let t: int as 'myNumber = ()
let t: Mod.Sub.t as 'longidentAlias = ()
let t: color<int as 'r, int as 'g, int as 'b> as 'rgb = ()
let t: Color.t<int as 'r, int as 'g, int as 'b> as 'rgb = ()
let t: %t as 'extension = ()
let t: %t.typ as 'extension = ()
let t: %ext.foo("raw") as 'extension = ()
let t: (int as 'x, int as 'y) as 'tupleAlias = ()
