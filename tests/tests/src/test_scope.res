@val("x") external f: int => int = ""
/* [@@scope "u"] [@@scope "uuu"] */
@val("x") external ff: int => int = ""
let h = f(3)
let hh = ff(3)
let f = (x, y) => x ++ y
