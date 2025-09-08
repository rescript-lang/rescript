@obj external mk_config: (~hi: int, ~lo: int, ~width: int=?, unit) => _ = ""

type hh = {"hi": int, "lo": int, "width": Js.undefined<int>}
let hh = mk_config(~hi=30, ~lo=20, ())

/* let v = hh##widt */
let v = hh["width"]

@obj external config: (~hi: int, ~lo: int, ~width: int=?, unit) => _ = ""

let v = config(~hi=32, ~lo=3, ())

let vv = config(~lo=3, ~width=3, ~hi=3, ())

let u = v["hi"]
/* val u:  int type */
let uu = v["width"]
/* val uu :  int Js.undefined */
/* compile error
let uu = v##xx
*/
