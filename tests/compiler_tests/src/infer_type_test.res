type hh = {"hi": int, "lo": int, "width": option<int>}

@obj external mk_config: (~hi: int, ~lo: int, ~width: int=?, unit) => hh = ""

let hh = mk_config(~hi=30, ~lo=20, ())

/* let v = hh##widt */
let v = hh["width"]

@obj external config: (~hi: int, ~lo: int, ~width: int=?, unit) => hh = ""

let v = config(~hi=32, ~lo=3, ())

let vv = config(~lo=3, ~width=3, ~hi=3, ())

let u = v["hi"]
/* val u:  int type */
let uu = v["width"]
/* val uu : option<int> */
/* compile error
let uu = v##xx
*/
