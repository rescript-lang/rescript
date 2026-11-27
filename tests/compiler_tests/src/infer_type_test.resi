type hh = {"hi": int, "lo": int, "width": option<int>}

@obj external mk_config: (~hi: int, ~lo: int, ~width: int=?, unit) => hh = ""

let hh: hh

let v: {"hi": int, "lo": int, "width": option<int>}

let vv: {"hi": int, "lo": int, "width": option<int>}

let u: int
let uu: option<int>
