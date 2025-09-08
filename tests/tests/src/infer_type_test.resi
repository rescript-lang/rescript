@obj external mk_config: (~hi: int, ~lo: int, ~width: int=?, unit) => _ = ""

type hh = {"hi": int, "lo": int, "width": Js.undefined<int>}
let hh: hh

let v: {"hi": int, "lo": int, "width": Js.undefined<int>}

let vv: {"hi": int, "lo": int, "width": Js.undefined<int>}

let u: int
let uu: Js.undefined<int>
