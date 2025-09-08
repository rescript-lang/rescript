type t

@module("some-es6-module") external default: t = "default"
@module("some-es6-module") external default2: t = "default2"
let (default, default2) = (default, default2)

@module("./ome-es6-module") external oefault: t = "default"
@module("./ome-es6-module") external oefault2: t = "default2"
let (oefault, oefault2) = (oefault, oefault2)

type window
@val @module("vscode") external window: window = "window"

let window = window
let mk = (window, default) => {"window": window, "default": default}
type t_ = {window: int, default: int}

let mk2 = (window, default) => list{{window, default}}

let des = v => {"window": v["window"], "default": v["default"]}

let case = 3

let test = {"case": case, "window": 3}

@send external \"switch": window => string = "switch"

let u = () => \"switch"(window)
