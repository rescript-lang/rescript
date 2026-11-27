@@uncurried

@obj
external makeOptions: (
  ~objectMode: @as(json`false`) _,
  ~name: string,
  ~someOther: @as(json`true`) _,
  unit,
) => int = ""

let mo = makeOptions

let options = mo(~name="foo", ())

let shouldNotFail: (~objectMode: _, ~name: string) => int = (~objectMode, ~name) => 3

@scope("somescope")
external constantArgOnly: @as(json`{foo:true}`) _ => string = "somefn"

let x = constantArgOnly()

@scope("somescope")
external semanticStringArg: @as(`\x61\u0062`) _ => string = "stringfn"

let y = semanticStringArg()

// Property accesses whose arguments are all constants are called with the
// synthetic unit argument, which is not a declared unit argument (#9339).
@get
external constantGet: @as(json`{"x": 1}`) _ => int = "x"

let getX = constantGet()

@get_index
external constantGetIndex: (@as(json`{"k": 2}`) _, @as("k") _) => int = ""

let getK = constantGetIndex()
