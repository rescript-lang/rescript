@@uncurried

@obj
external makeOptions: (
  ~objectMode: %raw("false"),
  ~name: string,
  ~someOther: %raw("true"),
  unit,
) => int = ""

let mo = makeOptions

let options = mo(~name="foo", ())

let shouldNotFail: (~objectMode: _, ~name: string) => int = (~objectMode, ~name) => 3

@scope("somescope")
external constantArgOnly: (%raw(`{"foo":true}`)) => string = "somefn"

let x = constantArgOnly()

@scope("somescope")
external semanticStringArg: (%raw(`"ab"`)) => string = "stringfn"

let y = semanticStringArg()

type t = {"color": string}

@val
external unsafeAddStyle: (%raw("{}"), t, {..}) => t = "Object.assign"

let style = unsafeAddStyle({"color": "red"}, {"display": "flex"})
