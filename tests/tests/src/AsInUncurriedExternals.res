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
