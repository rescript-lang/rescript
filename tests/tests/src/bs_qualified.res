@module(("x", "X")) external xx: string => unit = "xx"

type param

@scope("commands") @module("vscode") @variadic
external executeCommands: (string, array<param>) => unit = "executeCommands"

@scope("process") @val external env: dict<string> = "env"

let f = (a, b, c) => {
  executeCommands("hi", [a, b, c])
  env
}

@module("z") @scope(("a0", "a1", "a2")) external hi: string = "hi"
@val @scope(("a0", "a1", "a2")) external ho: string = "ho"
@val @scope("Math") external imul: (int, int) => int = "imul"
let f2 = () => (hi, ho, imul(1, 2))

type buffer
@new @scope("global") external makeBuffer: int => buffer = "Buffer"

@new @scope(("global", "a0", "a1", "a2")) external makeBuffer1: int => buffer = "Buffer"

@new @scope(("global", "a0", "a1", "a2")) @module(("X", "ZZ"))
external makeBuffer2: int => buffer = "Buffer"

@new @scope(("global", "a0", "a1", "a2")) @module(("X", "Z"))
external makeBuffer3: int => buffer = "makeBuffer3"

@scope("Math") @val external max: (float, float) => float = "max"
/* TODO: `@val` is not necessary, by default is good?
 */

type t
@scope("mat4") @module("gl-matrix") external create: unit => t = "create"

/* external scope_f : t -> int = "" [@@get] [@@scope "hi"] */

@get_index @scope("a0") external getMockFn1: (t, int) => string = ""

@get_index @scope(("a0", "a1")) external getMockFn2: (t, int) => string = ""

@get_index @scope(("a0", "a1", "a2")) external getMockFn3: (t, int) => string = ""

@set_index @scope("a0") external setMocFn1: (t, int, string) => unit = ""

@set_index @scope(("a0", "a1")) external setMocFn2: (t, int, string) => unit = ""

@set_index @scope(("a0", "a1", "a2")) external setMocFn3: (t, int, string) => unit = ""

@get @scope("a0") external getX1: t => int = "getX1"

@get @scope(("a0", "a1")) external getX2: t => int = "getX2"

@get @scope(("a0", "a1", "a2")) external getX3: t => int = "getX3"

@set @scope("a0") external setX1: (t, int) => unit = "setX1"

@set @scope(("a0", "a1")) external setX2: (t, int) => unit = "setX2"

@set @scope(("a0", "a1", "a2")) external setX3: (t, int) => unit = "setX3"

@set @scope(("a0-hi", "a1", "a2")) external setXWeird3: (t, int) => unit = "setXWeird3"

@send @scope("a0") external send1: (t, int) => unit = "send1"
@send @scope(("a0", "a1")) external send2: (t, int) => unit = "send2"
@send @scope(("a0", "a1")) external send3: (t, int) => unit = "send3"

@send @scope("a0") external psend1: (t, int) => unit = "psend1"
@send @scope(("a0", "a1")) external psend2: (t, int) => unit = "psend2"
@send @scope(("a0", "a1")) external psend3: (t, int) => unit = "psend3"

let f3 = x => {
  ignore(makeBuffer(20))
  ignore(makeBuffer1(20))
  ignore(makeBuffer2(100))
  ignore(makeBuffer3(20))
  Console.log(max(1.0, 2.0))
  /* Console.log @@ scope_f x ; */
  Console.log(getMockFn1(x, 0))
  Console.log(getMockFn2(x, 0))
  Console.log(getMockFn3(x, 0))
  setMocFn1(x, 0, "x")
  setMocFn2(x, 0, "x")
  setMocFn3(x, 0, "x")
  Console.log(getX1(x))
  Console.log(getX2(x))
  Console.log(getX3(x))

  setX1(x, 0)
  setX2(x, 0)
  setX3(x, 0)
  setXWeird3(x, 0)

  send1(x, 0)
  send2(x, 0)
  send3(x, 0)
  x->psend1(0)
  x->psend2(0)
  x->psend3(0)
  create()
}
