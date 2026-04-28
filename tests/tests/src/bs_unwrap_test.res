@val
external log1: @unwrap
[
  | #Pair(string, int)
  | #Int(int)
  | #String(string)
] => unit = "console.log"

let _ = log1(#Pair("hello world", 1))
let _ = log1(#Int(1337))
let _ = log1(#String("hello world"))

let arg_string = #String("hi runtime")
let _ = log1(arg_string)

let arg_pair = #Pair("hi", 1)
let _ = log1(arg_pair)

@val
external log2: @unwrap
[
  | #Unit(unit)
] => unit = "console.log"

let _ = log2(#Unit())

@val
external log3: (
  ~req: @unwrap
  [
    | #String(string)
    | #Int(int)
  ],
  ~opt: @unwrap
  [
    | #String(string)
    | #Bool(bool)
  ]=?,
  unit,
) => unit = "console.log"

let _ = log3(~req=#Int(1), ())
let _ = log3(~req=#Int(2), ~opt=#String("hi"), ())
let _ = log3(~req=#Int(3), ~opt=?Some(#String("hi")), ())
let _ = log3(~req=#Int(4), ~opt=?None, ())

/* static optional arg as variable */
let some_arg = Some(#Bool(true))
let _ = log3(~req=#Int(5), ~opt=?some_arg, ())

let none_arg = None
let _ = log3(~req=#Int(6), ~opt=?none_arg, ())

/* static optional arg as complex side-effectful expression */
let _ = log3(
  ~req=#Int(7),
  ~opt=?(
    _ => {
      Console.log("trace")
      None
    }
  )(),
  (),
)

/* expose the external as a function in generated module */
let dyn_log3 = log3

/* call the dynamically reassigned external */
let _ = dyn_log3(~req=#Int(8), ~opt=#Bool(true), ())

@val
external log4: @unwrap
[
  | #String(string)
  | #Options({"foo": int})
] => unit = "console.log"

/* Make sure [@unwrap] plays nicely with [%obj] */
let _ = log4(#String("foo"))
let _ = log4(#Options({"foo": 1}))

let dyn_log4 = log4
let _ = dyn_log4(#Options({"foo": 2}))

let f = x => dyn_log4(x)

@val external log5: (~h: @unwrap [#A(int) | #B(string)]=?, int) => unit = "console.log"

let ff0 = (x, p) => log5(~h=?x, p)

let ff1 = (x, p) => log5(~h=?x(), p)

@obj external ff: (~a: int, ~b: int, ~x: unit) => _ = ""

let test00 = () => ff(~a=1, ~b=2, ~x=())
