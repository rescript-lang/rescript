type config1_expect = {"v": int}
@obj external config1: (~stdio: %raw(`"inherit"`), ~v: int, unit) => _ = ""

let v1: config1_expect = config1(~v=3, ())

type config2_expect = {"v": int}

@obj external config2: (~stdio: %raw("1"), ~v: int, unit) => _ = ""
let v2: config2_expect = config2(~v=2, ())

@val external on_exit: (%raw(`"exit"`), int => string) => unit = "process.on"

let () = on_exit(exit_code => Int.toString(exit_code))

@val external on_exit_int: (%raw("1"), int => unit) => unit = "process.on"

let () = on_exit_int(_ => ())

@val external on_exit3: (int => string, %raw(`"exit"`)) => unit = "process.on"

let () = on_exit3(i => Int.toString(i))

@val external on_exit4: (int => string, %raw("1")) => unit = "process.on"

let () = on_exit4(i => Int.toString(i))

@val @variadic external on_exit_slice: (int, %raw("3"), %raw(`"xxx"`), array<string>) => unit = "xx"

let () = on_exit_slice(3, ["a", "b"])

type t

@send external on_exit_slice1: (t, int, array<int>) => unit = "xx"

@send external on_exit_slice2: (t, int, %raw("3"), %raw(`"xxx"`), array<int>) => unit = "xx"

@send @variadic
external on_exit_slice3: (t, int, %raw("3"), %raw(`"xxx"`), array<int>) => unit = "xx"

@send @variadic
external on_exit_slice4: (
  t,
  int,
  %raw("3"),
  %raw(`"xxx"`),
  @int [#a | #b | #c],
  [#a | #b | #c],
  array<int>,
) => unit = "xx"

@send @variadic
external on_exit_slice5: (
  t,
  int,
  %raw("3"),
  %raw("true"),
  %raw("false"),
  %raw(`"你好"`),
  %raw(`["你好",1,2,3]`),
  %raw(`[{"arr":["你好",1,2,3],"encoding":"utf8"}]`),
  %raw(`[{"arr":["你好",1,2,3],"encoding":"utf8"}]`),
  %raw(`"xxx"`),
  @int [#a | #b | #c],
  %raw(`"yyy"`),
  [#a | #b | #c],
  array<int>,
) => unit = "xx"

/**
 TODO: @send conflicts with @val: better error message
*/
let f = (x: t) => {
  x->on_exit_slice1(__LINE__, [1, 2, 3])
  x->on_exit_slice2(__LINE__, [1, 2, 3])
  x->on_exit_slice3(__LINE__, [1, 2, 3])
  x->on_exit_slice4(__LINE__, #a, #b, [1, 2, 3, 4, 5])
  x->on_exit_slice5(__LINE__, #a, #b, [1, 2, 3, 4, 5])
}

@val external process_on_exit: (%raw(`"exit"`), int => unit) => unit = "process.on"

let () = process_on_exit(exit_code => Console.log2("error code: %d", exit_code))

type process

@send external on_exit: (process, %raw(`"exit"`), int => unit) => unit = "on"
let register = (p: process) => p->on_exit(i => Console.log(i))

@obj external io_config: (~stdio: %raw(`"inherit"`), ~cwd: string, unit) => _ = ""

let config = io_config(~cwd=".", ())
