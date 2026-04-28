/* let test _g = 
    on_exit_slice3 __LINE__ [|1;2;3|] 

*/

type t
@send @variadic
external on_exit_slice3: (t, int, ~h: @as(3) _, @as("xxx") _, array<int>) => unit = "xx"

let test = g => on_exit_slice3(g, __LINE__, [1, 2, 3])

@variadic @return({null_to_opt: null_to_opt}) @send
external hi: (int, array<int>) => option<int> = "hi"

let test_hi = x =>
  switch x->hi([1, 2, 3]) {
  | None => 1
  | Some(y) =>
    Console.log(y)
    2
  }

@variadic @return(nullable) @send external hi__2: (int, array<int>) => option<int> = "hi__2"

let test_hi__2 = x =>
  switch x->hi__2([]) {
  | None => 1
  | Some(_) => 2
  }

type id = int => int

@variadic @send external cb: (int, string, array<int>) => id = "cb"

type id2 = int => int
@variadic @send external cb2: (int, string, array<int>) => id2 = "cb2"

let test_cb = x => {
  ignore((x->cb("hI", [1, 2, 3]))(3))
  ignore(cb(x, "hI", [1, 2, 3])(3))
  cb2(x, "hI", [1, 2, 3])(3)
}

type u = int => int
@val external v: u = "v"

let f = x => ignore(v(x))

@val external fff0: (int, int, @as(json`[undefined,undefined]`) _) => int = "say"

let testUndefined = () => fff0(1, 2)
