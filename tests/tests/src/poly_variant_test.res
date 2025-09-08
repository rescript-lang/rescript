let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{
      (loc ++ (" id " ++ Js.Int.toString(test_id.contents)), _ => Mt.Eq(x, y)),
      ...suites.contents,
    }
}

%%raw(`
function hey_string (option){
  switch(option){
  case "on_closed" : 
  case "on_open" : 
  case "in" : return option
  default : throw Error ("impossible")
 }
}
function hey_int (option){
  switch (option){
   case 0 : 
   case 3 : 
   case 4 : 
   case 5:
   case 6 : return option
   default : throw Error("impossible")
  }
 }
`)

/** when marshall, make sure location does not matter */
type u = [
  | #on_closed
  | #on_open
  | #in_
]
/* [@as "in"] TODO: warning test */
/* indeed we have a warning here */
/* TODO: add warning test
 */

/** when marshall, make sure location does not matter */
@val
external test_string_type: (~flag: @string [#on_closed | #on_open | @as("in") #in_]) => string =
  "hey_string"

@val
external test_int_type: @int
[
  | #on_closed
  | @as(3) #on_open
  | #in_
  | @as(5) #again
  | #hey
] => int = "hey_int"

let uu = [
  test_string_type(~flag=#on_open),
  test_string_type(~flag=#on_closed),
  test_string_type(~flag=#in_),
]

let vv = [test_int_type(#on_open), test_int_type(#on_closed), test_int_type(#in_)]

let () = {
  eq(__LOC__, vv, [3, 0, 4])
  eq(__LOC__, (test_int_type(#again), test_int_type(#hey)), (5, 6))
  eq(__LOC__, uu, ["on_open", "on_closed", "in"])
}

let option = #on_closed

let v = test_string_type(~flag=option)

let ff = h => test_string_type(~flag=h)

let xx = test_string_type(~flag=#in_)

type readline
@send
external on: (
  readline,
  @string
  [
    | #line(string => unit)
    | #close(unit => unit)
  ],
) => unit = "on"

let register = readline => {
  on(readline, #line(s => Js.log(s)))
  on(readline, #close(() => Js.log("finished")))
}

/* external on : */
/* ([ `line of (string -> unit [@bs]) */
/* | `close of (unit -> unit [@bs])] */
/* [@string]) -> */
/* readline -> readline  = */
/* "on" [@@send] */
@send
external on2: (
  readline,
  @string
  [
    | #line(string => unit)
    | #close(unit => unit)
  ],
) => unit = "on2"

@module("fs") external readFileSync: (string, [#utf8 | #ascii]) => string = "readFileSync"

let read = name => readFileSync(name, #utf8)

module N = {
  @module("fs") external readFileSync: (string, [#utf8 | #ascii]) => string = "readFileSync"
  let read = name => readFileSync(name, #utf8)
}
/**
let register readline = 
  readline 
  |> on (`line begin fun [@bs] s -> Js.log s end)
  |> on (`close begin fun [@bs] () -> Js.log \"finished\" end)

{[
let register readline = 
  on (`line begin fun [@bs] s -> Js.log s end) readline; 
  on (`close begin fun [@bs] () -> Js.log \"finished\" end) readline

]}
*/
let readN = N.read

/**
let register readline = 
  readline 
  |> on (`line begin fun [@bs] s -> Js.log s end)
  |> on (`close begin fun [@bs] () -> Js.log \"finished\" end)

{[
let register readline = 
  on (`line begin fun [@bs] s -> Js.log s end) readline; 
  on (`close begin fun [@bs] () -> Js.log \"finished\" end) readline

]}
*/
let test = (readline, x) => on(readline, x)

let p_is_int_test = x =>
  switch x {
  | #a => 2
  | #b(_) => 3
  }

let u = #b(2)

let () = {
  eq(__LOC__, 2, p_is_int_test(#a))
  eq(__LOC__, 3, p_is_int_test(u))
}

let hey = x =>
  switch x {
  | (#a
    | #b
    | #d
    | #c) as u =>
    Js.log("u")
    Js.log(u)

  | (#e
    | #f
    | #h) as v =>
    Js.log("v")
    Js.log(v)
  }

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
