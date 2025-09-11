open Mocha
open Test_utils

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
let option = #on_closed
let v = test_string_type(~flag=option)
let p_is_int_test = x =>
  switch x {
  | #a => 2
  | #b(_) => 3
  }
let u = #b(2)
type t = [#"🚀" | #"🔥"]

describe(__MODULE__, () => {
  test("poly variant string marshalling", () => {
    eq(__LOC__, vv, [3, 0, 4])
    eq(__LOC__, (test_int_type(#again), test_int_type(#hey)), (5, 6))
    eq(__LOC__, uu, ["on_open", "on_closed", "in"])
  })

  test("poly variant function application", () => {
    eq(__LOC__, v, "on_closed")
  })

  test("poly variant pattern matching", () => {
    eq(__LOC__, 2, p_is_int_test(#a))
    eq(__LOC__, 3, p_is_int_test(u))
  })

  test("emoji poly variant conversion", () => {
    eq(__LOC__, "🚀", (#"🚀": t :> string))
    eq(__LOC__, "🔥", (#"🔥": t :> string))
  })
})
