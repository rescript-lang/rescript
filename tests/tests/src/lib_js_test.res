open Mocha
open Test_utils

@val external to_json_string: 'a => string = "JSON.stringify"

let () = {
  Js.log(to_json_string(list{1, 2, 3}))
  Js.log("hey")
}
@val external of_any: 'a => string = "String"

describe(__MODULE__, () => {
  test("anything_to_string", () => eq(__LOC__, "3", of_any(3)))
  /* in js, array is printed as {[ 1,2 ]} without brackets */
  /* "array_to_string", (fun _ -> Eq("[0]", Js.anything_to_string [|0|])) */
})
