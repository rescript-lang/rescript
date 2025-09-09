/* ;; if bool_of_string "x" then "" else "" */

open Mocha
open Test_utils

let v = ref(3)

let update = () => {
  incr(v)
  true
}

if update() {
  ""
} else {
  ""
}->ignore

describe(__MODULE__, () => {
  test("gpr_1762 ref increment test", () => {
    eq(__LOC__, v.contents, 4)
  })
})
