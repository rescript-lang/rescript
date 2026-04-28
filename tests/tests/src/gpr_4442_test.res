open Mocha
open Test_utils

let u = %raw(`function fib(n){
  if(n===0||n==1){
    return 1
  }
  return fib(n-1) + fib(n-2)
}`)

describe(__FILE__, () => {
  test("gpr_4442_test", () => {
    eq(__LOC__, u(2), 2)
    eq(__LOC__, u(3), 3)
  })
})
