open Mocha
open Test_utils

type response = {status: int}

let isRetriable = error =>
  switch error {
  | {status: 502 | 503 | 504} => true
  | _ => false
  }

describe(__MODULE__, () => {
  test("int range on record field", () => {
    eq(
      __LOC__,
      [404, 500, 501, 502, 503, 504, 505]->Array.map(status => isRetriable({status: status})),
      [false, false, false, true, true, true, false],
    )
  })
})
