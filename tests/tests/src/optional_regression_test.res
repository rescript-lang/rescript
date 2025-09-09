open Mocha
open Test_utils

@deriving(abstract)
type testDeriving = {
  @optional s: string,
  @optional b: bool,
  @optional i: int,
}

let make = (~s=?, ~b=?, ~i=?, ()) => testDeriving(~s?, ~b?, ~i?, ())

let hh = make(~s="", ~b=false, ~i=0, ())

describe(__MODULE__, () => {
  test("optional regression test", () => {
    eq(__LOC__, hh->sGet, Some(""))
    eq(__LOC__, hh->bGet, Some(false))
    eq(__LOC__, hh->iGet, Some(0))
  })
})
