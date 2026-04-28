open Mocha
open Test_utils

/* module X =  Map.Make(String) */

module Y0 = Functor_def.Make(Functor_inst)

module Y1 = Functor_def.Make(Functor_inst)

describe(__MODULE__, () => {
  test("functor_app_test", () => {
    eq(__LOC__, Y0.h(1, 2), 4)
    eq(__LOC__, Y1.h(2, 3), 6)

    let v = Functor_def.return()

    eq(__LOC__, v, 2)
  })
})
