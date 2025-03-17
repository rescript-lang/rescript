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

module Test_null = {
  let f1 = x =>
    switch Js.Null.toOption(x) {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }

  let f2 = x => {
    let u = Js.Null.toOption(x)
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f5 = (h, x) => {
    let u = Js.Null.toOption(h(32))
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f4 = (h, x) => {
    let u = Js.Null.toOption(h(32))
    let v = 32 + x
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, v)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f6 = (x, y) => x === y

  let f7 = x =>
    switch Some(x) {
    | None => None
    | Some(x) => x
    }

  /* can [from_opt x ]  generate [Some None] which has type ['a Js.opt Js.opt] ?
   No, if [x] is [null] then None else [Some x]
*/
  let f8 = (x: Js.Null.t<Js.Null.t<'a>>) =>
    switch Js.Null.toOption(x) {
    | Some(x) =>
      switch Js.Null.toOption(x) {
      | Some(_) => 0
      | None => 1
      }
    | None => 2
    }

  let u = f8(Js.Null.return(Js.Null.return(None)))

  let f9 = x => Js.Null.toOption(x)

  let f10 = x => x == Js.null

  let f11 = Js.Null.return(3) == Js.null
}

module Test_def = {
  let f1 = x =>
    switch Js.Undefined.toOption(x) {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }

  let f2 = x => {
    let u = Js.Undefined.toOption(x)
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f5 = (h, x) => {
    let u = Js.Undefined.toOption(h(32))
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f4 = (h, x) => {
    let u = Js.Undefined.toOption(h(32))
    let v = 32 + x
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, v)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f6 = (x, y) => x === y

  let f7 = x =>
    switch Some(x) {
    | None => None
    | Some(x) => x
    }

  /* can [from_def x ]  generate [Some None] which has type ['a Js.opt Js.opt] ?
     No, if [x] is [null] then None else [Some x]
 */
  let f8 = x =>
    switch Js.Undefined.toOption(x) {
    | Some(x) =>
      switch Js.Undefined.toOption(x) {
      | Some(_) => 0
      | None => 1
      }
    | None => 2
    }

  let u = f8(Js.Undefined.return(Js.Undefined.return(None)))

  let f9 = x => Js.Undefined.toOption(x)

  let f10 = x => x == Js.undefined
  let f11 = Js.Undefined.return(3) == Js.undefined
}

module Test_null_def = {
  open Js.Null_undefined
  let f1 = x =>
    switch toOption(x) {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }

  let f2 = x => {
    let u = toOption(x)
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f5 = (h, x) => {
    let u = toOption(h(32))
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, 2)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f4 = (h, x) => {
    let u = toOption(h(32))
    let v = 32 + x
    switch u {
    | None =>
      let sum = (x, y) => x + y
      sum(1, v)
    | Some(x) =>
      let sum = (x, y) => x + y
      sum(x, 1)
    }
  }

  let f6 = (x, y) => x === y

  let f7 = x =>
    switch Some(x) {
    | None => None
    | Some(x) => x
    }

  /* can [from_opt x ]  generate [Some None] which has type ['a Js.opt Js.opt] ?
     No, if [x] is [null] then None else [Some x]
 */
  let f8 = (x: t<t<'a>>) =>
    switch toOption(x) {
    | Some(x) =>
      switch toOption(x) {
      | Some(_) => 0
      | None => 1
      }
    | None => 2
    }

  let u = f8(return(return(None)))

  let f9 = x => toOption(x)

  let f10 = x => isNullable(x)

  let f11 = isNullable(return(3))
}

let () = {
  eq(__LOC__, Test_null_def.f1(Js.Null_undefined.return(0)), 1)
  eq(__LOC__, Test_null_def.f1(%raw("null")), 3)
  eq(__LOC__, Test_null_def.f1(%raw("undefined")), 3)

  eq(__LOC__, Test_null.f1(Js.Null.return(0)), 1)
  eq(__LOC__, Test_null.f1(%raw("null")), 3)

  eq(__LOC__, Test_def.f1(Js.Undefined.return(0)), 1)
  eq(__LOC__, Test_def.f1(%raw("undefined")), 3)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)

module Null_undefined_neq = {
  let a = null
  let b = undefined
  let res = a != b
}
