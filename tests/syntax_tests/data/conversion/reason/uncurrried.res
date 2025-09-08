// ok
let updateBriefletNarrative = (. updateObj) => Js.log("patented merge algorithm goes here")

// this is a bug in Reason, the . will be parsed wrong and disappear.
/* updateBriefletNarrative(. briefletNarrativeUpdateObj); */

// this is a bug in Reason, the . will be parsed wrong and disappear.
/* foo(. 3); */

module D = {
  // this is a bug in Reason, the . will be parsed wrong and disappear.
  /* foo(. 3); */
}

// ok
let x = foo(. 3)

let x = {
  let a = 3
  // ok
  foo(. a)
}

let x = {
  // ok
  let f = (. a, b) => apply(. a + b)
  let a = 3
  // ok
  foo(. a)
  // ok
  f(. 2, 2)
}

// ok
let () = switch something(. x, y) {
| None =>
  // ok
  log(. a, b)
| Some(_) =>
  let a = 1
  // ok
  log(. a, 2)
}

let () = {
  // ok
  let dontDoThisAhome = (. a, b, . c, d, . e, f) => a + b + c + d + e + f
  // ok
  dontDoThisAhome(. a, b)(. c, d)(. e, f)
}

let _ = library.getBalance(. account)->Promise.Js.catch(_ => Promise.resolved(None))

let _ =
  library.getBalance(. account)
  ->Promise.Js.catch(_ => Promise.resolved(None))
  ->Promise.get(newBalance =>
    dispatch(
      LoadAddress(
        account,
        newBalance->Belt.Option.flatMap(balance => Eth.make(balance.toString(.))),
      ),
    )
  )
