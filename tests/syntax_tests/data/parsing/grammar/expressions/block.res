let b = {
  module Array = Belt.Array
  [1, 2]
  ->Array.map(x => x + 1)
  ->Js.log
}

let b = {
  open Belt.Array
  [1, 2]
  ->map(x => x + 1)
  ->Js.log
}

let b = {
  exception QuitEarly
  throw(QuitEarly)
}

// let-bindings
let b = {
  let a = 1
  let b = 2
  a + b
}

let b = {
  let _ = sideEffect();
}

let b = {
  let _ = sideEffect() // no semi
}

let b = {
  a()
  b()
  c()
}

let b = {
  a()
  b()
  let a = 1
  f(a)
}

let b = {
  let a = 1
  let b = 2
}

// parse semicolons
let b = {
  module Array = Belt.Array;
  open Array;
  exception Terminate(int);
  let a = 1;
  let b = 2;
  sideEffect();
  let x = (1 + 2)->(x => x + 1);
  throw(Terminate(x)); 
}

let b = {
  f()
  g()
  h()
  let arr = [1, 2, 3]
}

let res = {
  let a = "a starts out as"
  {
    print_string(a)
    let a = 20
    print_int(a)
  }
  print_string(a)
}

let res = {
  let a = "first its a string"
  let a = 20
  print_int(a)
  print_int(a)
  print_int(a)
}

let res = {
  let a = "a is always a string"
  print_string(a)
  let b = 30
  print_int(b)
}

let nestedLet = {
  let _ = 1
  ()
}

let nestedLet = {
  let _ = 1
  2
}

let init = () => {
  foo(
    1===1
  )
  %assert(1 === 2)
}

let init = () => {
  %assert(1 === 2)
  foo(
    1===1
  )
  %assert(1 === 2)
}

let f = () => {
  let x = 1
  _ => ()
}

let reifyStyle = (type a, x: 'a): (style<a>, a) => {
  module Internal = {
    type rec constructor
    @val external canvasGradient: constructor = "CanvasGradient" /* internal */
    @val external canvasPattern: constructor = "CanvasPattern" /* internal */
    let instanceOf = (
      %raw(`function(x,y) {return +(x instanceof y)}`): ('a, constructor) => bool
    ) /* internal */
  }

  // This not a module application
  (
    if Js.typeof(x) == "string" {
      Obj.magic(String)
    } else if Internal.instanceOf(x, Internal.canvasGradient) {
      Obj.magic(Gradient)
    } else if Internal.instanceOf(x, Internal.canvasPattern) {
      Obj.magic(Pattern)
    } else {
      throw(
        Invalid_argument(
          "Unknown canvas style kind. Known values are: String, CanvasGradient, CanvasPattern",
        ),
      )
    },
    Obj.magic(x),
  )
}

let calc_fps = (t0, t1) => {
  let delta = (t1 -. t0) /. 1000.
  1. /. delta
}
