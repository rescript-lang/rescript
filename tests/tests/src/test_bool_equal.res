let bool_equal = (x, y) =>
  switch (x, y) {
  | (true, true)
  | (false, false) => true
  | (false, true)
  | (true, false) => false
  }

/* TODO: 
   There is a problem for the ffi,
   for boolean function, we might do 
   [x !==0 ]
   which return the same value whether user input [true] or [false]
   u.bool_equal(false,true) */
let assertions = () => {
  assert(bool_equal(true, true))
  assert(bool_equal(false, false))
  assert(!bool_equal(true, false))
  assert(!bool_equal(false, true))
  assert(true == true)
  assert(false == false)
  assert(!(true == false))
  assert(!(false == true))
}

let f0 = x =>
  if x == true {
    1
  } else {
    2
  }
let f1 = x =>
  if !(x == true) {
    1
  } else {
    2
  }

let f2 = x =>
  if x == true {
    1
  } else {
    2
  }
let f3 = x =>
  if x == false {
    1
  } else {
    2
  }

let f4 = x =>
  if !(x == true) {
    1
  } else {
    2
  }

let f5 = x =>
  switch x {
  | list{} => 1
  | _ => 2
  }

let f6 = x =>
  if x == list{} {
    1
  } else {
    2
  }

let f7 = x =>
  if Belt.Array.length(x) > 0 {
    1
  } else {
    2
  }

let f8 = x =>
  if Belt.Array.length(x) >= 0 {
    1
  } else {
    2
  }
