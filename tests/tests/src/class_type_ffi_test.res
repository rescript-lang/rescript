/* TODO: create a special type 
   ['a Js.prop_set] for better error message
*/
let test_set = x => x["length__aux"] = 3

/* This type is generated on the fly -- in which case
  it can not be nominal
*/
let ff = (
  fn: ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11) => 'a12,
  a0,
  a1,
  a2,
  a3,
  a4,
  a5,
  a6,
  a7,
  a8,
  a9,
  a10,
  a11,
) => fn(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

let ff2 = (fn, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
  fn(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

/* Test [fn_run_method] */
let off2 = (o, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
  o["huge_method"](a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

/* Test [fn_mk] */
let mk_f = () =>
  (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
    a0(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
