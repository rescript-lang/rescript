let f = x => x["length"] +. x["width"]
let i = () => ()

let h = (x): unit => {
  x["height"] = 3
  i(x["width"] = 3)
}

let chain = x => x["element"]["length"] + x["element"]["length"]

/* current error message : 
   Error: '##' is not a valid value identifier.
*/
/* let syntax_error x = */
/* x ## _set_height 3 3 */

let g = x => {
  let () = x["method1"](3)
  x["method2"](3, 3)
}
