let h1: {.."p": 'a} => 'a
let h3: {.."hi": (int, int) => 'a} => 'a
let g5: {..@set "hi": int} => unit
let h5: {..@set "hi": int} => unit
/* The inferred type is 
   val h5 : < hi#= : (int -> unit [@meth]); .. > -> unit
   We should propose the rescript syntax:
   { mutable "hi" : int  }
*/
let h6: {.."p": 'a} => 'a
let h7: {.."m": (int, int) => 'a} => 'a
let h8: {.."hi": (int, int) => 'a} => 'a

let chain_g: {"x": {"y": {"z": int}}} => int
