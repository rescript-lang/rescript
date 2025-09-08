@@config({
  flags: [
    /* "-drawlambda"; */
    /* "-dsource"; */
    /* "-bs-diagnose" */
  ],
})

let h1 = u => u["p"]

let h3 = u => {
  let f = u["hi"]
  f(1, 2)
}

let g5 = u => u["hi"] = 3
let h5 = u => u["hi"] = 3
/* assignment 
 This seems to be wrong in rescript syntax
  u["hi"] = 3 
*/

let h6 = u => u["p"]

let h7 = u => u["m"](1, 2)

let h8 = u => {
  let f = u["hi"]
  f(1, 2)
}

let chain_f = h => h["x"]["y"]["z"]

let chain_g = h => h["x"]["y"]["z"]
