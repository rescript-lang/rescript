@@config({
  flags: [
    "-w",
    "@A",
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose"; */
    /* "-dparsetree"; */
    /* "-dsource"; */
    /* "-bs-no-builtin-ppx"; */
  ],
})
/*
let h1 = fun [@bs]() -> 1 
let h0 f a b c = f a b c [@bs]
let h =
let f x ~y = x + y in
            f ~y:3 */

/* let f x ~y = x + y

;; Js.log (f 2 3) */
