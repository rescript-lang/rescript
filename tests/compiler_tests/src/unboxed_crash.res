@@config({
  flags: [
    "-w",
    "@A",
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-dparsetree"; */
    /* "-dsource"; */
    /* "-bs-no-builtin-ppx"; */
  ],
})

@unboxed type rec t = A(t => int)

let g = x =>
  switch x {
  | A(v) => v(x)
  }

let loop = g(A(g))
