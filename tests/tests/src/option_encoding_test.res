@@config({
  flags: [
    "-w",
    "@A",
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose"; */
    /* "-dparsetree"; */
    /* "-dsource"; */
  ],
})

module N = {
  type t<'a> = option<'a> =
    | None
    | Some('a)
}

let u = {
  open N
  (None, Some(3))
}

let h = N.None
