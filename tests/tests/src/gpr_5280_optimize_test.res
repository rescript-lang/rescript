@@config({
  flags: [
    "-w",
    "@A",
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose" */
    /* "-dparsetree"; */
    /* "-dsource"; */
    /* "-bs-no-builtin-ppx"; */
  ],
})

type color = Orange | Color(string)

let a = Color("#ffff")

let c = switch a {
| Orange => "orange"
| Color("#ffff") => "white"
| Color(s) => s
}
