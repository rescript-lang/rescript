let f = x =>
  switch x {
  | "abcd" => 0
  | "bcde" => 1
  | _ => assert(false)
  }

/* local variables: */
/* compile-command: "ocamlc -dlambda -c string_case.ml && js_of_ocaml string_case.cmo --pretty -o string_case.1.js" */
/* end: */
