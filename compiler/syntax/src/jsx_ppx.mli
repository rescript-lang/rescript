(*
  This is the module that handles turning Reason JSX' agnostic function call into
  a ReasonReact-specific function call. Aka, this is a macro, using OCaml's ppx
  facilities; https://whitequark.org/blog/2014/04/16/a-guide-to-extension-
  points-in-ocaml/
  You wouldn't use this file directly; it's used by ReScript's
  rescript.json. Specifically, there's a field called `react-jsx` inside the
  field `reason`, which enables this ppx through some internal call in bsb
*)

val rewrite_implementation :
  jsx_version:int ->
  jsx_module:string ->
  Parsetree.structure ->
  Parsetree.structure

val rewrite_signature :
  jsx_version:int ->
  jsx_module:string ->
  Parsetree.signature ->
  Parsetree.signature
