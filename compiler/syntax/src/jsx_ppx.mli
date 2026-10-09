(* The built-in JSX transform: rewrites the parsetree's JSX elements and
   components as calls to the configured JSX module (see ../JSX.md). Version 4,
   from [jsx_version] or a [@jsxConfig] attribute, is the only transform. *)

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
