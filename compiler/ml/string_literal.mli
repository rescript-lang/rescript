val decode_js_escapes : string -> string option
(** Decode the escape sequences in a JavaScript string-literal body into its
    semantic UTF-8 value. Returns [None] for malformed input or unpaired
    UTF-16 surrogates. *)

val runtime_value : string -> string option -> string
(** Return the runtime value represented by a typed string constant.

    Ordinary quoted literals and backquoted literals still contain JavaScript
    escape sequences at this point in the pipeline. Other constants already
    contain their semantic value. *)

val compare : string * string option -> string * string option -> int
(** Compare typed string constants by runtime value rather than source
    encoding. *)
