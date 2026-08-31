val decode_js_escapes : string -> string option
(** Decode the escape sequences in a JavaScript string-literal body into its
    semantic UTF-8 value. Returns [None] for malformed input or unpaired
    UTF-16 surrogates. *)
