val is_valid_utf8 : string -> bool
(** Whether a string consists entirely of valid UTF-8 scalar sequences. *)

val decode_js_escapes : string -> string option
(** Decode the escape sequences in a JavaScript string-literal body into its
    semantic UTF-8 value. Returns [None] for malformed input or unpaired
    UTF-16 surrogates. *)

val decode_js_template_escapes : string -> string option
(** Decode the escape sequences in a JavaScript template segment into its
    semantic UTF-8 value. Literal CR and CRLF line endings are normalized to LF
    as required by JavaScript template-literal semantics. *)

val encode_js_string : string -> string
(** Encode a semantic UTF-8 string as a canonical JavaScript string-literal
    body. *)

val encode_js_template : string -> string
(** Encode a semantic UTF-8 string as a canonical JavaScript template-segment
    body. *)

val encode_char_source : int -> string
(** Encode an integer as a canonical character-literal body. Non-scalar values
    use braced Unicode escape spelling so compiler-generated ghost patterns and
    legacy PPX output remain printable. *)

val utf16_length : string -> int
(** Return the number of UTF-16 code units in a semantic UTF-8 string, matching
    JavaScript's [String.length]. *)

val code_point_at_utf16_index : string -> int -> int option
(** Return the result of JavaScript's [String.codePointAt] for a UTF-16 code
    unit index into a semantic UTF-8 string. *)
