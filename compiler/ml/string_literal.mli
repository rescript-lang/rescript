type string_kind
type template_kind
type 'kind payload

type string_literal = string_kind payload
(** A validated ordinary JavaScript string body and its semantic value. *)

type template_segment = template_kind payload
(** A validated JavaScript template segment and its semantic value. *)

val source : 'kind payload -> string
val semantic : 'kind payload -> string

val is_valid_utf8 : string -> bool
(** Whether a string consists entirely of valid UTF-8 scalar sequences. *)

val replace_invalid_utf8 : string -> string
(** Replace each malformed UTF-8 sequence with the Unicode replacement
    character. *)

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

val string_from_source : string -> string_literal option
(** Validate and decode an ordinary string-literal body. *)

val string_from_semantic : string -> string_literal
(** Construct an ordinary string payload with canonical source spelling. *)

val invalid_string_for_recovery : string -> string_literal
(** Preserve the source of an invalid parser input after its diagnostic has
    been recorded. Its placeholder semantic value is the empty string. *)

val template_from_source : string -> template_segment option
(** Validate and decode one template segment. *)

val template_from_semantic : string -> template_segment
(** Construct a template segment with canonical source spelling. *)

val string_as_template : string_literal -> template_segment
(** Convert an ordinary string payload to a template segment, preserving its
    source spelling when that spelling has the same template semantics. *)

val concat_template : template_segment list -> template_segment
(** Concatenate template segments, preserving their combined source spelling
    when it still decodes to the combined semantic value and otherwise using a
    canonical spelling. *)

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
