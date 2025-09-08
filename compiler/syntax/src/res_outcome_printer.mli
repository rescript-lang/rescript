(* For the curious: the outcome printer is a printer to print data
 * from the outcometree.mli file in the ocaml compiler.
 * The outcome tree is used by:
 *  - ocaml's toplevel/repl, print results/errors
 *  - super errors, print nice errors
 *  - editor tooling, e.g. show type on hover
 *
 * In general it represent messages to show results or errors to the user. *)

val parenthesized_ident : string -> bool [@@live]

val setup : unit lazy_t [@@live]

(* Needed for e.g. the playground to print typedtree data *)
val print_out_type_doc : Outcometree.out_type -> Res_doc.t [@@live]
val print_out_sig_item_doc :
  ?print_name_as_is:bool -> Outcometree.out_sig_item -> Res_doc.t
[@@live]
