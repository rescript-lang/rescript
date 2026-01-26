(* Sexp printer for Typedtree - used for parity testing between OCaml and Rust compilers *)

(** Print a typed structure as S-expression (without location information) *)
val print_typed_structure : Format.formatter -> Typedtree.structure -> unit

(** Print a typed structure as S-expression (with location information) *)
val print_typed_structure_with_locs : Format.formatter -> Typedtree.structure -> unit

(** Print a typed signature as S-expression (without location information) *)
val print_typed_signature : Format.formatter -> Typedtree.signature -> unit

(** Print a typed signature as S-expression (with location information) *)
val print_typed_signature_with_locs : Format.formatter -> Typedtree.signature -> unit
