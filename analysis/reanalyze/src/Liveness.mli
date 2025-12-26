(** Forward liveness fixpoint computation.

    Computes the set of live declarations by forward propagation:
    1. Start with roots (inherently live declarations)
    2. For each live declaration, mark what it references as live
    3. Repeat until fixpoint
    
    Roots include:
    - Declarations annotated @live or @genType
    - Declarations referenced from non-declaration positions (external uses) *)

(** Reason why a declaration is live *)
type live_reason =
  | Annotated  (** Has @live or @genType annotation *)
  | ExternalRef  (** Referenced from outside any declaration *)
  | Propagated  (** Referenced by another live declaration *)

val reason_to_string : live_reason -> string
(** Convert a live reason to a human-readable string *)

val compute_forward :
  debug:bool ->
  decl_store:DeclarationStore.t ->
  refs:References.t ->
  ann_store:AnnotationStore.t ->
  live_reason PosHash.t
(** Compute liveness using forward propagation.
    Returns a hashtable mapping live positions to their [live_reason].
    Pass [~debug:true] for verbose output. *)

val is_live_forward : live:live_reason PosHash.t -> Lexing.position -> bool
(** Check if a position is live according to forward-computed liveness *)

val get_live_reason :
  live:live_reason PosHash.t -> Lexing.position -> live_reason option
(** Get the reason why a position is live, if it is *)
