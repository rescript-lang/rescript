(** The dispatch of a [switch] on an untagged variant, as data: which tests
    the generated code runs on the scrutinee, in which statements, and which
    body each path runs. [Lam_compile] prints a plan as JavaScript. The plan
    is built once per [switch] from the variant's facts and the arms of the
    match, so every decision about the dispatch is made here, with all of
    them known. *)

(** A test on the scrutinee [x] *)
type test =
  | Equals of Variant_runtime.literal_tag  (** [x === literal] *)
  | Typeof of Variant_runtime.block_type
      (** [typeof x === "k"], for a kind with a [typeof] name *)
  | Instance of Variant_runtime.Instance.t
      (** [Array.isArray(x)] or [x instanceof C] *)
  | Not_null  (** [x !== null] *)
  | Not of test
  | And of test * test
  | Or of test * test

(** How the generated code reaches the bodies ['a] *)
type 'a tree =
  | Run of 'a  (** an arm's body, unchecked *)
  | Fallback
      (** the match's default action, reached from the payload dispatch *)
  | Skip  (** no statement *)
  | If of test * 'a tree * 'a tree
  | Block of 'a tree  (** a statement block around the tree *)
  | Switch_value of (Variant_runtime.literal_tag * 'a clause) list * 'a tree
      (** [switch (x)] over literals; the tree is the [default:] *)
  | Switch_typeof of (Variant_runtime.block_type * 'a clause) list * 'a tree
      (** [switch (typeof x)] over kinds with a [typeof] name; the tree is the
          [default:]. A switch with one clause that is not empty is printed
          as an [if]. *)

(** A [case] of a switch *)
and 'a clause =
  | Falls_through of 'a option
      (** an empty case falling through to the next one; the body only gives
          the source location *)
  | Body of 'a tree  (** a [Run] or [Fallback] *)

type 'a input = {
  facts: Variant_runtime.matching_facts;
  literal_arms: (Variant_runtime.literal_tag * 'a) list;
      (** the arms of the literal cases, in the order of the match *)
  payload_arms: (Variant_runtime.block_type * 'a) list;
      (** the arms of the payload cases, in the order of the match *)
  literals_complete: bool;  (** every literal case has an arm *)
  payloads_complete: bool;  (** every payload case has an arm *)
  fallback: 'a option;
      (** the default action of the match, for the cases without an arm *)
}

val plan : same:('a -> 'a -> bool) -> 'a input -> 'a tree
(** [same] tells two bodies that are one body apart *)

(** What constant folding knows about a constant: the JavaScript value it is
    emitted as *)
type value =
  | Literal of Variant_runtime.literal_tag
  | Object  (** a record or an inline record, which is no class instance *)
  | Array  (** a tuple, or a record whose fields are named 0, 1, … *)

type 'a outcome =
  | Arm of 'a
  | Default  (** the match's default action *)
  | Nothing  (** no statement runs *)

val select : ?on_test:(test -> unit) -> 'a tree -> value -> 'a outcome
(** What the code printed from a plan runs for a value: constant folding
    evaluates the plan instead of deciding by itself, so a folded match and
    the generated code agree. [on_test] sees each test that is evaluated. *)
