(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Generic walks over a Lambda term. Each builds only through Lambda's
   constructors, so what comes out is normalized like any other term. *)

val shallow_exists : (Lambda.t -> bool) -> Lambda.t -> bool
(** Does any immediate child satisfy the predicate? Short-circuits. *)

val shallow_map_sharing : (Lambda.t -> Lambda.t) -> Lambda.t -> Lambda.t
(** Rewrite a node's immediate children. A node whose children all come back
    physically unchanged is returned as-is, so a traversal that rewrites
    nothing allocates nothing. *)

val iter : (Lambda.t -> unit) -> Lambda.t -> unit

val free_variables : Lambda.t -> Set_ident.t

val subst_lambda : Lambda.t Ident.tbl -> Lambda.t -> Lambda.t
(** Substitute for the free variables in the domain of the substitution.
    Assumes the substitution's image is out of reach of the term's bound
    variables, so no capture can occur. *)

val make_key : Lambda.t -> Lambda.t option
(** A canonical form for comparing two terms: locations are dropped, alias
    bindings are substituted away, and remaining binders are renumbered. Only
    for comparison - the result is not meant to be emitted. [None] when the
    term is too big, or contains a form the key cannot canonicalize. *)
