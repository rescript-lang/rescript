(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Replacing locally abstract types by type variables. This raises a
    compiler error, so it is not part of [Ast_helper]. *)

val varify_constructors :
  string Asttypes.loc list -> Parsetree.core_type -> Parsetree.core_type
(** [varify_constructors newtypes te] is type expression [te], of which
    any of nullary type constructor [tc] is replaced by type variable of
    the same name, if [tc]'s name appears in [newtypes].
    Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
    appears in [newtypes]. *)
