(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val check_attribute : Typedtree.expression -> Parsetree.attribute -> unit

val check_attribute_on_module :
  Typedtree.module_expr -> Parsetree.attribute -> unit

val add_inline_attribute :
  Lambda.lambda -> Location.t -> Parsetree.attributes -> Lambda.lambda

val get_inline_attribute : Parsetree.attributes -> Lambda.inline_attribute

val get_and_remove_inlined_attribute :
  Typedtree.expression -> Lambda.inline_attribute * Typedtree.expression

val get_and_remove_inlined_attribute_on_module :
  Typedtree.module_expr -> Lambda.inline_attribute * Typedtree.module_expr
