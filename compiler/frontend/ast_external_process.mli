(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type resolution = {
  pval_type: Parsetree.core_type;
      (** The declared type, without the arguments FFI attributes erase *)
  spec: External_ffi_types.t;
  pval_attributes: Parsetree.attributes;  (** Attributes not consumed *)
  no_inline_cross_module: bool;
      (** The external names a relative module, so other modules must not
          inline it *)
}

val resolve :
  Location.t -> Ast_core_type.t -> Ast_attributes.t -> string -> resolution
(** [resolve loc pval_type pval_attributes prim_name] applies the FFI
    attributes of an [external] declaration. Errors are raised and warnings
    reported for the declaration, and the attributes it consumes are marked
    used. *)
