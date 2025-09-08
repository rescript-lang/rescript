(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

(* It can be useful for common sub expression elimination ?
    if two lambdas are not equal, it should return false, other wise,
    it might return true , this is only used as a way of optimizaton

    Use case :
    1. switch case -- common fall through
*)

(* lambda pass for alpha conversion
    and alias
    we need think about the order of the pass, might be the alias pass can be done
    in the  beginning, when we do alpha conversion, we can instrument the table
*)

(* type alias_tbl =  Ident.t Hash_ident.t *)

type ident_tbl = Lam_id_kind.t Hash_ident.t

type t = {
  export_idents: Set_ident.t;
  exports: Ident.t list;
  (*It is kept since order matters? *)
  ident_tbl: ident_tbl;
      (** we don't need count arities for all identifiers, for identifiers
      for sure it's not a function, there is no need to count them
  *)
}

let pp = Format.fprintf

(* let pp_alias_tbl fmt (tbl : alias_tbl) =
   Hash_ident.iter  tbl (fun k v -> pp fmt "@[%a -> %a@]@." Ident.print k Ident.print v) *)

let pp_ident_tbl fmt (ident_tbl : ident_tbl) =
  Hash_ident.iter ident_tbl (fun k v ->
      pp fmt "@[%a -> %a@]@." Ident.print k Lam_id_kind.print v)

let print fmt (v : t) =
  pp fmt "@[Ident table:@ @[%a@]@]" pp_ident_tbl v.ident_tbl;
  pp fmt "@[exports:@ @[%a@]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> pp fmt "@ ;") Ident.print)
    v.exports

let make ~export_idents ~export_ident_sets : t =
  {
    ident_tbl = Hash_ident.create 31;
    exports = export_idents;
    export_idents = export_ident_sets;
  }
