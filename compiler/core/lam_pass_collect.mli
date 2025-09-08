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

(** This pass is used to collect meta data information.

    It includes:
    alias table, arity for identifiers and might more information,

    ATTENTION:
    For later pass to keep its information complete and up to date,
    we  need update its table accordingly

    - Alias inference is not for substitution, it is for analyze which module is 
      actually a global module or an exception, so it can be relaxed a bit
      (without relying on strict analysis)

    - Js object (local) analysis 

    Design choice:

    Side effectful operations:
    - Lassign 
    - Psetfield

    1. What information should be collected:

    2. What's the key
       If it's identifier, 

    Information that is always sound, not subject to change 

    - shall we collect that if an identifier is passed as a parameter, (useful for escape analysis), 
      however, since it's going to change after inlning (for local function)

    - function arity, subject to change when you make it a mutable ref and change it later

    - Immutable blocks of identifiers

      if identifier itself is function/non block then the access can be inlined 
      if identifier itself is immutable block can be inlined
      if identifier is mutable block can be inlined (without Lassign) since

    - When collect some information, shall we propogate this information to 
      all alias table immeidately

    - annotation identifiers (at first time)
      -
*)

val collect_info : Lam_stats.t -> Lam.t -> unit
(** Modify existing [meta] *)
