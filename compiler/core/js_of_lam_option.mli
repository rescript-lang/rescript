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

type option_unwrap_time = Static_unwrapped | Runtime_maybe_unwrapped

val val_from_option : J.expression -> J.expression
(** Given [Some a ], return [a] *)

val get_default_undefined_from_optional : J.expression -> J.expression
(** Given [Some x] or [None], return [x]*)

val option_unwrap : J.expression -> J.expression
(** Given [Some (`a x)] or [None], 
    return [x] *)

val destruct_optional :
  for_sure_none:'a ->
  for_sure_some:(J.expression -> 'a) ->
  not_sure:(unit -> 'a) ->
  J.expression ->
  'a

val some : J.expression -> J.expression

val is_not_none : J.expression -> J.expression

val null_to_opt : J.expression -> J.expression

val undef_to_opt : J.expression -> J.expression

val null_undef_to_opt : J.expression -> J.expression
