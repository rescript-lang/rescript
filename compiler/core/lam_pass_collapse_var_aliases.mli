(* Copyright (C) 2026 - Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version. *)

(** Collapse [let x = y] aliases. Pattern matching introduces these as
    O(1) renames of a pattern ident onto the scrutinee; dropping them
    before collect keeps [ident_tbl] from re-recording the same alias.
    Exported names are kept so coercion can still see them. *)

val collapse : exports:Set_ident.t -> Lam.t -> Lam.t
