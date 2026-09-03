(* Copyright (C) 2026 - Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version. *)

let rec resolve tbl id =
  match Hash_ident.find_opt tbl id with
  | None -> id
  | Some id' -> resolve tbl id'

let collapse ~exports (lam : Lambda.t) : Lambda.t =
  let tbl = Hash_ident.create 16 in
  let rec go (lam : Lambda.t) : Lambda.t =
    match lam with
    | Lvar x ->
      let x' = resolve tbl x in
      if x' == x then lam else Lambda.var x'
    | Llet (Alias, id, Lvar u, body) ->
      let u = resolve tbl u in
      Hash_ident.add tbl id u;
      (* The binding is dropped unless the name is exported, in which case it
         has to survive under its own name. *)
      if Set_ident.mem exports id then
        Lambda.let_ Alias id (Lambda.var u) (go body)
      else go body
    | _ -> Lambda_traverse.shallow_map_sharing go lam
  in
  go lam
