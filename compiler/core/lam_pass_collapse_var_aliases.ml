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

let collapse ~exports (lam : Lam.t) : Lam.t =
  let tbl = Hash_ident.create 64 in
  let rec go (lam : Lam.t) : Lam.t =
    match lam with
    | Lvar x -> Lam.var (resolve tbl x)
    | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> lam
    | Lapply {ap_func; ap_args; ap_info; ap_transformed_jsx} ->
      Lam.apply (go ap_func) (Ext_list.map ap_args go) ap_info
        ~ap_transformed_jsx
    | Lfunction {arity; params; body; attr; loc} ->
      Lam.function_ ~loc ~attr ~arity ~params ~body:(go body)
    | Llet (Alias, id, Lvar u, body) ->
      let u = resolve tbl u in
      Hash_ident.add tbl id u;
      if Set_ident.mem exports id then Lam.let_ Alias id (Lam.var u) (go body)
      else go body
    | Llet (kind, id, arg, body) -> Lam.let_ kind id (go arg) (go body)
    | Lletrec (bindings, body) ->
      Lam.letrec (Ext_list.map_snd bindings go) (go body)
    | Lprim {primitive; args; loc} ->
      Lam.prim ~primitive ~args:(Ext_list.map args go) loc
    | Lswitch (arg, sw) ->
      Lam.switch (go arg)
        {
          sw with
          sw_consts = Ext_list.map_snd sw.sw_consts go;
          sw_blocks = Ext_list.map_snd sw.sw_blocks go;
          sw_failaction = Ext_option.map sw.sw_failaction go;
        }
    | Lstringswitch (arg, cases, default) ->
      Lam.stringswitch (go arg)
        (Ext_list.map_snd cases go)
        (Ext_option.map default go)
    | Lstaticraise (i, args) -> Lam.staticraise i (Ext_list.map args go)
    | Lstaticcatch (body, ids, handler) ->
      Lam.staticcatch (go body) ids (go handler)
    | Ltrywith (body, id, handler) -> Lam.try_ (go body) id (go handler)
    | Lifthenelse (b, t, e) -> Lam.if_ (go b) (go t) (go e)
    | Lsequence (a, b) -> Lam.seq (go a) (go b)
    | Lwhile (b, body) -> Lam.while_ (go b) (go body)
    | Lfor (id, lo, hi, dir, body) -> Lam.for_ id (go lo) (go hi) dir (go body)
    | Lfor_of (id, iterable, body) -> Lam.for_of id (go iterable) (go body)
    | Lfor_await_of (id, iterable, body) ->
      Lam.for_await_of id (go iterable) (go body)
    | Lassign (id, e) -> Lam.assign id (go e)
  in
  go lam
