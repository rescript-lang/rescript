(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(* Adapted for Javascript backend : Hongbo Zhang,  *)

(* Scalar replacement of aggregates (SROA) for local mutable blocks.

   A block can be replaced by mutable scalar bindings when every occurrence
   of the block is a direct, statically indexed field read or write. JavaScript
   closures capture bindings, so direct accesses from nested functions remain
   eligible. Analysis is kept separate from rewriting so a failed eligibility
   check cannot partially transform the term. *)

type field_use = {mutable read: bool; mutable written: bool}

let valid_field uses index = index >= 0 && index < Array.length uses

(* Does the block appear only as direct, in-range field reads and writes? While
   answering, record how every field is used. [analyze] and [rewrite] below are
   a matched pair: [rewrite] handles exactly the occurrences [analyze] accepts,
   and asserts on the rest. Extending one without the other is a compiler crash
   rather than a type error, so keep their cases in step. *)
let rec analyze block uses (lam : Lambda.t) =
  match lam with
  | Lvar id -> not (Ident.same id block)
  | Lassign (id, value) ->
    (not (Ident.same id block)) && analyze block uses value
  | Lprim {primitive = Pfield (index, _); args = [Lvar id]}
    when Ident.same id block ->
    if valid_field uses index then (
      uses.(index).read <- true;
      true)
    else false
  | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]}
    when Ident.same id block ->
    if valid_field uses index then (
      uses.(index).written <- true;
      analyze block uses value)
    else false
  | _ ->
    not
      (Lambda_traverse.shallow_exists
         (fun child -> not (analyze block uses child))
         lam)

let discard_value value body =
  if Lam_analysis.no_side_effects value then body else Lambda.seq value body

let rec rewrite block fields uses (lam : Lambda.t) =
  match lam with
  | Lprim {primitive = Pfield (index, _); args = [Lvar id]}
    when Ident.same id block ->
    Lambda.var fields.(index)
  | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]}
    when Ident.same id block ->
    let value = rewrite block fields uses value in
    if not uses.(index).read then discard_value value Lambda.lambda_unit
    else Lambda.assign fields.(index) value
  (* Unreachable: [analyze] rejected the block for both of these, so [replace]
     never reaches the rewrite. They are kept as assertions rather than dropped
     so that a future occurrence form added to [analyze] but not here fails
     loudly instead of silently losing the write. *)
  | Lvar id when Ident.same id block -> assert false
  | Lassign (id, _) when Ident.same id block -> assert false
  | _ -> Lambda_traverse.shallow_map_sharing (rewrite block fields uses) lam

let fields_for_block block info field_count =
  let fallback () =
    Array.init field_count (fun index ->
        if index = 0 then block else Ident.rename block)
  in
  if field_count = 1 then [|block|]
  else
    let names =
      match info with
      | Lambda.Blk_record {fields} | Lambda.Blk_record_inlined {fields} ->
        if Array.length fields = field_count then
          Some (Array.map (fun (name, _) -> name) fields)
        else None
      | Lambda.Blk_record_ext {fields} ->
        if Array.length fields = field_count then Some fields else None
      | Lambda.Blk_tuple | Lambda.Blk_constructor _ | Lambda.Blk_poly_var
      | Lambda.Blk_module _ | Lambda.Blk_module_export _ | Lambda.Blk_extension
        ->
        None
    in
    match names with
    | None -> fallback ()
    | Some names ->
      Array.map (fun name -> Ident.create (Ident.name block ^ "_" ^ name)) names

let replace ~block ~info ~initializers body =
  match initializers with
  | [] -> None
  | _ ->
    let field_count = List.length initializers in
    let uses =
      Array.init field_count (fun _ -> {read = false; written = false})
    in
    if not (analyze block uses body) then None
    else
      let fields = fields_for_block block info field_count in
      let body = rewrite block fields uses body in
      let rec bind_fields index initializers body =
        match initializers with
        | [] -> body
        | init :: rest ->
          let body = bind_fields (index + 1) rest body in
          let use = uses.(index) in
          (* A never-read field needs no storage; its initializer and writes are
             retained only when they have effects. A read-only field can use a
             normal refined let, while a field that is both read and written
             still needs a mutable scalar binding. *)
          if not use.read then discard_value init body
          else if not use.written then
            Lam_util.refine_let ~kind:Strict fields.(index) init body
          else Lambda.let_ Variable fields.(index) init body
      in
      Some (bind_fields 0 initializers body)

let rec simplify (lam : Lambda.t) =
  match lam with
  | Llet (kind, block, init, body) -> (
    let init' = simplify init in
    let body' = simplify body in
    match (kind, init') with
    | ( (Strict | StrictOpt),
        Lambda.Lprim {primitive = Pmakeblock info; args = initializers} )
      when not (Lambda.is_immutable_block info) -> (
      match replace ~block ~info ~initializers body' with
      | Some replacement -> replacement
      | None ->
        if init' == init && body' == body then lam
        else Lambda.let_ kind block init' body')
    | _ ->
      if init' == init && body' == body then lam
      else Lambda.let_ kind block init' body')
  | _ -> Lambda_traverse.shallow_map_sharing simplify lam
