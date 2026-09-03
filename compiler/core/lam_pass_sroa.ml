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

let valid_field field_count index = index >= 0 && index < field_count

(* Does the block appear anywhere other than as a direct, in-range field read
   or write? [escapes] and [rewrite] below are a matched pair: [rewrite] handles
   exactly the occurrences [escapes] accepts, and asserts on the rest. Extending
   one without the other is a compiler crash rather than a type error, so keep
   their cases in step. *)
let rec escapes block field_count (lam : Lambda.t) =
  match lam with
  | Lvar id -> Ident.same id block
  | Lassign (id, value) ->
    Ident.same id block || escapes block field_count value
  | Lprim {primitive = Pfield (index, _); args = [Lvar id]}
    when Ident.same id block ->
    not (valid_field field_count index)
  | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]}
    when Ident.same id block ->
    (not (valid_field field_count index)) || escapes block field_count value
  | _ -> Lambda.shallow_exists (escapes block field_count) lam

let rec rewrite block fields (lam : Lambda.t) =
  match lam with
  | Lprim {primitive = Pfield (index, _); args = [Lvar id]}
    when Ident.same id block ->
    Lambda.var fields.(index)
  | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]}
    when Ident.same id block ->
    Lambda.assign fields.(index) (rewrite block fields value)
  (* Unreachable: [escapes] rejected the block for both of these, so [replace]
     never reaches the rewrite. They are kept as assertions rather than dropped
     so that a future occurrence form added to [escapes] but not here fails
     loudly instead of silently losing the write. *)
  | Lvar id when Ident.same id block -> assert false
  | Lassign (id, _) when Ident.same id block -> assert false
  | _ -> Lambda.shallow_map_sharing (rewrite block fields) lam

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
    if escapes block field_count body then None
    else
      let fields = fields_for_block block info field_count in
      let body = rewrite block fields body in
      Some
        (Ext_list.fold_right2 (Array.to_list fields) initializers body
           (fun field init body -> Lambda.let_ Variable field init body))

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
  | _ -> Lambda.shallow_map_sharing simplify lam
