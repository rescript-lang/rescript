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

(* A local mutable block, with how its fields are used so far. [order]
   numbers blocks in the order of their declarations. [fields] is set when
   the block's scope has been analyzed and every occurrence was a direct,
   in-range field read or write. *)
type candidate = {
  order: int;
  uses: field_use array;
  mutable escapes: bool;
  mutable fields: Ident.t array option;
  mutable slots: slot list;
}

(* An initializer of a field of [owner], or a value written to it, without
   side occurrences. If the field is never read, the rewrite drops the value, and
   the occurrences of blocks declared before [owner] in it don't count:
   [occurrences] holds them until [owner] is decided. [parent] is the slot that
   contains this one. *)
and slot = {
  owner: candidate;
  field: int;
  parent: slot option;
  mutable occurrences: occurrence list;
}

and occurrence = Escape of candidate | Read of candidate * int

let valid_field uses index = index >= 0 && index < Array.length uses

let is_candidate kind (init : Lambda.t) =
  match (kind, init) with
  | ( (Lambda.Strict | StrictOpt),
      Lprim {primitive = Pmakeblock info; args = _ :: _} ) ->
    not (Lambda.is_immutable_block info)
  | _ -> false

(* Drop [value], a value of a field that is never read, keeping it only for
   its side effects, before [body]. [analyze] doesn't count the occurrences in
   the values dropped here, so they are not rewritten. *)
let discard_value rewrite value body =
  if Lam_analysis.no_side_effects value then body
  else Lambda.seq (rewrite value) body

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

(* Find the blocks whose every occurrence is a direct, in-range field read or
   write, recording how each field is used, in one traversal of the term.
   [rewrite] below handles exactly the occurrences accepted here, and asserts
   on the rest; keep their cases in step.

   The result is the one of analyzing each block's scope after rewriting the
   blocks declared in it, as if blocks were replaced one at a time from the
   innermost out, so pending [slot]s keep the occurrences that such a rewrite
   would drop. A block is decided when its scope has been traversed, so inner
   blocks are decided (and their fields named) before outer ones. *)
let analyze ~considered (lam : Lambda.t) =
  let candidates = Hash_ident.create 16 in
  let count = ref 0 in
  let apply = function
    | Escape c -> c.escapes <- true
    | Read (c, index) -> c.uses.(index).read <- true
  in
  let rec record slot occurrence =
    let target =
      match occurrence with
      | Escape c | Read (c, _) -> c
    in
    match slot with
    | Some slot when target.order < slot.owner.order ->
      slot.occurrences <- occurrence :: slot.occurrences
    | _ -> apply occurrence
  and decide c =
    if not c.escapes then (
      List.iter
        (fun slot ->
          if c.uses.(slot.field).read then
            List.iter (record slot.parent) slot.occurrences)
        c.slots;
      true)
    else (
      List.iter
        (fun slot -> List.iter (record slot.parent) slot.occurrences)
        c.slots;
      false)
  in
  (* The innermost pending slot containing the term being traversed. A
     reference rather than an argument of [go], so that traversing allocates
     no closures. *)
  let slot = ref None in
  let rec go (lam : Lambda.t) =
    match lam with
    | Lvar id -> (
      match Hash_ident.find_opt candidates id with
      | Some c -> record !slot (Escape c)
      | None -> ())
    | Lassign (id, value) ->
      (match Hash_ident.find_opt candidates id with
      | Some c -> record !slot (Escape c)
      | None -> ());
      go value
    | Lprim {primitive = Pfield (index, _); args = [Lvar id]}
      when Hash_ident.mem candidates id ->
      let c = Hash_ident.find candidates id in
      if valid_field c.uses index then record !slot (Read (c, index))
      else record !slot (Escape c)
    | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]}
      when Hash_ident.mem candidates id ->
      let c = Hash_ident.find candidates id in
      if valid_field c.uses index then (
        c.uses.(index).written <- true;
        go_value c index value)
      else (
        record !slot (Escape c);
        go value)
    | Llet (kind, block, init, body)
      when is_candidate kind init && considered block -> (
      match init with
      | Lprim {primitive = Pmakeblock info; args = initializers} ->
        let field_count = List.length initializers in
        let c =
          {
            order = !count;
            uses =
              Array.init field_count (fun _ -> {read = false; written = false});
            escapes = false;
            fields = None;
            slots = [];
          }
        in
        incr count;
        List.iteri (fun index init -> go_value c index init) initializers;
        Hash_ident.add candidates block c;
        go body;
        if decide c then
          c.fields <- Some (fields_for_block block info field_count)
      | _ -> assert false)
    | _ -> ignore (Lambda_traverse.shallow_exists go_child lam)
  and go_child lam =
    go lam;
    false
  (* Traverse a value of field [index] of [c], in a slot of its own if the
     rewrite drops the value when the field is never read. *)
  and go_value c index value =
    if Lam_analysis.no_side_effects value then (
      let parent = !slot in
      let s = {owner = c; field = index; parent; occurrences = []} in
      c.slots <- s :: c.slots;
      slot := Some s;
      go value;
      slot := parent)
    else go value
  in
  go lam;
  candidates

let rewrite candidates (lam : Lambda.t) =
  let replaced id =
    match Hash_ident.find_opt candidates id with
    | Some {fields = Some fields; uses} -> Some (fields, uses)
    | _ -> None
  in
  let rec rewrite (lam : Lambda.t) =
    match lam with
    | Lprim {primitive = Pfield (index, _); args = [Lvar id]} -> (
      match replaced id with
      | Some (fields, _) -> Lambda.var fields.(index)
      | None -> lam)
    | Lprim {primitive = Psetfield (index, _); args = [Lvar id; value]} -> (
      match replaced id with
      | Some (fields, uses) ->
        if not uses.(index).read then
          discard_value rewrite value Lambda.lambda_unit
        else Lambda.assign fields.(index) (rewrite value)
      | None -> Lambda_traverse.shallow_map_sharing rewrite lam)
    | Llet
        (_, block, Lprim {primitive = Pmakeblock _; args = initializers}, body)
      when replaced block <> None ->
      let fields, uses = Option.get (replaced block) in
      let body = rewrite body in
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
          if not use.read then discard_value rewrite init body
          else if not use.written then
            Lam_util.refine_let ~kind:Strict fields.(index) (rewrite init) body
          else Lambda.let_ Variable fields.(index) (rewrite init) body
      in
      bind_fields 0 initializers body
    (* Unreachable: [analyze] rejected the block for both of these. They are
       kept as assertions rather than dropped so that a future occurrence form
       added to [analyze] but not here fails loudly instead of silently losing
       the write. *)
    | Lvar id when replaced id <> None -> assert false
    | Lassign (id, _) when replaced id <> None -> assert false
    | _ -> Lambda_traverse.shallow_map_sharing rewrite lam
  in
  rewrite lam

(* Analyzing and rewriting each block separately would traverse its whole
   scope once per block, which is quadratic for a function with many local
   references. *)
let replace ~block ~info ~initializers body =
  let init =
    Lambda.prim ~primitive:(Pmakeblock info) ~args:initializers Location.none
  in
  if not (is_candidate Strict init) then None
  else
    let lam = Lambda.let_ Strict block init body in
    let candidates = analyze ~considered:(Ident.same block) lam in
    match Hash_ident.find_opt candidates block with
    | Some {fields = Some _} -> Some (rewrite candidates lam)
    | _ -> None

let simplify (lam : Lambda.t) =
  let candidates = analyze ~considered:(fun _ -> true) lam in
  if
    Hash_ident.fold
      (fun _ c found -> found || c.fields <> None)
      candidates false
  then rewrite candidates lam
  else lam
