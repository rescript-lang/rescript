(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Moscova, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Recording and dumping (partial) type information *)

(*
  We record all types in a list as they are created.
  This means we can dump type information even if type inference fails,
  which is extremely important, since type information is most
  interesting in case of errors.
*)

open Annot
open Lexing
open Location
open Typedtree

let output_int oc i = output_string oc (string_of_int i)

type annotation =
  | Ti_pat of pattern
  | Ti_expr of expression
  | Ti_class of unit
  | Ti_mod of module_expr
  | An_call of Location.t * Annot.call
  | An_ident of Location.t * string * Annot.ident

let get_location ti =
  match ti with
  | Ti_pat p -> p.pat_loc
  | Ti_expr e -> e.exp_loc
  | Ti_class () -> assert false
  | Ti_mod m -> m.mod_loc
  | An_call (l, _k) -> l
  | An_ident (l, _s, _k) -> l

let annotations = ref ([] : annotation list)
let phrases = ref ([] : Location.t list)

let record ti =
  if !Clflags.annotations && not (get_location ti).Location.loc_ghost then
    annotations := ti :: !annotations

let record_phrase loc = if !Clflags.annotations then phrases := loc :: !phrases

(* comparison order:
   the intervals are sorted by order of increasing upper bound
   same upper bound -> sorted by decreasing lower bound
*)
let cmp_loc_inner_first loc1 loc2 =
  match compare loc1.loc_end.pos_cnum loc2.loc_end.pos_cnum with
  | 0 -> compare loc2.loc_start.pos_cnum loc1.loc_start.pos_cnum
  | x -> x
let cmp_ti_inner_first ti1 ti2 =
  cmp_loc_inner_first (get_location ti1) (get_location ti2)

let print_position pp pos =
  if pos = dummy_pos then output_string pp "--"
  else (
    output_char pp '\"';
    output_string pp (String.escaped pos.pos_fname);
    output_string pp "\" ";
    output_int pp pos.pos_lnum;
    output_char pp ' ';
    output_int pp pos.pos_bol;
    output_char pp ' ';
    output_int pp pos.pos_cnum)

let print_location pp loc =
  print_position pp loc.loc_start;
  output_char pp ' ';
  print_position pp loc.loc_end

let sort_filter_phrases () =
  let ph = List.sort (fun x y -> cmp_loc_inner_first y x) !phrases in
  let rec loop accu cur l =
    match l with
    | [] -> accu
    | loc :: t ->
      if
        cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum
        && cur.loc_end.pos_cnum >= loc.loc_end.pos_cnum
      then loop accu cur t
      else loop (loc :: accu) loc t
  in
  phrases := loop [] Location.none ph

let rec printtyp_reset_maybe loc =
  match !phrases with
  | cur :: t when cur.loc_start.pos_cnum <= loc.loc_start.pos_cnum ->
    Printtyp.reset ();
    phrases := t;
    printtyp_reset_maybe loc
  | _ -> ()

let call_kind_string k =
  match k with
  | Tail -> "tail"
  | Stack -> "stack"
  | Inline -> "inline"

let print_ident_annot pp str k =
  match k with
  | Idef l ->
    output_string pp "def ";
    output_string pp str;
    output_char pp ' ';
    print_location pp l;
    output_char pp '\n'
  | Iref_internal l ->
    output_string pp "int_ref ";
    output_string pp str;
    output_char pp ' ';
    print_location pp l;
    output_char pp '\n'
  | Iref_external ->
    output_string pp "ext_ref ";
    output_string pp str;
    output_char pp '\n'

(* The format of the annotation file is documented in emacs/caml-types.el. *)

let print_info pp prev_loc ti =
  match ti with
  | Ti_class _ | Ti_mod _ -> prev_loc
  | Ti_pat {pat_loc = loc; pat_type = typ; pat_env = env}
  | Ti_expr {exp_loc = loc; exp_type = typ; exp_env = env} ->
    if loc <> prev_loc then (
      print_location pp loc;
      output_char pp '\n');
    output_string pp "type(\n";
    printtyp_reset_maybe loc;
    Printtyp.mark_loops typ;
    Format.pp_print_string Format.str_formatter "  ";
    Printtyp.wrap_printing_env env (fun () ->
        Printtyp.type_sch Format.str_formatter typ);
    Format.pp_print_newline Format.str_formatter ();
    let s = Format.flush_str_formatter () in
    output_string pp s;
    output_string pp ")\n";
    loc
  | An_call (loc, k) ->
    if loc <> prev_loc then (
      print_location pp loc;
      output_char pp '\n');
    output_string pp "call(\n  ";
    output_string pp (call_kind_string k);
    output_string pp "\n)\n";
    loc
  | An_ident (loc, str, k) ->
    if loc <> prev_loc then (
      print_location pp loc;
      output_char pp '\n');
    output_string pp "ident(\n  ";
    print_ident_annot pp str k;
    output_string pp ")\n";
    loc

let get_info () =
  let info = List.fast_sort cmp_ti_inner_first !annotations in
  annotations := [];
  info

let dump filename =
  if !Clflags.annotations then (
    let do_dump _temp_filename pp =
      let info = get_info () in
      sort_filter_phrases ();
      ignore (List.fold_left (print_info pp) Location.none info)
    in
    (match filename with
    | None -> do_dump "" stdout
    | Some filename ->
      Misc.output_to_file_via_temporary ~mode:[Open_text] filename do_dump);
    phrases := [])
  else annotations := []
