(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Misc
open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet = Set.Make (TypeOps)
module TypeMap = Map.Make (TypeOps)
module TypeHash = Hashtbl.Make (TypeOps)

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Type level management ****)

let generic_level = 100000000

(* Used to mark a type during a traversal. *)
let lowest_level = 0
let pivot_level = (2 * lowest_level) - 1
(* pivot_level - lowest_level < lowest_level *)

(**** Some type creators ****)

let new_id = ref (-1)

let newty2 level desc =
  incr new_id;
  {desc; level; id = !new_id}
let newgenty desc = newty2 generic_level desc
let newgenvar ?name () = newgenty (Tvar name)
(*
let newmarkedvar level =
  incr new_id; { desc = Tvar; level = pivot_level - level; id = !new_id }
let newmarkedgenvar () =
  incr new_id;
  { desc = Tvar; level = pivot_level - generic_level; id = !new_id }
*)

(**** Check some types ****)

let is_Tvar = function
  | {desc = Tvar _} -> true
  | _ -> false
let is_Tunivar = function
  | {desc = Tunivar _} -> true
  | _ -> false
let is_Tconstr = function
  | {desc = Tconstr _} -> true
  | _ -> false

let dummy_method = "*dummy method*"
let default_mty = function
  | Some mty -> mty
  | None -> Mty_signature []

(**** Definitions for backtracking ****)

type change =
  | Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of row_field option ref * row_field option
  | Ckind of field_kind option ref * field_kind option
  | Ccommu of commutable ref * commutable
  | Cuniv of type_expr option ref * type_expr option
  | Ctypeset of TypeSet.t ref * TypeSet.t

type changes = Change of change * changes ref | Unchanged | Invalid

let trail = Weak.create 1

let log_change ch =
  match Weak.get trail 0 with
  | None -> ()
  | Some r ->
    let r' = ref Unchanged in
    r := Change (ch, r');
    Weak.set trail 0 (Some r')

(**** Representative of a type ****)

let rec field_kind_repr = function
  | Fvar {contents = Some kind} -> field_kind_repr kind
  | kind -> kind

let rec repr_link compress t d = function
  | {desc = Tlink t' as d'} -> repr_link true t d' t'
  | {desc = Tfield (_, k, _, t') as d'} when field_kind_repr k = Fabsent ->
    repr_link true t d' t'
  | t' ->
    if compress then (
      log_change (Ccompress (t, t.desc, d));
      t.desc <- d);
    t'

let repr t =
  match t.desc with
  | Tlink t' as d -> repr_link false t d t'
  | Tfield (_, k, _, t') as d when field_kind_repr k = Fabsent ->
    repr_link false t d t'
  | _ -> t

let rec commu_repr = function
  | Clink r when !r <> Cunknown -> commu_repr !r
  | c -> c

let rec row_field_repr_aux tl = function
  | Reither (_, tl', _, {contents = Some fi}) ->
    row_field_repr_aux (tl @ tl') fi
  | Reither (c, tl', m, r) -> Reither (c, tl @ tl', m, r)
  | Rpresent (Some _) when tl <> [] -> Rpresent (Some (List.hd tl))
  | fi -> fi

let row_field_repr fi = row_field_repr_aux [] fi

let rec rev_concat l ll =
  match ll with
  | [] -> l
  | l' :: ll -> rev_concat (l' @ l) ll

let rec row_repr_aux ll row =
  match (repr row.row_more).desc with
  | Tvariant row' ->
    let f = row.row_fields in
    row_repr_aux (if f = [] then ll else f :: ll) row'
  | _ ->
    if ll = [] then row
    else {row with row_fields = rev_concat row.row_fields ll}

let row_repr row = row_repr_aux [] row

let rec row_field tag row =
  let rec find = function
    | (tag', f) :: fields ->
      if tag = tag' then row_field_repr f else find fields
    | [] -> (
      match repr row.row_more with
      | {desc = Tvariant row'} -> row_field tag row'
      | _ -> Rabsent)
  in
  find row.row_fields

let rec row_more row =
  match repr row.row_more with
  | {desc = Tvariant row'} -> row_more row'
  | ty -> ty

let row_fixed row =
  let row = row_repr row in
  row.row_fixed
  ||
  match (repr row.row_more).desc with
  | Tvar _ | Tnil -> false
  | Tunivar _ | Tconstr _ -> true
  | _ -> assert false

let static_row row =
  let row = row_repr row in
  row.row_closed
  && List.for_all
       (fun (_, f) ->
         match row_field_repr f with
         | Reither _ -> false
         | _ -> true)
       row.row_fields

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := (223 * !accu) + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land ((1 lsl 31) - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let proxy ty =
  let ty0 = repr ty in
  match ty0.desc with
  | Tvariant row when not (static_row row) -> row_more row
  | Tobject (ty, _) ->
    let rec proxy_obj ty =
      match ty.desc with
      | Tfield (_, _, _, ty) | Tlink ty -> proxy_obj ty
      | Tvar _ | Tunivar _ | Tconstr _ -> ty
      | Tnil -> ty0
      | _ -> assert false
    in
    proxy_obj ty
  | _ -> ty0

(**** Utilities for fixed row private types ****)

let row_of_type t =
  match (repr t).desc with
  | Tobject (t, _) ->
    let rec get_row t =
      let t = repr t in
      match t.desc with
      | Tfield (_, _, _, t) -> get_row t
      | _ -> t
    in
    get_row t
  | Tvariant row -> row_more row
  | _ -> t

let has_constr_row t = (not (is_Tconstr t)) && is_Tconstr (row_of_type t)

let is_row_name s =
  let l = String.length s in
  if l < 4 then false else String.sub s (l - 4) 4 = "#row"

let is_constr_row ~allow_ident t =
  match t.desc with
  | Tconstr (Path.Pident id, _, _) when allow_ident ->
    is_row_name (Ident.name id)
  | Tconstr (Path.Pdot (_, s, _), _, _) -> is_row_name s
  | _ -> false

(**********************************)
(*  Utilities for type traversal  *)
(**********************************)

let rec iter_row f row =
  List.iter
    (fun (_, fi) ->
      match row_field_repr fi with
      | Rpresent (Some ty) -> f ty
      | Reither (_, tl, _, _) -> List.iter f tl
      | _ -> ())
    row.row_fields;
  match (repr row.row_more).desc with
  | Tvariant row -> iter_row f row
  | Tvar _ | Tunivar _ | Tsubst _ | Tconstr _ | Tnil ->
    Misc.may (fun (_, l) -> List.iter f l) row.row_name
  | _ -> assert false

let iter_type_expr f ty =
  match ty.desc with
  | Tvar _ -> ()
  | Tarrow (_, ty1, ty2, _, _) ->
    f ty1;
    f ty2
  | Ttuple l -> List.iter f l
  | Tconstr (_, l, _) -> List.iter f l
  | Tobject (ty, {contents = Some (_, p)}) ->
    f ty;
    List.iter f p
  | Tobject (ty, _) -> f ty
  | Tvariant row ->
    iter_row f row;
    f (row_more row)
  | Tfield (_, _, ty1, ty2) ->
    f ty1;
    f ty2
  | Tnil -> ()
  | Tlink ty -> f ty
  | Tsubst ty -> f ty
  | Tunivar _ -> ()
  | Tpoly (ty, tyl) ->
    f ty;
    List.iter f tyl
  | Tpackage (_, _, l) -> List.iter f l

let rec iter_abbrev f = function
  | Mnil -> ()
  | Mcons (_, _, ty, ty', rem) ->
    f ty;
    f ty';
    iter_abbrev f rem
  | Mlink rem -> iter_abbrev f !rem

type type_iterators = {
  it_signature: type_iterators -> signature -> unit;
  it_signature_item: type_iterators -> signature_item -> unit;
  it_value_description: type_iterators -> value_description -> unit;
  it_type_declaration: type_iterators -> type_declaration -> unit;
  it_extension_constructor: type_iterators -> extension_constructor -> unit;
  it_module_declaration: type_iterators -> module_declaration -> unit;
  it_modtype_declaration: type_iterators -> modtype_declaration -> unit;
  it_module_type: type_iterators -> module_type -> unit;
  it_type_kind: type_iterators -> type_kind -> unit;
  it_do_type_expr: type_iterators -> type_expr -> unit;
  it_type_expr: type_iterators -> type_expr -> unit;
  it_path: Path.t -> unit;
}

let iter_type_expr_cstr_args f = function
  | Cstr_tuple tl -> List.iter f tl
  | Cstr_record lbls -> List.iter (fun d -> f d.ld_type) lbls

let map_type_expr_cstr_args f = function
  | Cstr_tuple tl -> Cstr_tuple (List.map f tl)
  | Cstr_record lbls ->
    Cstr_record (List.map (fun d -> {d with ld_type = f d.ld_type}) lbls)

let iter_type_expr_kind f = function
  | Type_abstract -> ()
  | Type_variant cstrs ->
    List.iter
      (fun cd ->
        iter_type_expr_cstr_args f cd.cd_args;
        Misc.may f cd.cd_res)
      cstrs
  | Type_record (lbls, _) -> List.iter (fun d -> f d.ld_type) lbls
  | Type_open -> ()

let type_iterators =
  let it_signature it = List.iter (it.it_signature_item it)
  and it_signature_item it = function
    | Sig_value (_, vd) -> it.it_value_description it vd
    | Sig_type (_, td, _) -> it.it_type_declaration it td
    | Sig_typext (_, td, _) -> it.it_extension_constructor it td
    | Sig_module (_, md, _) -> it.it_module_declaration it md
    | Sig_modtype (_, mtd) -> it.it_modtype_declaration it mtd
    | Sig_class () -> assert false
    | Sig_class_type () -> assert false
  and it_value_description it vd = it.it_type_expr it vd.val_type
  and it_type_declaration it td =
    List.iter (it.it_type_expr it) td.type_params;
    may (it.it_type_expr it) td.type_manifest;
    it.it_type_kind it td.type_kind
  and it_extension_constructor it td =
    it.it_path td.ext_type_path;
    List.iter (it.it_type_expr it) td.ext_type_params;
    iter_type_expr_cstr_args (it.it_type_expr it) td.ext_args;
    may (it.it_type_expr it) td.ext_ret_type
  and it_module_declaration it md = it.it_module_type it md.md_type
  and it_modtype_declaration it mtd = may (it.it_module_type it) mtd.mtd_type
  and it_module_type it = function
    | Mty_ident p | Mty_alias (_, p) -> it.it_path p
    | Mty_signature sg -> it.it_signature it sg
    | Mty_functor (_, mto, mt) ->
      may (it.it_module_type it) mto;
      it.it_module_type it mt
  and it_type_kind it kind = iter_type_expr_kind (it.it_type_expr it) kind
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match ty.desc with
    | Tconstr (p, _, _)
    | Tobject (_, {contents = Some (p, _)})
    | Tpackage (p, _, _) ->
      it.it_path p
    | Tvariant row -> may (fun (p, _) -> it.it_path p) (row_repr row).row_name
    | _ -> ()
  and it_path _p = () in
  {
    it_path;
    it_type_expr = it_do_type_expr;
    it_do_type_expr;
    it_type_kind;
    it_module_type;
    it_signature;
    it_modtype_declaration;
    it_module_declaration;
    it_extension_constructor;
    it_type_declaration;
    it_value_description;
    it_signature_item;
  }

let copy_row f fixed row keep more =
  let fields =
    List.map
      (fun (l, fi) ->
        ( l,
          match row_field_repr fi with
          | Rpresent (Some ty) -> Rpresent (Some (f ty))
          | Reither (c, tl, m, e) ->
            let e = if keep then e else ref None in
            let m = if row.row_fixed then fixed else m in
            let tl = List.map f tl in
            Reither (c, tl, m, e)
          | _ -> fi ))
      row.row_fields
  in
  let name =
    match row.row_name with
    | None -> None
    | Some (path, tl) -> Some (path, List.map f tl)
  in
  {
    row_fields = fields;
    row_more = more;
    row_fixed = row.row_fixed && fixed;
    row_closed = row.row_closed;
    row_name = name;
  }

let rec copy_kind = function
  | Fvar {contents = Some k} -> copy_kind k
  | Fvar _ -> Fvar (ref None)
  | Fpresent -> Fpresent
  | Fabsent -> assert false

let copy_commu c = if commu_repr c = Cok then Cok else Clink (ref Cunknown)

(* Since univars may be used as row variables, we need to do some
   encoding during substitution *)
let rec norm_univar ty =
  match ty.desc with
  | Tunivar _ | Tsubst _ -> ty
  | Tlink ty -> norm_univar ty
  | Ttuple (ty :: _) -> norm_univar ty
  | _ -> assert false

let rec copy_type_desc ?(keep_names = false) f = function
  | Tvar _ as ty -> if keep_names then ty else Tvar None
  | Tarrow (p, ty1, ty2, c, arity) ->
    Tarrow (p, f ty1, f ty2, copy_commu c, arity)
  | Ttuple l -> Ttuple (List.map f l)
  | Tconstr (p, l, _) -> Tconstr (p, List.map f l, ref Mnil)
  | Tobject (ty, {contents = Some (p, tl)}) ->
    Tobject (f ty, ref (Some (p, List.map f tl)))
  | Tobject (ty, _) -> Tobject (f ty, ref None)
  | Tvariant _ -> assert false (* too ambiguous *)
  | Tfield (p, k, ty1, ty2) ->
    (* the kind is kept shared *)
    Tfield (p, field_kind_repr k, f ty1, f ty2)
  | Tnil -> Tnil
  | Tlink ty -> copy_type_desc f ty.desc
  | Tsubst _ -> assert false
  | Tunivar _ as ty -> ty (* always keep the name *)
  | Tpoly (ty, tyl) ->
    let tyl = List.map (fun x -> norm_univar (f x)) tyl in
    Tpoly (f ty, tyl)
  | Tpackage (p, n, l) -> Tpackage (p, n, List.map f l)

(* Utilities for copying *)

let saved_desc = ref []
(* Saved association of generic nodes with their description. *)

let save_desc ty desc = saved_desc := (ty, desc) :: !saved_desc

let saved_kinds = ref [] (* duplicated kind variables *)
let new_kinds = ref [] (* new kind variables *)
let dup_kind r =
  (match !r with
  | None -> ()
  | Some _ -> assert false);
  if not (List.memq r !new_kinds) then (
    saved_kinds := r :: !saved_kinds;
    let r' = ref None in
    new_kinds := r' :: !new_kinds;
    r := Some (Fvar r'))

(* Restored type descriptions. *)
let cleanup_types () =
  List.iter (fun (ty, desc) -> ty.desc <- desc) !saved_desc;
  List.iter (fun r -> r := None) !saved_kinds;
  saved_desc := [];
  saved_kinds := [];
  new_kinds := []

(* Mark a type. *)
let rec mark_type ty =
  let ty = repr ty in
  if ty.level >= lowest_level then (
    ty.level <- pivot_level - ty.level;
    iter_type_expr mark_type ty)

let mark_type_node ty =
  let ty = repr ty in
  if ty.level >= lowest_level then ty.level <- pivot_level - ty.level

let mark_type_params ty = iter_type_expr mark_type ty

let type_iterators =
  let it_type_expr it ty =
    let ty = repr ty in
    if ty.level >= lowest_level then (
      mark_type_node ty;
      it.it_do_type_expr it ty)
  in
  {type_iterators with it_type_expr}

(* Remove marks from a type. *)
let rec unmark_type ty =
  let ty = repr ty in
  if ty.level < lowest_level then (
    ty.level <- pivot_level - ty.level;
    iter_type_expr unmark_type ty)

let unmark_iterators =
  let it_type_expr _it ty = unmark_type ty in
  {type_iterators with it_type_expr}

let unmark_type_decl decl =
  unmark_iterators.it_type_declaration unmark_iterators decl

let unmark_extension_constructor ext =
  List.iter unmark_type ext.ext_type_params;
  iter_type_expr_cstr_args unmark_type ext.ext_args;
  Misc.may unmark_type ext.ext_ret_type

(*******************************************)
(*  Memorization of abbreviation expansion *)
(*******************************************)

(* Search whether the expansion has been memorized. *)

let lte_public p1 p2 =
  (* Private <= Public *)
  match (p1, p2) with
  | Private, _ | _, Public -> true
  | Public, Private -> false

let rec find_expans priv p1 = function
  | Mnil -> None
  | Mcons (priv', p2, _ty0, ty, _) when lte_public priv priv' && Path.same p1 p2
    ->
    Some ty
  | Mcons (_, _, _, _, rem) -> find_expans priv p1 rem
  | Mlink {contents = rem} -> find_expans priv p1 rem

(* debug: check for cycles in abbreviation. only works with -principal
   let rec check_expans visited ty =
     let ty = repr ty in
     assert (not (List.memq ty visited));
     match ty.desc with
       Tconstr (path, args, abbrev) ->
         begin match find_expans path !abbrev with
           Some ty' -> check_expans (ty :: visited) ty'
         | None -> ()
         end
     | _ -> ()
*)

let memo = ref []
(* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
  (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
  (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (priv, path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
  | Mnil -> assert false
  | Mcons (_, path', _, _, rem) when Path.same path path' -> rem
  | Mcons (priv, path', v, v', rem) ->
    Mcons (priv, path', v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
    mem' := forget_abbrev_rec !mem' path;
    raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()

(* debug: check for invalid abbreviations
   let rec check_abbrev_rec = function
       Mnil -> true
     | Mcons (_, ty1, ty2, rem) ->
         repr ty1 != repr ty2
     | Mlink mem' ->
         check_abbrev_rec !mem'

   let check_memorized_abbrevs () =
     List.for_all (fun mem -> check_abbrev_rec !mem) !memo
*)

(**********************************)
(*  Utilities for labels          *)
(**********************************)

let is_optional = function
  | Optional _ -> true
  | _ -> false

let label_name = function
  | Nolabel -> ""
  | Labelled s | Optional s -> s

let prefixed_label_name = function
  | Nolabel -> ""
  | Labelled s -> "~" ^ s
  | Optional s -> "?" ^ s

type sargs = (Asttypes.arg_label * Parsetree.expression) list

let rec extract_label_aux hd l = function
  | [] -> None
  | ((l', t) as p) :: ls ->
    if label_name l' = l then Some (l', t, List.rev_append hd ls)
    else extract_label_aux (p :: hd) l ls

let extract_label l (ls : sargs) :
    (arg_label * Parsetree.expression * sargs) option =
  extract_label_aux [] l ls

let rec label_assoc x (args : sargs) =
  match args with
  | [] -> false
  | (a, _) :: l -> Asttypes.same_arg_label a x || label_assoc x l

(**********************************)
(*  Utilities for backtracking    *)
(**********************************)

let undo_change = function
  | Ctype (ty, desc) -> ty.desc <- desc
  | Ccompress (ty, desc, _) -> ty.desc <- desc
  | Clevel (ty, level) -> ty.level <- level
  | Cname (r, v) -> r := v
  | Crow (r, v) -> r := v
  | Ckind (r, v) -> r := v
  | Ccommu (r, v) -> r := v
  | Cuniv (r, v) -> r := v
  | Ctypeset (r, v) -> r := v

type snapshot = changes ref * int
let last_snapshot = ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  log_type ty;
  let desc = ty.desc in
  ty.desc <- Tlink ty';
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match (desc, ty'.desc) with
  | Tvar name, Tvar name' -> (
    match (name, name') with
    | Some _, None ->
      log_type ty';
      ty'.desc <- Tvar name
    | None, Some _ -> ()
    | Some _, Some _ ->
      if ty.level < ty'.level then (
        log_type ty';
        ty'.desc <- Tvar name)
    | None, None -> ())
  | _ -> ()

(* ; assert (check_memorized_abbrevs ()) *)
(*  ; check_expans [] ty' *)
let set_level ty level =
  if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
  ty.level <- level
let set_univar rty ty =
  log_change (Cuniv (rty, !rty));
  rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm));
  nm := v
let set_row_field e v =
  log_change (Crow (e, !e));
  e := Some v
let set_kind rk k =
  log_change (Ckind (rk, !rk));
  rk := Some k
let set_commu rc c =
  log_change (Ccommu (rc, !rc));
  rc := c
let set_typeset rs s =
  log_change (Ctypeset (rs, !rs));
  rs := s

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  match Weak.get trail 0 with
  | Some r -> (r, old)
  | None ->
    let r = ref Unchanged in
    Weak.set trail 0 (Some r);
    (r, old)

let rec rev_log accu = function
  | Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
    let d = !next in
    next := Invalid;
    rev_log (ch :: accu) d

let backtrack (changes, old) =
  match !changes with
  | Unchanged -> last_snapshot := old
  | Invalid -> failwith "Btype.backtrack"
  | Change _ as change ->
    cleanup_abbrev ();
    let backlog = rev_log [] change in
    List.iter undo_change backlog;
    changes := Unchanged;
    last_snapshot := old;
    Weak.set trail 0 (Some changes)

let rec rev_compress_log log r =
  match !r with
  | Unchanged | Invalid -> log
  | Change (Ccompress _, next) -> rev_compress_log (r :: log) next
  | Change (_, next) -> rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
  | Unchanged | Invalid -> ()
  | Change _ ->
    let log = rev_compress_log [] changes in
    List.iter
      (fun r ->
        match !r with
        | Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
          ty.desc <- desc;
          r := !next
        | _ -> ())
      log
