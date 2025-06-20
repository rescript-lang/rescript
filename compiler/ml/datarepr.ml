(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types
open Btype

(* Simplified version of Ctype.free_vars *)
let free_vars ?(param = false) ty =
  let ret = ref TypeSet.empty in
  let rec loop ty =
    let ty = repr ty in
    if ty.level >= lowest_level then (
      ty.level <- pivot_level - ty.level;
      match ty.desc with
      | Tvar _ -> ret := TypeSet.add ty !ret
      | Tvariant row -> (
        let row = row_repr row in
        iter_row loop row;
        if not (static_row row) then
          match row.row_more.desc with
          | Tvar _ when param -> ret := TypeSet.add ty !ret
          | _ -> loop row.row_more)
      (* XXX: What about Tobject ? *)
      | _ -> iter_type_expr loop ty)
  in
  loop ty;
  unmark_type ty;
  !ret

let newgenconstr path tyl = newgenty (Tconstr (path, tyl, ref Mnil))

let constructor_existentials cd_args cd_res =
  let tyl =
    match cd_args with
    | Cstr_tuple l -> l
    | Cstr_record l -> List.map (fun l -> l.ld_type) l
  in
  let existentials =
    match cd_res with
    | None -> []
    | Some type_ret ->
      let arg_vars_set = free_vars (newgenty (Ttuple tyl)) in
      let res_vars = free_vars type_ret in
      TypeSet.elements (TypeSet.diff arg_vars_set res_vars)
  in
  (tyl, existentials)

let constructor_args priv cd_args cd_res path rep =
  let tyl, existentials = constructor_existentials cd_args cd_res in
  match cd_args with
  | Cstr_tuple l -> (existentials, l, None)
  | Cstr_record lbls ->
    let arg_vars_set = free_vars ~param:true (newgenty (Ttuple tyl)) in
    let type_params = TypeSet.elements arg_vars_set in
    let type_unboxed =
      match rep with
      | Record_unboxed _ -> unboxed_true_default_false
      | _ -> unboxed_false_default_false
    in
    let tdecl =
      {
        type_params;
        type_arity = List.length type_params;
        type_kind = Type_record (lbls, rep);
        type_private = priv;
        type_manifest = None;
        type_variance = List.map (fun _ -> Variance.full) type_params;
        type_newtype_level = None;
        type_loc = Location.none;
        type_attributes = [];
        type_immediate = false;
        type_unboxed;
        type_inlined_types = [];
      }
    in
    (existentials, [newgenconstr path type_params], Some tdecl)

let internal_optional = "internal.optional"

let optional_shape : Parsetree.attribute =
  ({txt = internal_optional; loc = Location.none}, Parsetree.PStr [])

let constructor_has_optional_shape
    ({cstr_attributes = attrs} : constructor_description) =
  List.exists (fun (x, _) -> x.txt = internal_optional) attrs

let constructor_descrs ty_path decl cstrs =
  let ty_res = newgenconstr ty_path decl.type_params in
  let num_consts = ref 0 and num_nonconsts = ref 0 in
  List.iter
    (fun {cd_args; _} ->
      if cd_args = Cstr_tuple [] then incr num_consts else incr num_nonconsts)
    cstrs;
  let rec describe_constructors idx_const idx_nonconst = function
    | [] -> []
    | {cd_id; cd_args; cd_res; cd_loc; cd_attributes} :: rem ->
      let ty_res =
        match cd_res with
        | Some ty_res' -> ty_res'
        | None -> ty_res
      in
      let tag, descr_rem =
        match cd_args with
        | _ when decl.type_unboxed.unboxed ->
          assert (rem = []);
          (Cstr_unboxed, [])
        | Cstr_tuple [] ->
          ( Cstr_constant idx_const,
            describe_constructors (idx_const + 1) idx_nonconst rem )
        | _ ->
          ( Cstr_block idx_nonconst,
            describe_constructors idx_const (idx_nonconst + 1) rem )
      in
      let cstr_name = Ident.name cd_id in
      let existentials, cstr_args, cstr_inlined =
        let representation =
          if decl.type_unboxed.unboxed then Record_unboxed true
          else
            Record_inlined
              {
                tag = idx_nonconst;
                name = cstr_name;
                num_nonconsts = !num_nonconsts;
                attrs = cd_attributes;
              }
        in
        constructor_args decl.type_private cd_args cd_res
          (Path.Pdot (ty_path, cstr_name, Path.nopos))
          representation
      in
      let cstr =
        {
          cstr_name;
          cstr_res = ty_res;
          cstr_existentials = existentials;
          cstr_args;
          cstr_arity = List.length cstr_args;
          cstr_tag = tag;
          cstr_consts = !num_consts;
          cstr_nonconsts = !num_nonconsts;
          cstr_private = decl.type_private;
          cstr_generalized = cd_res <> None;
          cstr_loc = cd_loc;
          cstr_attributes = cd_attributes;
          cstr_inlined;
        }
      in
      (cd_id, cstr) :: descr_rem
  in
  let result = describe_constructors 0 0 cstrs in
  match result with
  | [
      (({Ident.name = "None"} as a_id), ({cstr_args = []} as a_descr));
      (({Ident.name = "Some"} as b_id), ({cstr_args = [_]} as b_descr));
    ]
  | [
      (({Ident.name = "Some"} as a_id), ({cstr_args = [_]} as a_descr));
      (({Ident.name = "None"} as b_id), ({cstr_args = []} as b_descr));
    ] ->
    [
      ( a_id,
        {
          a_descr with
          cstr_attributes = optional_shape :: a_descr.cstr_attributes;
        } );
      ( b_id,
        {
          b_descr with
          cstr_attributes = optional_shape :: b_descr.cstr_attributes;
        } );
    ]
  | _ -> result

let extension_descr path_ext ext =
  let ty_res =
    match ext.ext_ret_type with
    | Some type_ret -> type_ret
    | None -> newgenconstr ext.ext_type_path ext.ext_type_params
  in
  let existentials, cstr_args, cstr_inlined =
    constructor_args ext.ext_private ext.ext_args ext.ext_ret_type path_ext
      Record_extension
  in
  {
    cstr_name = Path.last path_ext;
    cstr_res = ty_res;
    cstr_existentials = existentials;
    cstr_args;
    cstr_arity = List.length cstr_args;
    cstr_tag = Cstr_extension path_ext;
    cstr_consts = -1;
    cstr_nonconsts = -1;
    cstr_private = ext.ext_private;
    cstr_generalized = ext.ext_ret_type <> None;
    cstr_loc = ext.ext_loc;
    cstr_attributes = ext.ext_attributes;
    cstr_inlined;
  }

let none = {desc = Ttuple []; level = -1; id = -1}
(* Clearly ill-formed type *)

let dummy_label =
  {
    lbl_name = "";
    lbl_res = none;
    lbl_arg = none;
    lbl_mut = Immutable;
    lbl_optional = false;
    lbl_pos = -1;
    lbl_all = [||];
    lbl_repres = Record_regular;
    lbl_private = Public;
    lbl_loc = Location.none;
    lbl_attributes = [];
  }

let label_descrs ty_res lbls repres priv =
  let all_labels = Array.make (List.length lbls) dummy_label in
  let rec describe_labels num = function
    | [] -> []
    | l :: rest ->
      let lbl =
        {
          lbl_name = Ident.name l.ld_id;
          lbl_res = ty_res;
          lbl_arg = l.ld_type;
          lbl_mut = l.ld_mutable;
          lbl_optional = l.ld_optional;
          lbl_pos = num;
          lbl_all = all_labels;
          lbl_repres = repres;
          lbl_private = priv;
          lbl_loc = l.ld_loc;
          lbl_attributes = l.ld_attributes;
        }
      in
      all_labels.(num) <- lbl;
      (l.ld_id, lbl) :: describe_labels (num + 1) rest
  in
  describe_labels 0 lbls

exception Constr_not_found

let rec find_constr tag num_const num_nonconst = function
  | [] -> raise Constr_not_found
  | ({cd_args = Cstr_tuple []; _} as c) :: rem ->
    if Types.equal_tag tag (Cstr_constant num_const) then c
    else find_constr tag (num_const + 1) num_nonconst rem
  | c :: rem ->
    if Types.equal_tag tag (Cstr_block num_nonconst) || tag = Cstr_unboxed then
      c
    else find_constr tag num_const (num_nonconst + 1) rem

let find_constr_by_tag tag cstrlist = find_constr tag 0 0 cstrlist

let constructors_of_type ty_path decl =
  match decl.type_kind with
  | Type_variant cstrs -> constructor_descrs ty_path decl cstrs
  | Type_record _ | Type_abstract | Type_open -> []

let labels_of_type ty_path decl =
  match decl.type_kind with
  | Type_record (labels, rep) ->
    label_descrs
      (newgenconstr ty_path decl.type_params)
      labels rep decl.type_private
  | Type_variant _ | Type_abstract | Type_open -> []

(* Set row_name in Env, cf. GPR#1204/1329 *)
let set_row_name decl path =
  match decl.type_manifest with
  | None -> ()
  | Some ty -> (
    let ty = repr ty in
    match ty.desc with
    | Tvariant row when static_row row ->
      let row =
        {(row_repr row) with row_name = Some (path, decl.type_params)}
      in
      ty.desc <- Tvariant row
    | _ -> ())
