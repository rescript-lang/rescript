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

type loc_kind = Loc_FILE | Loc_LINE | Loc_MODULE | Loc_LOC | Loc_POS

type tag_info =
  | Blk_constructor of {
      name: string;
      num_nonconst: int;
      tag: int;
      attrs: Parsetree.attributes;
    }
  | Blk_record_inlined of {
      name: string;
      num_nonconst: int;
      tag: int;
      fields: (string * bool (* optional *)) array;
      mutable_flag: Asttypes.mutable_flag;
      attrs: Parsetree.attributes;
    }
  | Blk_tuple
  | Blk_poly_var of string
  | Blk_record of {
      fields: (string * bool (* optional *)) array;
      mutable_flag: Asttypes.mutable_flag;
    }
  | Blk_module of string list
  | Blk_module_export of Ident.t list
  | Blk_extension
  | Blk_some
  | Blk_some_not_nested
    (* ['a option] where ['a] can not inhabit a non-like value *)
  | Blk_record_ext of {
      fields: string array;
      mutable_flag: Asttypes.mutable_flag;
    }
  | Blk_lazy_general

let tag_of_tag_info (tag : tag_info) =
  match tag with
  | Blk_constructor {tag} | Blk_record_inlined {tag} -> tag
  | Blk_tuple | Blk_poly_var _ | Blk_record _ | Blk_module _
  | Blk_module_export _ | Blk_extension | Blk_some (* tag not make sense *)
  | Blk_some_not_nested (* tag not make sense *)
  | Blk_lazy_general (* tag not make sense 248 *)
  | Blk_record_ext _ (* similar to Blk_extension*) ->
    0

let mutable_flag_of_tag_info (tag : tag_info) =
  match tag with
  | Blk_record_inlined {mutable_flag}
  | Blk_record {mutable_flag}
  | Blk_record_ext {mutable_flag} ->
    mutable_flag
  | Blk_lazy_general -> Mutable
  | Blk_tuple | Blk_constructor _ | Blk_poly_var _ | Blk_module _
  | Blk_module_export _ | Blk_extension | Blk_some_not_nested | Blk_some ->
    Immutable

type label = Types.label_description

let find_name (attr : Parsetree.attribute) =
  match attr with
  | ( {txt = "as"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _);
          };
        ] ) ->
    Some s
  | _ -> None

let blk_record (fields : (label * _ * _) array) mut =
  let all_labels_info =
    Ext_array.map fields (fun (lbl, _, _) ->
        ( Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name,
          lbl.lbl_optional ))
  in
  Blk_record {fields = all_labels_info; mutable_flag = mut}

let blk_record_ext fields mutable_flag =
  let all_labels_info =
    Array.map
      (fun ((lbl : label), _, _) ->
        Ext_list.find_def lbl.Types.lbl_attributes find_name lbl.lbl_name)
      fields
  in
  Blk_record_ext {fields = all_labels_info; mutable_flag}

let blk_record_inlined fields name num_nonconst ~tag ~attrs mutable_flag =
  let fields =
    Array.map
      (fun ((lbl : label), _, _) ->
        ( Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name,
          lbl.lbl_optional ))
      fields
  in
  Blk_record_inlined {fields; name; num_nonconst; tag; mutable_flag; attrs}

let ref_tag_info : tag_info =
  Blk_record {fields = [|("contents", false)|]; mutable_flag = Mutable}

type field_dbg_info =
  | Fld_record of {name: string; mutable_flag: Asttypes.mutable_flag}
  | Fld_module of {name: string}
  | Fld_record_inline of {name: string}
  | Fld_record_extension of {name: string}
  | Fld_tuple
  | Fld_poly_var_tag
  | Fld_poly_var_content
  | Fld_extension
  | Fld_variant
  | Fld_cons

let fld_record (lbl : label) =
  Fld_record
    {
      name = Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name;
      mutable_flag = lbl.lbl_mut;
    }

let fld_record_extension (lbl : label) =
  Fld_record_extension
    {name = Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name}

let ref_field_info : field_dbg_info =
  Fld_record {name = "contents"; mutable_flag = Mutable}

type set_field_dbg_info =
  | Fld_record_set of string
  | Fld_record_inline_set of string
  | Fld_record_extension_set of string

let ref_field_set_info : set_field_dbg_info = Fld_record_set "contents"
let fld_record_set (lbl : label) =
  Fld_record_set (Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name)

let fld_record_inline (lbl : label) =
  Fld_record_inline
    {name = Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name}

let fld_record_inline_set (lbl : label) =
  Fld_record_inline_set
    (Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name)

let fld_record_extension_set (lbl : label) =
  Fld_record_extension_set
    (Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name)

type immediate_or_pointer = Immediate | Pointer

type primitive =
  | Pidentity
  | Pignore
  | Pdebugger
  | Ptypeof
  | Pnull
  | Pundefined
  | Pfn_arity
  | Prevapply
  | Pdirapply
  | Ploc of loc_kind (* Globals *)
  | Pgetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of tag_info
  | Pfield of int * field_dbg_info
  | Psetfield of int * set_field_dbg_info
  | Pduprecord
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* object operations *)
  | Pobjcomp of comparison
  | Pobjorder
  | Pobjmin
  | Pobjmax
  | Pobjtag
  | Pobjsize
  (* Boolean operations *)
  | Psequand
  | Psequor
  | Pnot
  | Pboolcomp of comparison
  | Pboolorder
  | Pboolmin
  | Pboolmax
  (* Integer operations *)
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp of comparison
  | Pintorder
  | Pintmin
  | Pintmax
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pabsfloat
  | Pmodfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pfloatcomp of comparison
  | Pfloatorder
  | Pfloatmin
  | Pfloatmax
  (* BigInt operations *)
  | Pnegbigint
  | Paddbigint
  | Psubbigint
  | Ppowbigint
  | Pmulbigint
  | Pdivbigint
  | Pmodbigint
  | Pandbigint
  | Porbigint
  | Pxorbigint
  | Plslbigint
  | Pasrbigint
  | Pbigintcomp of comparison
  | Pbigintorder
  | Pbigintmin
  | Pbigintmax
  (* String operations *)
  | Pstringlength
  | Pstringrefu
  | Pstringrefs
  | Pstringcomp of comparison
  | Pstringorder
  | Pstringmin
  | Pstringmax
  | Pstringadd
  (* Array operations *)
  | Pmakearray of Asttypes.mutable_flag
  | Parraylength
  | Parrayrefu
  | Parraysetu
  | Parrayrefs
  | Parraysets
  (* List primitives *)
  | Pmakelist of Asttypes.mutable_flag
  (* dict primitives *)
  | Pmakedict
  (* promise *)
  | Pawait
  (* module *)
  | Pimport
  | Pinit_mod
  | Pupdate_mod
  (* hash *)
  | Phash
  | Phash_mixint
  | Phash_mixstring
  | Phash_finalmix
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Test if the argument is null or undefined *)
  | Pisnullable
  (* exn *)
  | Pcreate_extension of string
  | Pextension_slot_eq
  | Pwrap_exn
  (* js *)
  | Pcurry_apply of int
  | Pjscomp of comparison
  | Pundefined_to_opt
  | Pnull_to_opt
  | Pnullable_to_opt
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Pis_poly_var_block
  | Pjs_raw_expr
  | Pjs_raw_stmt
  | Pjs_fn_make of int
  | Pjs_fn_make_unit
  | Pjs_fn_method

and comparison = Ceq | Cneq | Clt | Cgt | Cle | Cge

and value_kind = Pgenval

and raise_kind = Raise_regular | Raise_reraise

type pointer_info =
  | Pt_constructor of {
      name: string;
      const: int;
      non_const: int;
      attrs: Parsetree.attributes;
    }
  | Pt_variant of {name: string}
  | Pt_module_alias
  | Pt_shape_none
  | Pt_assertfalse

type structured_constant =
  | Const_base of Asttypes.constant
  | Const_pointer of int * pointer_info
  | Const_block of tag_info * structured_constant list
  | Const_immstring of string
  | Const_false
  | Const_true
type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Default_inline (* no [@inline] attribute *)

type let_kind = Strict | Alias | StrictOpt | Variable

type function_attribute = {
  inline: inline_attribute;
  is_a_functor: bool;
  return_unit: bool;
  async: bool;
  directive: string option;
  one_unit_arg: bool;
}

type lambda =
  | Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * Location.t
  | Lswitch of lambda * lambda_switch * Location.t
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * Location.t
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * Asttypes.direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of string * lambda * Location.t

and lfunction = {
  params: Ident.t list;
  body: lambda;
  attr: function_attribute; (* specified with [@inline] attribute *)
  loc: Location.t;
}

and lambda_apply = {
  ap_func: lambda;
  ap_args: lambda list;
  ap_loc: Location.t;
  ap_inlined: inline_attribute;
}

and lambda_switch = {
  sw_numconsts: int;
  sw_consts: (int * lambda) list;
  sw_numblocks: int;
  sw_blocks: (int * lambda) list;
  sw_failaction: lambda option;
  sw_names: Ast_untagged_variants.switch_names option;
}

(* This is actually a dummy value
    not necessary "()", it can be used as a place holder for module
    alias etc.
*)
let const_unit =
  Const_pointer
    (0, Pt_constructor {name = "()"; const = 1; non_const = 0; attrs = []})

let lambda_assert_false = Lconst (Const_pointer (0, Pt_assertfalse))

let lambda_module_alias = Lconst (Const_pointer (0, Pt_module_alias))

let lambda_unit = Lconst const_unit

let default_function_attribute =
  {
    inline = Default_inline;
    is_a_functor = false;
    return_unit = false;
    async = false;
    one_unit_arg = false;
    directive = None;
  }

(* Build sharing keys *)
(*
   Those keys are later compared with Pervasives.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

let make_key e =
  let count = ref 0 (* Used for controling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count;
    if !count > max_raw then raise_notrace Not_simple;
    (* Too big ! *)
    match e with
    | Lvar id -> ( try Ident.find_same id env with Not_found -> e)
    | Lconst (Const_base (Const_string _)) ->
      (* Mutable constants are not shared *)
      raise_notrace Not_simple
    | Lconst _ -> e
    | Lapply ap ->
      Lapply
        {
          ap with
          ap_func = tr_rec env ap.ap_func;
          ap_args = tr_recs env ap.ap_args;
          ap_loc = Location.none;
        }
    | Llet (Alias, _k, x, ex, e) ->
      (* Ignore aliases -> substitute *)
      let ex = tr_rec env ex in
      tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt), _k, x, ex, Lvar v) when Ident.same v x ->
      tr_rec env ex
    | Llet (str, k, x, ex, e) ->
      (* Because of side effects, keep other lets with normalized names *)
      let ex = tr_rec env ex in
      let y = make_key x in
      Llet (str, k, y, ex, tr_rec (Ident.add x (Lvar y) env) e)
    | Lprim (p, es, _) -> Lprim (p, tr_recs env es, Location.none)
    | Lswitch (e, sw, loc) -> Lswitch (tr_rec env e, tr_sw env sw, loc)
    | Lstringswitch (e, sw, d, _) ->
      Lstringswitch
        ( tr_rec env e,
          List.map (fun (s, e) -> (s, tr_rec env e)) sw,
          tr_opt env d,
          Location.none )
    | Lstaticraise (i, es) -> Lstaticraise (i, tr_recs env es)
    | Lstaticcatch (e1, xs, e2) ->
      Lstaticcatch (tr_rec env e1, xs, tr_rec env e2)
    | Ltrywith (e1, x, e2) -> Ltrywith (tr_rec env e1, x, tr_rec env e2)
    | Lifthenelse (cond, ifso, ifnot) ->
      Lifthenelse (tr_rec env cond, tr_rec env ifso, tr_rec env ifnot)
    | Lsequence (e1, e2) -> Lsequence (tr_rec env e1, tr_rec env e2)
    | Lassign (x, e) -> Lassign (x, tr_rec env e)
    | Lsend (m, e1, _loc) -> Lsend (m, tr_rec env e1, Location.none)
    | Lletrec _ | Lfunction _ | Lfor _ | Lwhile _ -> raise_notrace Not_simple
  and tr_recs env es = List.map (tr_rec env) es
  and tr_sw env sw =
    {
      sw with
      sw_consts = List.map (fun (i, e) -> (i, tr_rec env e)) sw.sw_consts;
      sw_blocks = List.map (fun (i, e) -> (i, tr_rec env e)) sw.sw_blocks;
      sw_failaction = tr_opt env sw.sw_failaction;
    }
  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e)
  in

  try Some (tr_rec Ident.empty e) with Not_simple -> None

(***************)

let name_lambda strict arg fn =
  match arg with
  | Lvar id -> fn id
  | _ ->
    let id = Ident.create "let" in
    Llet (strict, Pgenval, id, arg, fn id)

let iter_opt f = function
  | None -> ()
  | Some e -> f e

let iter f = function
  | Lvar _ | Lconst _ -> ()
  | Lapply {ap_func = fn; ap_args = args} ->
    f fn;
    List.iter f args
  | Lfunction {body} -> f body
  | Llet (_str, _k, _id, arg, body) ->
    f arg;
    f body
  | Lletrec (decl, body) ->
    f body;
    List.iter (fun (_id, exp) -> f exp) decl
  | Lprim (_p, args, _loc) -> List.iter f args
  | Lswitch (arg, sw, _) ->
    f arg;
    List.iter (fun (_key, case) -> f case) sw.sw_consts;
    List.iter (fun (_key, case) -> f case) sw.sw_blocks;
    iter_opt f sw.sw_failaction
  | Lstringswitch (arg, cases, default, _) ->
    f arg;
    List.iter (fun (_, act) -> f act) cases;
    iter_opt f default
  | Lstaticraise (_, args) -> List.iter f args
  | Lstaticcatch (e1, _, e2) ->
    f e1;
    f e2
  | Ltrywith (e1, _, e2) ->
    f e1;
    f e2
  | Lifthenelse (e1, e2, e3) ->
    f e1;
    f e2;
    f e3
  | Lsequence (e1, e2) ->
    f e1;
    f e2
  | Lwhile (e1, e2) ->
    f e1;
    f e2
  | Lfor (_v, e1, e2, _dir, e3) ->
    f e1;
    f e2;
    f e3
  | Lassign (_, e) -> f e
  | Lsend (_k, obj, _) -> f obj

module IdentSet = Set.Make (Ident)

let free_ids get l =
  let fv = ref IdentSet.empty in
  let rec free l =
    iter free l;
    fv := List.fold_right IdentSet.add (get l) !fv;
    match l with
    | Lfunction {params} ->
      List.iter (fun param -> fv := IdentSet.remove param !fv) params
    | Llet (_str, _k, id, _arg, _body) -> fv := IdentSet.remove id !fv
    | Lletrec (decl, _body) ->
      List.iter (fun (id, _exp) -> fv := IdentSet.remove id !fv) decl
    | Lstaticcatch (_e1, (_, vars), _e2) ->
      List.iter (fun id -> fv := IdentSet.remove id !fv) vars
    | Ltrywith (_e1, exn, _e2) -> fv := IdentSet.remove exn !fv
    | Lfor (v, _e1, _e2, _dir, _e3) -> fv := IdentSet.remove v !fv
    | Lassign (id, _e) -> fv := IdentSet.add id !fv
    | Lvar _ | Lconst _ | Lapply _ | Lprim _ | Lswitch _ | Lstringswitch _
    | Lstaticraise _ | Lifthenelse _ | Lsequence _ | Lwhile _ | Lsend _ ->
      ()
  in
  free l;
  !fv

let free_variables l =
  free_ids
    (function
      | Lvar id -> [id]
      | _ -> [])
    l

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count;
  !raise_count

let negative_raise_count = ref 0

let next_negative_raise_count () =
  decr negative_raise_count;
  !negative_raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0, [])

let rec is_guarded = function
  | Lifthenelse (_cond, _body, Lstaticraise (0, [])) -> true
  | Llet (_str, _k, _id, _lam, body) -> is_guarded body
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0, [])) ->
    Lifthenelse (cond, body, patch)
  | Llet (str, k, id, lam, body) ->
    Llet (str, k, id, lam, patch_guarded patch body)
  | _ -> assert false

(* Translate an access path *)

let rec transl_normal_path = function
  | Path.Pident id ->
    if Ident.global id then Lprim (Pgetglobal id, [], Location.none)
    else Lvar id
  | Pdot (p, s, pos) ->
    Lprim
      ( Pfield (pos, Fld_module {name = s}),
        [transl_normal_path p],
        Location.none )
  | Papply _ -> assert false

(* Translation of identifiers *)

let transl_module_path ?(loc = Location.none) env path =
  transl_normal_path (Env.normalize_path (Some loc) env path)

let transl_value_path ?(loc = Location.none) env path =
  transl_normal_path (Env.normalize_path_prefix (Some loc) env path)

let transl_extension_path = transl_value_path

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst = function
    | Lvar id as l -> ( try Ident.find_same id s with Not_found -> l)
    | Lconst _ as l -> l
    | Lapply ap ->
      Lapply
        {
          ap with
          ap_func = subst ap.ap_func;
          ap_args = List.map subst ap.ap_args;
        }
    | Lfunction {params; body; attr; loc} ->
      Lfunction {params; body = subst body; attr; loc}
    | Llet (str, k, id, arg, body) -> Llet (str, k, id, subst arg, subst body)
    | Lletrec (decl, body) -> Lletrec (List.map subst_decl decl, subst body)
    | Lprim (p, args, loc) -> Lprim (p, List.map subst args, loc)
    | Lswitch (arg, sw, loc) ->
      Lswitch
        ( subst arg,
          {
            sw with
            sw_consts = List.map subst_case sw.sw_consts;
            sw_blocks = List.map subst_case sw.sw_blocks;
            sw_failaction = subst_opt sw.sw_failaction;
          },
          loc )
    | Lstringswitch (arg, cases, default, loc) ->
      Lstringswitch
        (subst arg, List.map subst_strcase cases, subst_opt default, loc)
    | Lstaticraise (i, args) -> Lstaticraise (i, List.map subst args)
    | Lstaticcatch (e1, io, e2) -> Lstaticcatch (subst e1, io, subst e2)
    | Ltrywith (e1, exn, e2) -> Ltrywith (subst e1, exn, subst e2)
    | Lifthenelse (e1, e2, e3) -> Lifthenelse (subst e1, subst e2, subst e3)
    | Lsequence (e1, e2) -> Lsequence (subst e1, subst e2)
    | Lwhile (e1, e2) -> Lwhile (subst e1, subst e2)
    | Lfor (v, e1, e2, dir, e3) -> Lfor (v, subst e1, subst e2, dir, subst e3)
    | Lassign (id, e) -> Lassign (id, subst e)
    | Lsend (k, obj, loc) -> Lsend (k, subst obj, loc)
  and subst_decl (id, exp) = (id, subst exp)
  and subst_case (key, case) = (key, subst case)
  and subst_strcase (key, case) = (key, subst case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst e)
  in
  subst lam

(* To let-bind expressions to variables *)

let bind str var exp body =
  match exp with
  | Lvar var' when Ident.same var var' -> body
  | _ -> Llet (str, Pgenval, var, exp, body)

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"

let lam_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let file, lnum, cnum = Location.get_pos_info loc_start in
  let file = Filename.basename file in
  let enum =
    loc.Location.loc_end.Lexing.pos_cnum - loc_start.Lexing.pos_cnum + cnum
  in
  match kind with
  | Loc_POS ->
    Lconst
      (Const_block
         ( Blk_tuple,
           [
             Const_immstring file;
             Const_base (Const_int lnum);
             Const_base (Const_int cnum);
             Const_base (Const_int enum);
           ] ))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Env.get_unit_name () in
    let module_name = if name = "" then "//" ^ filename ^ "//" else name in
    Lconst (Const_immstring module_name)
  | Loc_LOC ->
    let loc =
      Printf.sprintf "File %S, line %d, characters %d-%d" file lnum cnum enum
    in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum))
