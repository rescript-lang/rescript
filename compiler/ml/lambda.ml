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

type hoisted_function = {binding: Ident.t; path: string list; loc: Location.t}

type tag_info =
  | Blk_constructor of {
      name: string;
      num_nonconst: int;
      runtime: Variant_runtime.block_runtime;
    }
  | Blk_record_inlined of {
      name: string;
      num_nonconst: int;
      fields: (string * bool (* optional *)) array;
      mutable_flag: Asttypes.mutable_flag;
      runtime: Variant_runtime.block_runtime;
    }
  | Blk_tuple
  | Blk_poly_var
  | Blk_record of {
      fields: (string * bool (* optional *)) array;
      mutable_flag: Asttypes.mutable_flag;
    }
  | Blk_module of string list
  | Blk_module_export of Ident.t list
  | Blk_extension
  | Blk_record_ext of {
      fields: string array;
      mutable_flag: Asttypes.mutable_flag;
    }

(* Label used by the lambda printer for a block; blocks carry no
   numeric tag anymore *)
let tag_label_of_tag_info (tag : tag_info) =
  match tag with
  | Blk_constructor {name} | Blk_record_inlined {name} -> name
  | Blk_tuple | Blk_poly_var | Blk_record _ | Blk_module _ | Blk_module_export _
  | Blk_extension | Blk_record_ext _ ->
    "0"

let mutable_flag_of_tag_info (tag : tag_info) =
  match tag with
  | Blk_record_inlined {mutable_flag}
  | Blk_record {mutable_flag}
  | Blk_record_ext {mutable_flag} ->
    mutable_flag
  | Blk_tuple | Blk_constructor _ | Blk_poly_var | Blk_module _
  | Blk_module_export _ | Blk_extension ->
    Immutable

type label = Types.label_description

let find_name (({txt}, payload) : Parsetree.attribute) =
  if txt = "as" then Ast_payload.semantic_string_of_payload payload else None

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

let blk_record_inlined fields name num_nonconst ~runtime mutable_flag =
  let fields =
    Array.map
      (fun ((lbl : label), _, _) ->
        ( Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name,
          lbl.lbl_optional ))
      fields
  in
  Blk_record_inlined {fields; name; num_nonconst; mutable_flag; runtime}

let ref_tag_info : tag_info =
  Blk_record {fields = [|("contents", false)|]; mutable_flag = Mutable}

type field_dbg_info =
  | Fld_record of {name: string}
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
    {name = Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name}

let fld_record_extension (lbl : label) =
  Fld_record_extension
    {name = Ext_list.find_def lbl.lbl_attributes find_name lbl.lbl_name}

let ref_field_info : field_dbg_info = Fld_record {name = "contents"}

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

(* The target of a dynamic [import], resolved at translation: the argument
   of the import primitive is a module reference, never an expression. *)
type import_source =
  | Import_module of {module_: Ident.t; path: string list}
    (* a ReScript module, or a value/submodule reached from one
         ([path = []]: the module itself). The module is resolved here, at
         translation; the JS export name for a nested [path] is resolved
         where the cmj tables live, at emission. *)
  | Import_external of {
      module_: External_ffi_types.external_module_name;
      path: string list;
          (* access path inside the module: @scope segments then the
             name; [] means the external is the module itself *)
    }

(* `%identity` / `%ignore` / unary `+`: builtins that erase at translation
   rather than primitives. See [builtin]. *)
type eliminated = Identity | Ignore

type primitive =
  | Pdebugger
  | Ptypeof
  | Psome
  | Psome_not_nest
      (** [Some x] where [x] cannot itself be [undefined], so no wrapping is
          needed. *)
  (* Operations on heap blocks *)
  | Pmakeblock of tag_info
  | Pfield of int * field_dbg_info
  | Psetfield of int * set_field_dbg_info
  | Pduprecord
  | Precord_rest of string list (* excluded runtime field names *)
  (* JS FFI calls, expanded from the external's spec at translation *)
  | Pjs_call of {
      prim_name: string;
      arg_types: External_arg_spec.params;
      ffi: External_ffi_types.external_decl;
      transformed_jsx: bool;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  | Pjs_object_get of string
  | Pjs_object_set of string
  (* Exceptions *)
  | Praise
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
  | Ppowint
  | Pandint
  | Porint
  | Pxorint
  | Pnotint
  | Plslint
  | Plsrint
  | Pasrint
  | Pintcomp of comparison
  | Pintorder
  | Pintmin
  | Pintmax
  (* Float operations *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Pmodfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Ppowfloat
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
  | Pnotbigint
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
  | Pmakearray
  | Parraylength
  | Parrayrefu
  | Parraysetu
  | Parrayrefs
  | Parraysets
  (* List primitives *)
  | Pmakelist
  (* dict primitives *)
  | Pmakedict
  | Pdict_has
  (* promise *)
  | Pawait
  (* module *)
  | Pimport of import_source
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
  (* Test if the argument is null or undefined *)
  | Pis_null_undefined
  (* exn *)
  | Pcreate_extension of string
  (* js *)
  | Pjscomp of comparison
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  (* Produced by Lam_pass_remove_alias, not by translation *)
  | Pis_null
  | Pis_undefined
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Pis_poly_var_block
  (* Validated JavaScript source from [raw], [ffi], or [re], together with its
     expression/program kind. For example, [%raw("x + 1")] carries ["x + 1"]
     as code, not as a decoded runtime string. *)
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_method
  (* A JavaScript tagged template operation. For [sql`id = ${id}`], the payload
     is ["id = "; ""] and the primitive arguments are [sql; id]. Segment text
     remains raw and may contain invalid escapes. *)
  | Ptagged_template of string list
  (* An ordinary backquoted-template operation. For [`a ${value}\n`], the
     payload contains the source and semantic forms of ["a "] and ["\\n"],
     and the primitive arguments contain [value]. The source forms are retained
     for JavaScript output; semantic forms are used by optimizations. *)
  | Ptemplate of Asttypes.template_segment list

and comparison = Ceq | Cneq | Clt | Cgt | Cle | Cge

type structured_constant =
  | Const_int of int32
  | Const_char of int
    (* The decoded Unicode code point; literal source spelling is no longer
       present at this layer. *)
  | Const_string of string
    (* A decoded runtime string value; literal source spelling is no longer
       present at this layer. *)
  | Const_float of string
  | Const_bigint of bool * string
  | Const_block of tag_info * structured_constant list
  | Const_constructor of Variant_runtime.tag
      (** Constant constructor of a nominal variant, from its canonical
          runtime descriptor. Integer-represented ones are [Const_int]. *)
  | Const_polyvar of string
      (** Tagless polymorphic variant; numeric-looking names are [Const_int]. *)
  | Const_assertfalse
  | Const_module_alias
  | Const_js_false
  | Const_js_true
  | Const_js_null
  | Const_some of structured_constant
  | Const_js_undefined of {is_unit: bool}
      (** [is_unit] tells the unit value apart from JS [undefined]; both emit
          [undefined]. *)

(* What a `%builtin` name in the primitive table means. Only [Primitive]
   reaches the IR: [mk_builtin] erases the other cases at translation, so
   they need no [primitive] constructor to stand in for them. *)
type builtin =
  | Primitive of primitive
  | Eliminated of eliminated
  | Constant of structured_constant
  | Offset_ref of int
      (** [%incr] / [%decr]: an assignment through the reference, expanded here
          so the caller's own IR carries the form its escape analysis reads. *)

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

type t =
  | Lvar of Ident.t
  | Lglobal_module of Ident.t
      (** A reference to another compilation unit: a name the module system
          resolves, not a value this one computes. *)
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * Ident.t * t * t
  | Lletrec of (Ident.t * t) list * t
  | Lprim of prim_info
  | Lswitch of t * lambda_switch
  | Lstringswitch of t * (string * t) list * t option
  | Lstaticraise of int * t list
  | Lstaticcatch of t * (int * Ident.t list) * t
  | Ltrywith of t * Ident.t * t
  | Lifthenelse of t * t * t
  | Lsequence of t * t
  | Lbreak
  | Lcontinue
  | Lwhile of t * t
  | Lfor of Ident.t * t * t * Asttypes.direction_flag * t
  | Lfor_of of Ident.t * t * t
  | Lfor_await_of of Ident.t * t * t
  | Lassign of Ident.t * t

and lfunction = {
  params: Ident.t list;
  body: t;
  attr: function_attribute; (* specified with [@inline] attribute *)
  loc: Location.t;
}

and prim_info = {primitive: primitive; args: t list; loc: Location.t}

and ap_info = {ap_loc: Location.t; ap_inlined: inline_attribute}

and lambda_apply = {
  ap_func: t;
  ap_args: t list;
  ap_info: ap_info;
  ap_transformed_jsx: bool;
}

and switch_key =
  | Switch_int of int
  | Switch_constructor of Variant_runtime.constructor_case

and switch_dispatch =
  | Switch_direct
  | Switch_variant of Variant_runtime.matching_facts

and 'a switch = {
  sw_consts_full: bool;
  sw_consts: (switch_key * 'a) list;
  sw_blocks_full: bool;
  sw_blocks: (switch_key * 'a) list;
  sw_failaction: 'a option;
  sw_dispatch: switch_dispatch;
}

and lambda_switch = t switch

(* This is actually a dummy value
    not necessary "()", it can be used as a place holder for module
    alias etc.
*)
let const_int (i : int) = Const_int (Int32.of_int i)

let const_string s = Const_string (String_literal.normalize_semantic s)

let const_of_typed (c : Asttypes.constant) : structured_constant =
  match c with
  | Asttypes.Const_int i -> Const_int (Int32.of_int i)
  | Asttypes.Const_char i -> Const_char i
  | Asttypes.Const_string s -> Const_string s
  | Asttypes.Const_float f -> Const_float f
  | Asttypes.Const_bigint (sign, i) -> Const_bigint (sign, i)

let const_unit = Const_js_undefined {is_unit = true}

(* The JS value of a constant constructor: unit has its own constant, and a
   constructor represented as a number is a genuine number at runtime, so
   folding sees it as an ordinary integer. *)
let const_constructor (tag : Variant_runtime.tag) =
  if tag.name = "()" then const_unit
  else
    match tag.tag_type with
    | Some (Variant_runtime.Int v) -> Const_int (Int32.of_int v)
    | _ -> Const_constructor tag

(* A constructor with an optional shape carries no payload when constant. *)
let const_shape_none = Const_js_undefined {is_unit = false}

(* The JS value of a polymorphic variant's name: a numeric-looking name is a
   number at runtime, anything else is a string. Used both for a tagless
   variant and for the name field of one carrying a payload. *)
let const_polyvar name =
  if Ext_string.is_valid_hash_number name then
    Const_int (Ext_string.hash_number_as_i32_exn name)
  else Const_polyvar name

let const_polyvar_name name =
  match const_polyvar name with
  | Const_polyvar s -> Const_string s
  | c -> c

let const_module_alias = Const_module_alias

let lambda_assert_false = Lconst Const_assertfalse

let lambda_module_alias = Lconst const_module_alias

let lambda_unit = Lconst const_unit
let lambda_true = Lconst Const_js_true
let lambda_false = Lconst Const_js_false

(* [r := r.contents + delta]. The reference is mentioned twice, so bind it
   unless it is already a variable. *)
let offset_ref ~delta r loc =
  let assign r =
    Lprim
      {
        primitive = Psetfield (0, ref_field_set_info);
        args =
          [
            r;
            Lprim
              {
                primitive = Paddint;
                args =
                  [
                    Lprim
                      {primitive = Pfield (0, ref_field_info); args = [r]; loc};
                    Lconst (const_int delta);
                  ];
                loc;
              };
          ];
        loc;
      }
  in
  match r with
  | Lvar _ -> assign r
  | _ ->
    let id = Ident.create "ref" in
    Llet (Strict, id, r, assign (Lvar id))

let eq_comparison (p : comparison) (p1 : comparison) = p = p1

let eq_field_dbg_info (x : field_dbg_info) (y : field_dbg_info) = x = y
let eq_set_field_dbg_info (x : set_field_dbg_info) (y : set_field_dbg_info) =
  x = y

let eq_tag_info (x : tag_info) y = x = y

let eq_primitive_approx (lhs : primitive) (rhs : primitive) =
  match lhs with
  | Praise
  (* generic comparison *)
  | Pobjorder | Pobjmin | Pobjmax | Pobjtag | Pobjsize
  (* bool primitives *)
  | Psequand | Psequor | Pnot | Pboolcomp _ | Pboolorder | Pboolmin | Pboolmax
  (* int primitives *)
  | Pisint | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint | Ppowint
  | Pnotint | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
  | Pintorder | Pintmin | Pintmax
  (* float primitives *)
  | Pintoffloat | Pfloatofint | Pnegfloat | Paddfloat | Psubfloat | Pmulfloat
  | Pdivfloat | Pmodfloat | Ppowfloat | Pfloatorder | Pfloatmin | Pfloatmax
  (* bigint primitives *)
  | Pnegbigint | Paddbigint | Psubbigint | Pmulbigint | Pdivbigint | Pmodbigint
  | Ppowbigint | Pnotbigint | Pandbigint | Porbigint | Pxorbigint | Plslbigint
  | Pasrbigint | Pbigintorder | Pbigintmin | Pbigintmax
  (* string primitives *)
  | Pstringlength | Pstringrefu | Pstringrefs | Pstringadd | Pstringcomp _
  | Pstringorder | Pstringmin | Pstringmax
  (* List primitives *)
  | Pmakelist
  (* dict primitives *)
  | Pmakedict | Pdict_has
  (* promise *)
  | Pawait
  (* etc *)
  | Pval_from_option | Pval_from_option_not_nest | Pnull_to_opt
  | Pnull_undefined_to_opt | Pis_null | Pis_not_none | Psome | Psome_not_nest
  | Pis_undefined | Pis_null_undefined | Ptypeof | Pis_poly_var_block
  | Pdebugger | Pinit_mod | Pupdate_mod | Pduprecord | Pmakearray | Parraylength
  | Parrayrefu | Parraysetu | Parrayrefs | Parraysets | Pjs_fn_method | Phash
  | Phash_mixstring | Phash_mixint | Phash_finalmix | Precord_rest _ ->
    rhs = lhs
  (* Reachable only via the optimizer's term-equality comparison, which the
     test suite doesn't exercise for template primitives. *)
  | Ptagged_template _ | Ptemplate _ -> ( ((rhs = lhs) [@coverage off]))
  | Pcreate_extension a -> (
    match rhs with
    | Pcreate_extension b -> a = (b : string)
    | _ -> false)
  (* | Pcaml_obj_set_length -> rhs = Pcaml_obj_set_length *)
  | Pfield (n0, info0) -> (
    match rhs with
    | Pfield (n1, info1) -> n0 = n1 && eq_field_dbg_info info0 info1
    | _ -> false)
  | Psetfield (i0, info0) -> (
    match rhs with
    | Psetfield (i1, info1) -> i0 = i1 && eq_set_field_dbg_info info0 info1
    | _ -> false)
  | Pmakeblock info0 -> (
    match rhs with
    | Pmakeblock info1 -> eq_tag_info info0 info1
    | _ -> false)
  | Pjs_call {prim_name; arg_types; ffi; _} -> (
    match rhs with
    | Pjs_call rhs ->
      prim_name = rhs.prim_name && arg_types = rhs.arg_types && ffi = rhs.ffi
    | _ -> false)
  | Pimport src -> (
    match rhs with
    | Pimport src2 -> src = src2
    | _ -> false)
  | Pjs_object_create obj_create -> (
    match rhs with
    | Pjs_object_create obj_create1 -> obj_create = obj_create1
    | _ -> false)
  | Pobjcomp comparison -> (
    match rhs with
    | Pobjcomp comparison1 -> eq_comparison comparison comparison1
    | _ -> false)
  | Pintcomp comparison -> (
    match rhs with
    | Pintcomp comparison1 -> eq_comparison comparison comparison1
    | _ -> false)
  | Pfloatcomp comparison -> (
    match rhs with
    | Pfloatcomp comparison1 -> eq_comparison comparison comparison1
    | _ -> false)
  | Pbigintcomp comparison -> (
    match rhs with
    | Pbigintcomp comparison1 -> eq_comparison comparison comparison1
    | _ -> false)
  | Pjscomp comparison -> (
    match rhs with
    | Pjscomp comparison1 -> eq_comparison comparison comparison1
    | _ -> false)
  | Pjs_object_get name -> (
    match rhs with
    | Pjs_object_get rhs_name -> name = rhs_name
    | _ -> false)
  | Pjs_object_set name -> (
    match rhs with
    | Pjs_object_set rhs_name -> name = rhs_name
    | _ -> false)
  | Praw_js_code _ -> false
(* TOO lazy, here comparison is only approximation*)

(* The source-level name a field access carries, when it has one. *)
let str_of_field_info (x : field_dbg_info) : string option =
  match x with
  | Fld_extension | Fld_variant | Fld_cons | Fld_poly_var_tag
  | Fld_poly_var_content | Fld_tuple ->
    None
  | Fld_record {name}
  | Fld_module {name}
  | Fld_record_inline {name}
  | Fld_record_extension {name} ->
    Some name

let is_immutable_block (info : tag_info) =
  mutable_flag_of_tag_info info = Immutable

(* A constant that has to be built at run time rather than shared. *)
let rec const_is_allocating (c : structured_constant) : bool =
  match c with
  | Const_some t -> const_is_allocating t
  | Const_block _ -> true
  | Const_js_null | Const_js_undefined _ | Const_js_true | Const_js_false
  | Const_int _ | Const_assertfalse | Const_constructor _ | Const_char _
  | Const_string _ | Const_float _ | Const_bigint _ | Const_polyvar _
  | Const_module_alias ->
    false

let rec const_eq_approx (x : structured_constant) (y : structured_constant) =
  match x with
  | Const_module_alias -> y = Const_module_alias
  | Const_js_null -> y = Const_js_null
  | Const_js_undefined b -> y = Const_js_undefined b
  | Const_js_true -> y = Const_js_true
  | Const_js_false -> y = Const_js_false
  | Const_int ix -> (
    match y with
    | Const_int iy -> ix = iy
    | _ -> false)
  | Const_assertfalse -> y = Const_assertfalse
  | Const_constructor ix -> (
    match y with
    | Const_constructor iy -> ix = iy
    | _ -> false)
  | Const_char ix -> (
    match y with
    | Const_char iy -> ix = iy
    | _ -> false)
  | Const_string sx -> (
    match y with
    | Const_string sy -> sx = sy
    | _ -> false)
  | Const_float ix -> (
    match y with
    | Const_float iy -> ix = iy
    | _ -> false)
  | Const_bigint (sx, ix) -> (
    match y with
    | Const_bigint (sy, iy) -> sx = sy && ix = iy
    | _ -> false)
  | Const_polyvar ix -> (
    match y with
    | Const_polyvar iy -> ix = iy
    | _ -> false)
  | Const_block (ix, ixs) -> (
    match y with
    | Const_block (iy, iys) ->
      ix = iy && Ext_list.for_all2_no_exn ixs iys const_eq_approx
    | _ -> false)
  | Const_some ix -> (
    match y with
    | Const_some iy -> const_eq_approx ix iy
    | _ -> false)

let cmp_int32 (cmp : comparison) (a : int32) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_float (cmp : comparison) (a : float) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

(* Constructors. The type is private outside this module, so every term is
   built through one of these. They are plain for now; the normalizations that
   Lambda.prim / Lambda.if_ / Lambda.switch perform will move here when the two layers
   become one type. *)

let var id : t = Lvar id
let global_module id : t = Lglobal_module id
let const ct : t = Lconst ct

let function_ ~loc ~attr ~params ~body : t = Lfunction {params; body; attr; loc}

let let_ kind id e body : t = Llet (kind, id, e, body)
let letrec bindings body : t = Lletrec (bindings, body)

let staticraise i args : t = Lstaticraise (i, args)
let staticcatch body catch handler : t = Lstaticcatch (body, catch, handler)
let try_ body id handler : t = Ltrywith (body, id, handler)
let break : t = Lbreak
let continue : t = Lcontinue
let while_ cond body : t = Lwhile (cond, body)
let for_ id from_ to_ dir body : t = Lfor (id, from_, to_, dir, body)
let for_of id iterable body : t = Lfor_of (id, iterable, body)

let for_await_of id iterable body : t = Lfor_await_of (id, iterable, body)

let assign id body : t = Lassign (id, body)

exception Not_simple_form

(**


   [is_eta_conversion_exn params inner_args outer_args]
   case 1:
   {{
    (fun params -> wrap (primitive (inner_args)) args
   }}
   when [inner_args] are the same as [params], it can be simplified as
   [wrap (primitive args)]

    where [wrap] used to be simple instructions
    Note that [external] functions are forced to do eta-conversion
    when combined with [|>] operator, we need to make sure beta-reduction
    is applied though since `[@variadic]` needs such guarantee.
    Since `[@variadic] is the tail position
*)
let rec is_eta_conversion_exn params inner_args outer_args : t list =
  match (params, inner_args, outer_args) with
  | x :: xs, Lvar y :: ys, r :: rest when Ident.same x y ->
    r :: is_eta_conversion_exn xs ys rest
  | [], [], [] -> []
  | _, _, _ -> raise_notrace Not_simple_form

let rec apply ?(ap_transformed_jsx = false) fn args (ap_info : ap_info) : t =
  match fn with
  | Lfunction
      {
        params;
        body =
          Lprim
            {
              primitive =
                ( Pnull_to_opt | Pnull_undefined_to_opt | Pis_null
                | Pis_null_undefined | Ptypeof ) as wrap;
              args =
                [Lprim ({primitive = _; args = inner_args} as primitive_call)];
            };
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args ->
      let loc = ap_info.ap_loc in
      Lprim
        {primitive = wrap; args = [Lprim {primitive_call with args; loc}]; loc}
    | exception Not_simple_form ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx})
  | Lfunction
      {
        params;
        body = Lprim ({primitive = _; args = inner_args} as primitive_call);
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args -> Lprim {primitive_call with args; loc = ap_info.ap_loc}
    | exception _ ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx})
  | Lfunction
      {
        params;
        body =
          Lsequence
            ( Lprim ({primitive = _; args = inner_args} as primitive_call),
              (Lconst _ as const) );
      } -> (
    match is_eta_conversion_exn params inner_args args with
    | args ->
      Lsequence (Lprim {primitive_call with args; loc = ap_info.ap_loc}, const)
    | exception _ ->
      Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx}
      (* | Lfunction {params;body} when Ext_list.same_length params args ->
          Ext_list.fold_right2 (fun p arg acc ->
            Llet(Strict,p,arg,acc)
          ) params args body *)
      (* TODO: more rigirous analysis on [let_kind] *))
  | Llet (kind, id, e, (Lfunction _ as fn)) ->
    Llet (kind, id, e, apply fn args ap_info ~ap_transformed_jsx)
  (* | Llet (kind0, id0, e0, Llet (kind,id, e, (Lfunction _ as fn))) ->
     Llet(kind0,id0,e0,Llet (kind, id, e, apply fn args loc status)) *)
  | _ -> Lapply {ap_func = fn; ap_args = args; ap_info; ap_transformed_jsx}

let rec eq_approx (l1 : t) (l2 : t) =
  match l1 with
  | Lglobal_module i1 -> (
    match l2 with
    | Lglobal_module i2 -> Ident.same i1 i2
    | _ -> false)
  | Lvar i1 -> (
    match l2 with
    | Lvar i2 -> Ident.same i1 i2
    | _ -> false)
  | Lconst c1 -> (
    match l2 with
    | Lconst c2 -> const_eq_approx c1 c2
    | _ -> false)
  | Lapply app1 -> (
    match l2 with
    | Lapply app2 ->
      eq_approx app1.ap_func app2.ap_func
      && eq_approx_list app1.ap_args app2.ap_args
    | _ -> false)
  | Lifthenelse (a, b, c) -> (
    match l2 with
    | Lifthenelse (a0, b0, c0) ->
      eq_approx a a0 && eq_approx b b0 && eq_approx c c0
    | _ -> false)
  | Lsequence (a, b) -> (
    match l2 with
    | Lsequence (a0, b0) -> eq_approx a a0 && eq_approx b b0
    | _ -> false)
  | Lbreak -> l2 = Lbreak
  | Lcontinue -> l2 = Lcontinue
  | Lwhile (p, b) -> (
    match l2 with
    | Lwhile (p0, b0) -> eq_approx p p0 && eq_approx b b0
    | _ -> false)
  | Lassign (v0, l0) -> (
    match l2 with
    | Lassign (v1, l1) -> Ident.same v0 v1 && eq_approx l0 l1
    | _ -> false)
  | Lstaticraise (id, ls) -> (
    match l2 with
    | Lstaticraise (id1, ls1) -> id = id1 && eq_approx_list ls ls1
    | _ -> false)
  | Lprim info1 -> (
    match l2 with
    | Lprim info2 ->
      eq_primitive_approx info1.primitive info2.primitive
      && eq_approx_list info1.args info2.args
    | _ -> false)
  | Lstringswitch (arg, patterns, default) -> (
    match l2 with
    | Lstringswitch (arg2, patterns2, default2) ->
      eq_approx arg arg2 && eq_option default default2
      && Ext_list.for_all2_no_exn patterns patterns2
           (fun ((k : string), v) (k2, v2) -> k = k2 && eq_approx v v2)
    | _ -> false)
  | Lfunction _
  | Llet (_, _, _, _)
  | Lletrec _ | Lswitch _ | Lstaticcatch _ | Ltrywith _
  | Lfor (_, _, _, _, _)
  | Lfor_of (_, _, _)
  | Lfor_await_of (_, _, _) ->
    false

and eq_option l1 l2 =
  match l1 with
  | None -> l2 = None
  | Some l1 -> (
    match l2 with
    | Some l2 -> eq_approx l1 l2
    | None -> false)

and eq_approx_list ls ls1 = Ext_list.for_all2_no_exn ls ls1 eq_approx

let switch lam (lam_switch : lambda_switch) : t =
  let action_or_switch = function
    | Some action -> action
    | None -> (
      match lam_switch.sw_failaction with
      | Some action -> action
      | None -> Lswitch (lam, lam_switch))
  in
  match lam with
  | Lconst (Const_constructor cstr_name) ->
    let action =
      Ext_list.find_opt lam_switch.sw_consts (fun (key, action) ->
          match key with
          | Switch_constructor (Constant tag) when cstr_name = tag ->
            Some action
          | Switch_int _ | Switch_constructor _ -> None)
    in
    action_or_switch action
  | Lconst (Const_int i) ->
    (* Because of inlining and dead code, we might be looking at a value of unexpected type
       e.g. an integer, so the const case might not be found *)
    let i = Int32.to_int i in
    let action =
      Ext_list.find_opt lam_switch.sw_consts (fun (key, action) ->
          match key with
          | Switch_int ordinal when ordinal = i -> Some action
          | Switch_constructor
              (Constant {tag_type = Some (Variant_runtime.Int value)})
            when value = i ->
            Some action
          | Switch_int _ | Switch_constructor _ -> None)
    in
    action_or_switch action
  | Lconst (Const_block (tag_info, _)) ->
    let runtime =
      match tag_info with
      | Blk_constructor {runtime} | Blk_record_inlined {runtime} -> Some runtime
      | Blk_tuple | Blk_poly_var | Blk_record _ | Blk_record_ext _
      | Blk_module _ | Blk_module_export _ | Blk_extension ->
        None
    in
    let action =
      Ext_list.find_opt lam_switch.sw_blocks (fun (key, action) ->
          match key with
          | Switch_constructor (Block {runtime = case_runtime})
            when runtime = Some case_runtime ->
            Some action
          | Switch_int _ | Switch_constructor _ -> None)
    in
    action_or_switch action
  | _ -> Lswitch (lam, lam_switch)

let stringswitch (lam : t) cases default : t =
  match lam with
  | Lconst (Const_string s) -> Ext_list.assoc_by_string cases s default
  | _ -> Lstringswitch (lam, cases, default)

let rec seq (a : t) b : t =
  match a with
  | Lprim {primitive = Pmakeblock _; args = x :: xs} ->
    seq (Ext_list.fold_left xs x seq) b
  | Lprim {primitive = Pnull_to_opt | Pnull_undefined_to_opt; args = [a]} ->
    seq a b
  | _ -> Lsequence (a, b)

module Lift = struct
  let int i : t = Lconst (Const_int i)

  let bool b = if b then lambda_true else lambda_false

  let string s : t = Lconst (Const_string s)

  let char b : t = Lconst (Const_char b)
end

let prim ~primitive:(prim : primitive) ~args loc : t =
  let default () : t = Lprim {primitive = prim; args; loc} in
  match args with
  | [Lconst a] -> (
    match (prim, a) with
    | Pnegint, Const_int i -> Lift.int (Int32.neg i)
    (* | Pfloatofint, ( (Const_int a)) *)
    (*   -> Lift.float (float_of_int a) *)
    | Pintoffloat, Const_float a ->
      Lift.int (Int32.of_float (float_of_string a))
    (* | Pnegfloat -> Lift.float (-. a) *)
    | Pstringlength, Const_string s ->
      Lift.int (Int32.of_int (String_literal.utf16_length s))
    (* | Pnegbint Pnativeint, ( (Const_nativeint i)) *)
    (*   ->   *)
    (*   Lift.nativeint (Nativeint.neg i) *)
    | Pnot, Const_js_true -> lambda_false
    | Pnot, Const_js_false -> lambda_true
    | _ -> default ())
  | [Lconst a; Lconst b] -> (
    match (prim, a, b) with
    | Pintcomp cmp, Const_int a, Const_int b -> Lift.bool (cmp_int32 cmp a b)
    | Pfloatcomp cmp, Const_float a, Const_float b ->
      (* FIXME: could raise? *)
      Lift.bool (cmp_float cmp (float_of_string a) (float_of_string b))
    | Pbigintcomp _, Const_bigint _, Const_bigint _ -> default ()
    | Pintcomp ((Ceq | Cneq) as op), Const_polyvar a, Const_polyvar b ->
      Lift.bool
        (match op with
        | Ceq -> a = (b : string)
        | Cneq -> a <> b
        | _ -> assert false)
    | ( Pintcomp ((Ceq | Cneq) as op),
        Const_constructor {name = a; tag_type = None},
        Const_constructor {name = b; tag_type = None} ) ->
      (* Both runtime representations are the constructor names *)
      Lift.bool
        (match op with
        | Ceq -> a = b
        | Cneq -> a <> b
        | _ -> assert false)
    | ( ( Paddint | Psubint | Pmulint | Pdivint | Pmodint | Pandint | Porint
        | Pxorint | Plslint | Plsrint | Pasrint ),
        Const_int aa,
        Const_int bb ) -> (
      (* WE SHOULD keep it as [int], to preserve types *)
      let int_ = Lift.int in
      match prim with
      | Paddint -> int_ (Int32.add aa bb)
      | Psubint -> int_ (Int32.sub aa bb)
      | Pmulint -> int_ (Int32.mul aa bb)
      | Pdivint -> if bb = 0l then default () else int_ (Int32.div aa bb)
      | Pmodint -> if bb = 0l then default () else int_ (Int32.rem aa bb)
      | Pandint -> int_ (Int32.logand aa bb)
      | Porint -> int_ (Int32.logor aa bb)
      | Pxorint -> int_ (Int32.logxor aa bb)
      | Plslint -> int_ (Int32.shift_left aa (Int32.to_int bb))
      | Plsrint -> int_ (Int32.shift_right_logical aa (Int32.to_int bb))
      | Pasrint -> int_ (Int32.shift_right aa (Int32.to_int bb))
      | _ -> default ())
    | Psequand, Const_js_false, (Const_js_true | Const_js_false) -> lambda_false
    | Psequand, Const_js_true, Const_js_true -> lambda_true
    | Psequand, Const_js_true, Const_js_false -> lambda_false
    | Psequor, Const_js_true, (Const_js_true | Const_js_false) -> lambda_true
    | Psequor, Const_js_false, Const_js_true -> lambda_true
    | Psequor, Const_js_false, Const_js_false -> lambda_false
    | Pstringadd, Const_string a, Const_string b -> Lift.string (a ^ b)
    | (Pstringrefs | Pstringrefu), Const_string a, Const_int b -> (
      match String_literal.code_point_at_utf16_index a (Int32.to_int b) with
      | Some codepoint -> Lift.char codepoint
      | None -> default ())
    | _ -> default ())
  | _ -> (
    match prim with
    | Pmakeblock (Blk_module fields) -> (
      let rec aux fields args (var : Ident.t) i =
        match (fields, args) with
        | [], [] -> true
        | ( f :: fields,
            Lprim
              {
                primitive = Pfield (pos, Fld_module {name = f1});
                args = [(Lglobal_module v1 | Lvar v1)];
              }
            :: args ) ->
          pos = i && f = f1 && Ident.same var v1 && aux fields args var (i + 1)
        | _, _ -> false
      in
      match (fields, args) with
      | ( field1 :: rest,
          Lprim
            {
              primitive = Pfield (pos, Fld_module {name = f1});
              args = [((Lglobal_module v1 | Lvar v1) as lam)];
            }
          :: args1 ) ->
        if pos = 0 && field1 = f1 && aux rest args1 v1 1 then lam
        else default ()
      | _ -> default ())
    (* In this level, include is already expanded, so that
       {[
         { x0 : y0 ; x1 : y1 }
       ]}
       such module x can indeed be replaced by module y
    *)
    | _ -> default ())

let not_ loc x : t =
  match x with
  | Lprim ({primitive = Pintcomp Cneq} as prim) ->
    Lprim {prim with primitive = Pintcomp Ceq}
  | _ -> prim ~primitive:Pnot ~args:[x] loc

let has_boolean_type (x : t) =
  match x with
  | Lprim
      {
        primitive =
          ( Pnot | Psequand | Psequor | Pis_not_none | Pobjcomp _ | Pboolcomp _
          | Pintcomp _ | Pfloatcomp _ | Pbigintcomp _ | Pstringcomp _ );
        loc;
      } ->
    Some loc
  | _ -> None

let rec eval_const_as_bool (v : structured_constant) : bool option =
  match v with
  | Const_int x -> Some (x <> 0l)
  | Const_assertfalse -> Some false
  | Const_char x -> Some (x <> 0)
  | Const_js_false | Const_js_null | Const_module_alias | Const_js_undefined _
    ->
    Some false
  | Const_js_true | Const_string _ | Const_polyvar _ | Const_float _
  | Const_bigint _ | Const_block _ ->
    Some true
  | Const_some b -> eval_const_as_bool b
  | Const_constructor {name; tag_type} -> (
    (* Truthiness of the canonical runtime representation *)
    match tag_type with
    | None -> Some (name <> "[]") (* the name string; [] is the number 0 *)
    | Some (String s) -> Some (s <> "")
    | Some (Int i) -> Some (i <> 0)
    | Some (Bool b) -> Some b
    | Some Null | Some Undefined -> Some false
    | Some (Float _ | BigInt _ | Untagged _) -> None)

let if_ (a : t) (b : t) (c : t) : t =
  match a with
  | Lconst v -> (
    match eval_const_as_bool v with
    | Some v -> if v then b else c
    | None -> Lifthenelse (a, b, c))
  | _ -> (
    match (b, c) with
    | _, Lconst Const_assertfalse ->
      seq a b (* TODO: we could customize more cases *)
    | Lconst Const_assertfalse, _ -> seq a c
    | Lconst Const_js_true, Lconst Const_js_false ->
      if has_boolean_type a != None then a else Lifthenelse (a, b, c)
    | Lconst Const_js_false, Lconst Const_js_true -> (
      match has_boolean_type a with
      | Some loc -> not_ loc a
      | None -> Lifthenelse (a, b, c))
    (* [if a then raise e else c] could become [(if a then raise e else ()); c],
       but that is code motion, not normalization: it changes the shape that
       matching's own exit bookkeeping inspects after the term is assembled,
       and doing it here leaves static raises without their catch. It is
       {!Lam_pass_guard_raises} instead. *)
    | _ -> (
      match a with
      | Lprim {primitive = Pisint; args = [Lvar i]; _} -> (
        match b with
        | Lifthenelse
            (Lprim {primitive = Pintcomp Ceq; args = [Lvar j; Lconst _]}, _, b_f)
          when Ident.same i j && eq_approx b_f c ->
          b
        | Lprim {primitive = Pintcomp Ceq; args = [Lvar j; Lconst _]}
          when Ident.same i j && eq_approx lambda_false c ->
          b
        | Lifthenelse
            ( Lprim
                ({primitive = Pintcomp Cneq; args = [Lvar j; Lconst _]} as
                 b_pred),
              b_t,
              b_f )
          when Ident.same i j && eq_approx b_t c ->
          Lifthenelse (Lprim {b_pred with primitive = Pintcomp Ceq}, b_f, b_t)
        | Lprim
            {primitive = Pintcomp Cneq; args = [Lvar j; Lconst _] as args; loc}
        | Lprim
            {
              primitive = Pnot;
              args =
                [
                  Lprim
                    {
                      primitive = Pintcomp Ceq;
                      args = [Lvar j; Lconst _] as args;
                      loc;
                    };
                ];
            }
          when Ident.same i j && eq_approx lambda_true c ->
          Lprim {primitive = Pintcomp Cneq; args; loc}
        | _ -> Lifthenelse (a, b, c))
      | _ -> Lifthenelse (a, b, c)))

(** [shallow_map_sharing f lam] rewrites [lam]'s immediate children with [f]
    and rebuilds the node through its smart constructor, so the result is
    normalized. A node whose children all come back physically unchanged is
    returned as-is, so a traversal that rewrites nothing allocates nothing. *)
let shallow_map_sharing (f : t -> t) (lam : t) : t =
  match lam with
  | Lvar _ | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> lam
  | Lapply ap ->
    let fn = f ap.ap_func in
    let args = Ext_list.map_sharing ap.ap_args f in
    if fn == ap.ap_func && args == ap.ap_args then lam
    else apply fn args ap.ap_info ~ap_transformed_jsx:ap.ap_transformed_jsx
  | Lfunction {params; body; attr; loc} ->
    let body' = f body in
    if body' == body then lam else function_ ~loc ~attr ~params ~body:body'
  | Llet (k, id, e, b) ->
    let e' = f e and b' = f b in
    if e' == e && b' == b then lam else let_ k id e' b'
  | Lletrec (bs, b) ->
    let bs' = Ext_list.map_snd_sharing bs f and b' = f b in
    if bs' == bs && b' == b then lam else letrec bs' b'
  | Lprim {primitive; args; loc} ->
    let args' = Ext_list.map_sharing args f in
    if args' == args then lam else prim ~primitive ~args:args' loc
  | Lswitch (e, sw) ->
    let e' = f e in
    let consts = Ext_list.map_snd_sharing sw.sw_consts f in
    let blocks = Ext_list.map_snd_sharing sw.sw_blocks f in
    let fail = Ext_option.map_sharing sw.sw_failaction f in
    if
      e' == e && consts == sw.sw_consts && blocks == sw.sw_blocks
      && fail == sw.sw_failaction
    then lam
    else
      switch e'
        {sw with sw_consts = consts; sw_blocks = blocks; sw_failaction = fail}
  | Lstringswitch (e, cases, d) ->
    let e' = f e in
    let cases' = Ext_list.map_snd_sharing cases f in
    let d' = Ext_option.map_sharing d f in
    if e' == e && cases' == cases && d' == d then lam
    else stringswitch e' cases' d'
  | Lstaticraise (i, args) ->
    let args' = Ext_list.map_sharing args f in
    if args' == args then lam else staticraise i args'
  | Lstaticcatch (b, h, hd) ->
    let b' = f b and hd' = f hd in
    if b' == b && hd' == hd then lam else staticcatch b' h hd'
  | Ltrywith (b, id, h) ->
    let b' = f b and h' = f h in
    if b' == b && h' == h then lam else try_ b' id h'
  | Lifthenelse (a, b, c) ->
    let a' = f a and b' = f b and c' = f c in
    if a' == a && b' == b && c' == c then lam else if_ a' b' c'
  | Lsequence (a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else seq a' b'
  | Lwhile (a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else while_ a' b'
  | Lfor (id, a, b, d, c) ->
    let a' = f a and b' = f b and c' = f c in
    if a' == a && b' == b && c' == c then lam else for_ id a' b' d c'
  | Lfor_of (id, a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else for_of id a' b'
  | Lfor_await_of (id, a, b) ->
    let a' = f a and b' = f b in
    if a' == a && b' == b then lam else for_await_of id a' b'
  | Lassign (id, b) ->
    let b' = f b in
    if b' == b then lam else assign id b'

let sequor l r = if_ l lambda_true r

(** [l && r] *)
let sequand l r = if_ l r lambda_false

(** [l && r ] *)

let mk_builtin b args loc =
  match b with
  | Primitive p -> Lprim {primitive = p; args; loc}
  | Constant c -> (
    match args with
    | [] -> Lconst c
    | _ -> assert false)
  | Offset_ref delta -> (
    match args with
    | [r] -> offset_ref ~delta r loc
    | _ -> assert false)
  | Eliminated Identity -> (
    match args with
    | [arg] -> arg
    | _ -> assert false)
  | Eliminated Ignore -> (
    match args with
    | [arg] -> Lsequence (arg, lambda_unit)
    | _ -> assert false)

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
    | Lglobal_module _ | Lconst _ -> e
    | Lapply ap ->
      Lapply
        {
          ap with
          ap_func = tr_rec env ap.ap_func;
          ap_args = tr_recs env ap.ap_args;
          ap_info = {ap.ap_info with ap_loc = Location.none};
        }
    | Llet (Alias, x, ex, e) ->
      (* Ignore aliases -> substitute *)
      let ex = tr_rec env ex in
      tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt), x, ex, Lvar v) when Ident.same v x ->
      tr_rec env ex
    | Llet (str, x, ex, e) ->
      (* Because of side effects, keep other lets with normalized names *)
      let ex = tr_rec env ex in
      let y = make_key x in
      Llet (str, y, ex, tr_rec (Ident.add x (Lvar y) env) e)
    | Lprim {primitive = p; args = es; loc = _} ->
      Lprim {primitive = p; args = tr_recs env es; loc = Location.none}
    | Lswitch (e, sw) -> Lswitch (tr_rec env e, tr_sw env sw)
    | Lstringswitch (e, sw, d) ->
      Lstringswitch
        ( tr_rec env e,
          List.map (fun (s, e) -> (s, tr_rec env e)) sw,
          tr_opt env d )
    | Lstaticraise (i, es) -> Lstaticraise (i, tr_recs env es)
    | Lstaticcatch (e1, xs, e2) ->
      Lstaticcatch (tr_rec env e1, xs, tr_rec env e2)
    | Ltrywith (e1, x, e2) -> Ltrywith (tr_rec env e1, x, tr_rec env e2)
    | Lifthenelse (cond, ifso, ifnot) ->
      Lifthenelse (tr_rec env cond, tr_rec env ifso, tr_rec env ifnot)
    | Lsequence (e1, e2) -> Lsequence (tr_rec env e1, tr_rec env e2)
    | Lbreak -> Lbreak
    | Lcontinue -> Lcontinue
    | Lassign (x, e) -> Lassign (x, tr_rec env e)
    | Lletrec _ | Lfunction _ | Lfor _ | Lfor_of _ | Lfor_await_of _ | Lwhile _
      ->
      raise_notrace Not_simple
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
    Llet (strict, id, arg, fn id)

(* Does any immediate child satisfy [f]? Short-circuits. *)
let shallow_exists (f : t -> bool) (lam : t) : bool =
  match lam with
  | Lvar _ | Lglobal_module _ | Lconst _ | Lbreak | Lcontinue -> false
  | Lapply {ap_func; ap_args} -> f ap_func || Ext_list.exists ap_args f
  | Lfunction {body} -> f body
  | Llet (_, _, arg, body) -> f arg || f body
  | Lletrec (decl, body) -> f body || Ext_list.exists_snd decl f
  | Lprim {args} -> Ext_list.exists args f
  | Lswitch (arg, {sw_consts; sw_blocks; sw_failaction}) ->
    f arg
    || Ext_list.exists_snd sw_consts f
    || Ext_list.exists_snd sw_blocks f
    || Ext_option.exists sw_failaction f
  | Lstringswitch (arg, cases, default) ->
    f arg || Ext_list.exists_snd cases f || Ext_option.exists default f
  | Lstaticraise (_, args) -> Ext_list.exists args f
  | Lstaticcatch (e1, _, e2) -> f e1 || f e2
  | Ltrywith (e1, _, e2) -> f e1 || f e2
  | Lifthenelse (e1, e2, e3) -> f e1 || f e2 || f e3
  | Lsequence (e1, e2) -> f e1 || f e2
  | Lwhile (e1, e2) -> f e1 || f e2
  | Lfor (_, e1, e2, _, e3) -> f e1 || f e2 || f e3
  | Lfor_of (_, e1, e2) | Lfor_await_of (_, e1, e2) -> f e1 || f e2
  | Lassign (_, e) -> f e

let iter f lam =
  ignore
    (shallow_exists
       (fun x ->
         f x;
         false)
       lam)

let free_ids get l =
  let fv = ref Set_ident.empty in
  let rec free l =
    iter free l;
    fv := List.fold_left Set_ident.add !fv (get l);
    match l with
    | Lfunction {params} ->
      List.iter (fun param -> fv := Set_ident.remove !fv param) params
    | Llet (_str, id, _arg, _body) -> fv := Set_ident.remove !fv id
    | Lletrec (decl, _body) ->
      List.iter (fun (id, _exp) -> fv := Set_ident.remove !fv id) decl
    | Lstaticcatch (_e1, (_, vars), _e2) ->
      List.iter (fun id -> fv := Set_ident.remove !fv id) vars
    | Ltrywith (_e1, exn, _e2) -> fv := Set_ident.remove !fv exn
    | Lfor (v, _e1, _e2, _dir, _e3) -> fv := Set_ident.remove !fv v
    | Lfor_of (v, _e1, _e2) | Lfor_await_of (v, _e1, _e2) ->
      fv := Set_ident.remove !fv v
    | Lassign (id, _e) -> fv := Set_ident.add !fv id
    | Lvar _ | Lglobal_module _ | Lconst _ | Lapply _ | Lprim _ | Lswitch _
    | Lstringswitch _ | Lstaticraise _ | Lifthenelse _ | Lsequence _ | Lbreak
    | Lcontinue | Lwhile _ ->
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
(* Translate an access path *)

let rec transl_normal_path = function
  | Path.Pident id ->
    (* A predefined exception is its own name at runtime, so the reference is
       that string rather than a module. *)
    if Ident.is_predef_exn id then Lconst (Const_string id.name)
    else if Ident.global id then Lglobal_module id
    else Lvar id
  | Pdot (p, s, pos) ->
    Lprim
      {
        primitive = Pfield (pos, Fld_module {name = s});
        args = [transl_normal_path p];
        loc = Location.none;
      }
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

(* Substitution rebuilds through [shallow_map_sharing], so the result is
   normalized and an untouched subterm is returned physically unchanged. *)
let subst_lambda s lam =
  let rec subst l =
    match l with
    | Lvar id -> ( try Ident.find_same id s with Not_found -> l)
    | _ -> shallow_map_sharing subst l
  in
  subst lam

let make_exit i = Lstaticraise (i, [])

let rec as_simple_exit = function
  | Lstaticraise (i, []) -> Some i
  | Llet (Alias, _, _, e) -> as_simple_exit e
  | _ -> None

(* Introduce a catch around [handler], if worth it. Returns the exit number to
   raise to, and a function wrapping a body in the catch - a body that turns
   out to be exactly that raise gets the handler itself instead. *)
let make_catch_delayed handler =
  match as_simple_exit handler with
  | Some i -> (i, fun act -> act)
  | None -> (
    let i = next_raise_count () in
    ( i,
      fun body ->
        match body with
        | Lstaticraise (j, _) -> if i = j then handler else body
        | _ -> Lstaticcatch (body, (i, []), handler) ))

(* To let-bind expressions to variables *)

let bind str var exp body =
  match exp with
  | Lvar var' when Ident.same var var' -> body
  | _ -> Llet (str, var, exp, body)
