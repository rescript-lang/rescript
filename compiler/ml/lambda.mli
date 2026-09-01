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

(* The "lambda" intermediate code *)

open Asttypes

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
      mutable_flag: mutable_flag;
      runtime: Variant_runtime.block_runtime;
    }
  | Blk_tuple
  | Blk_poly_var
  | Blk_record of {
      fields: (string * bool (* optional *)) array;
      mutable_flag: mutable_flag;
    }
  | Blk_module of string list
  | Blk_module_export of Ident.t list
  | Blk_extension
    (* underlying is the same as tuple, immutable block
       {[
          exception A of int * int
       ]}
       is translated into
       {[
         [A, x, y]
       ]}
    *)
  | Blk_record_ext of {fields: string array; mutable_flag: mutable_flag}

val find_name : Parsetree.attribute -> Asttypes.label option

val tag_label_of_tag_info : tag_info -> string
val mutable_flag_of_tag_info : tag_info -> mutable_flag
val blk_record :
  (Types.label_description * Typedtree.record_label_definition * bool) array ->
  mutable_flag ->
  tag_info

val blk_record_ext :
  (Types.label_description * Typedtree.record_label_definition * bool) array ->
  mutable_flag ->
  tag_info

val blk_record_inlined :
  (Types.label_description * Typedtree.record_label_definition * bool) array ->
  string ->
  int ->
  runtime:Variant_runtime.block_runtime ->
  mutable_flag ->
  tag_info

val ref_tag_info : tag_info

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

val fld_record : Types.label_description -> field_dbg_info

val fld_record_inline : Types.label_description -> field_dbg_info

val fld_record_extension : Types.label_description -> field_dbg_info

val ref_field_info : field_dbg_info

type set_field_dbg_info =
  | Fld_record_set of string
  | Fld_record_inline_set of string
  | Fld_record_extension_set of string

val ref_field_set_info : set_field_dbg_info

val fld_record_set : Types.label_description -> set_field_dbg_info

val fld_record_inline_set : Types.label_description -> set_field_dbg_info

val fld_record_extension_set : Types.label_description -> set_field_dbg_info

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
  (* object primitives *)
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
  (* modules *)
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
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_method
  | Ptagged_template

and comparison = Ceq | Cneq | Clt | Cgt | Cle | Cge

type structured_constant =
  | Const_int of int32
  | Const_char of int
  | Const_string of {s: string; delim: External_arg_spec.delim option}
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
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effects; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
    Variable: the variable x is assigned later in e'
*)

(* [true] means yes, [false] may mean unknown *)
type function_attribute = {
  inline: inline_attribute;
  is_a_functor: bool;
  return_unit: bool;
  async: bool;
  directive: string option;
  one_unit_arg: bool;
}

type lambda = private
  | Lvar of Ident.t
  | Lglobal_module of Ident.t
      (** A reference to another compilation unit: a name the module system
          resolves, not a value this one computes. *)
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of prim_info
  | Lswitch of lambda * lambda_switch
  (* switch on strings, clauses are sorted by string order,
     strings are pairwise distinct *)
  | Lstringswitch of lambda * (string * lambda) list * lambda option
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lbreak
  | Lcontinue
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lfor_of of Ident.t * lambda * lambda
  | Lfor_await_of of Ident.t * lambda * lambda
  | Lassign of Ident.t * lambda

and lfunction = {
  params: Ident.t list;
  body: lambda;
  attr: function_attribute; (* specified with [@inline] attribute *)
  loc: Location.t;
}

and prim_info = private {
  primitive: primitive;
  args: lambda list;
  loc: Location.t;
}

and ap_info = {
  ap_loc: Location.t;
  ap_inlined: inline_attribute; (* specified with the [@inlined] attribute *)
}

and lambda_apply = private {
  ap_func: lambda;
  ap_args: lambda list;
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
  sw_failaction: 'a option; (* Action to take if failure *)
  sw_dispatch: switch_dispatch;
}

and lambda_switch = lambda switch

(* Lambda code for the middle-end.
   * In the closure case the code is a sequence of assignments to a
     preallocated block of size [main_module_block_size] using
     (Setfield(Getglobal(module_ident))). The size is used to preallocate
     the block.
   * In the flambda case the code is an expression returning a block
     value of size [main_module_block_size]. The size is used to build
     the module root as an initialize_symbol
     Initialize_symbol(module_name, 0,
       [getfield 0; ...; getfield (main_module_block_size - 1)])
*)

(* Sharing key *)
val make_key : lambda -> lambda option

val const_int : int -> structured_constant
val const_string : string -> string option -> structured_constant
val const_of_typed : constant -> structured_constant
val const_unit : structured_constant
val const_constructor : Variant_runtime.tag -> structured_constant
val const_shape_none : structured_constant
val const_polyvar : string -> structured_constant
val const_polyvar_name : string -> structured_constant
val const_module_alias : structured_constant
val lambda_assert_false : lambda
val lambda_unit : lambda

val eq_primitive_approx : primitive -> primitive -> bool

val str_of_field_info : field_dbg_info -> string option

val eq_comparison : comparison -> comparison -> bool

val is_immutable_block : tag_info -> bool

val const_is_allocating : structured_constant -> bool

val const_eq_approx : structured_constant -> structured_constant -> bool

val cmp_int32 : comparison -> int32 -> int32 -> bool

val cmp_float : comparison -> float -> float -> bool

(* Constructors. [lambda] is private, so every term outside this module is
   built through one of these.

   Most are plain wrappers. Six normalize as they build, and are the only
   place that normalization happens - a pass cannot bypass it by writing a
   constructor directly:

   - [prim] folds an operation whose arguments are already constants, and
     collapses a module record rebuilt field-by-field from another module
     back to that module.
   - [if_] resolves a constant condition, collapses a branch that asserts
     false, turns boolean branches into the condition or its negation, and
     recognizes a few [Pisint] shapes.
   - [switch] and [stringswitch] pick the matching case when the scrutinee
     is constant.
   - [not_] rewrites a negated inequality into an equality.
   - [seq] drops a first operand that only allocates.
   - [apply] eta-reduces a function whose body is a single primitive call on
     its own parameters.

   These fire when a term is rebuilt with new children, which in practice
   means during the optimizer's passes rather than at production: the
   frontend has no constants in operand position yet. *)

val var : Ident.t -> lambda

val global_module : Ident.t -> lambda

val const : structured_constant -> lambda

val apply :
  ?ap_transformed_jsx:bool -> lambda -> lambda list -> ap_info -> lambda

val function_ :
  loc:Location.t ->
  attr:function_attribute ->
  params:Ident.t list ->
  body:lambda ->
  lambda

val let_ : let_kind -> Ident.t -> lambda -> lambda -> lambda

val letrec : (Ident.t * lambda) list -> lambda -> lambda

val prim : primitive:primitive -> args:lambda list -> Location.t -> lambda

val switch : lambda -> lambda_switch -> lambda

val stringswitch : lambda -> (string * lambda) list -> lambda option -> lambda

val staticraise : int -> lambda list -> lambda

val staticcatch : lambda -> int * Ident.t list -> lambda -> lambda

val try_ : lambda -> Ident.t -> lambda -> lambda

val if_ : lambda -> lambda -> lambda -> lambda

val seq : lambda -> lambda -> lambda

val break : lambda

val continue : lambda

val while_ : lambda -> lambda -> lambda

val for_ : Ident.t -> lambda -> lambda -> direction_flag -> lambda -> lambda

val for_of : Ident.t -> lambda -> lambda -> lambda

val for_await_of : Ident.t -> lambda -> lambda -> lambda

val assign : Ident.t -> lambda -> lambda

val not_ : Location.t -> lambda -> lambda

val sequor : lambda -> lambda -> lambda

val sequand : lambda -> lambda -> lambda

val lambda_true : lambda

val lambda_false : lambda

val shallow_map_sharing : (lambda -> lambda) -> lambda -> lambda
(** Rewrite a node's immediate children, rebuilding through the constructors
    so the result is normalized. A node whose children are all physically
    unchanged is returned as-is, so a traversal that rewrites nothing
    allocates nothing. *)

val eq_approx : lambda -> lambda -> bool

val mk_builtin : builtin -> lambda list -> Location.t -> lambda
(** Expands the non-[Primitive] builtins, which have no IR form. *)

val lambda_module_alias : lambda
val name_lambda : let_kind -> lambda -> (Ident.t -> lambda) -> lambda

val iter : (lambda -> unit) -> lambda -> unit
module Ident_set : Set.S with type elt = Ident.t
val free_variables : lambda -> Ident_set.t

val transl_normal_path : Path.t -> lambda (* Path.t is already normal *)

val transl_module_path : ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_value_path : ?loc:Location.t -> Env.t -> Path.t -> lambda
val transl_extension_path : ?loc:Location.t -> Env.t -> Path.t -> lambda

val subst_lambda : lambda Ident.tbl -> lambda -> lambda
val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda

val default_function_attribute : function_attribute

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int

val make_exit : int -> lambda

val as_simple_exit : lambda -> int option

(* Exit number to raise to, and a wrapper that puts the catch around a body. *)
val make_catch_delayed : lambda -> int * (lambda -> lambda)
val next_negative_raise_count : unit -> int
(* Negative raise counts are used to compile 'match ... with
   exception x -> ...'.  This disabled some simplifications
   performed by the Simplif module that assume that static raises
   are in tail position in their handler. *)

val staticfail : lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded : lambda -> bool
val patch_guarded : lambda -> lambda -> lambda
