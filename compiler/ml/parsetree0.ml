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

(** Abstract syntax tree produced by parsing *)

open Asttypes

type constant =
  | Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' are rejected by the typechecker
  *)
  | Pconst_char of int
  (* 'c' *)
  | Pconst_string of string * string option
  (* "constant"
     {delim|other constant|delim}
  *)
  | Pconst_float of string * char option
(* 3.4 2e5 1.4e-4

   Suffixes [g-z][G-Z] are accepted by the parser.
   Suffixes are rejected by the typechecker.
*)

(** {1 Extension points} *)

type attribute = string loc * payload
(* [@id ARG]
   [@@id ARG]

   Metadata containers passed around within the AST.
   The compiler ignores unknown attributes.
*)

and extension = string loc * payload
(* [%id ARG]
   [%%id ARG]

   Sub-language placeholder -- rejected by the typechecker.
*)

and attributes = attribute list

and payload =
  | PStr of structure
  | PSig of signature (* : SIG *)
  | PTyp of core_type (* : T *)
  | PPat of pattern * expression option
(* ? P  or  ? P when E *)

(* Type expressions *)

(** {1 Core language} *)

and core_type = {
  ptyp_desc: core_type_desc;
  ptyp_loc: Location.t;
  ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
}

and core_type_desc =
  | Ptyp_any (*  _ *)
  | Ptyp_var of string (* 'a *)
  | Ptyp_arrow of Noloc.arg_label * core_type * core_type
    (* T1 -> T2       Simple
       ~l:T1 -> T2    Labelled
       ?l:T1 -> T2    Optional
    *)
  | Ptyp_tuple of core_type list
    (* T1 * ... * Tn

       Invariant: n >= 2
    *)
  | Ptyp_constr of Longident.t loc * core_type list
    (* tconstr
       T tconstr
       (T1, ..., Tn) tconstr
    *)
  | Ptyp_object of object_field list * closed_flag
    (* < l1:T1; ...; ln:Tn >     (flag = Closed)
       < l1:T1; ...; ln:Tn; .. > (flag = Open)
    *)
  | Ptyp_class of unit (* dummy AST node *)
  | Ptyp_alias of core_type * string (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
    (* [ `A|`B ]         (flag = Closed; labels = None)
       [> `A|`B ]        (flag = Open;   labels = None)
       [< `A|`B ]        (flag = Closed; labels = Some [])
       [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
    *)
  | Ptyp_poly of string loc list * core_type
    (* 'a1 ... 'an. T

       Can only appear in the following context:

       - As the core_type of a Ppat_constraint node corresponding
         to a constraint on a let-binding: let x : 'a1 ... 'an. T
         = e ...

       - Under Cfk_virtual for methods (not values).

       - As the core_type of a Pctf_method node.

       - As the core_type of a Pexp_poly node.

       - As the pld_type field of a label_declaration.

       - As a core_type of a Ptyp_object node.
    *)
  | Ptyp_package of package_type (* (module S) *)
  | Ptyp_extension of extension
(* [%id] *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list
(*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

and row_field =
  | Rtag of label loc * attributes * bool * core_type list
    (* [`A]                   ( true,  [] )
        [`A of T]              ( false, [T] )
        [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
        [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

       - The 2nd field is true if the tag contains a
         constant (empty) constructor.
       - '&' occurs when several types are used for the same constructor
         (see 4.2 in the manual)

       - TODO: switch to a record representation, and keep location
    *)
  | Rinherit of core_type
(* [ T ] *)

and object_field =
  | Otag of label loc * attributes * core_type
  | Oinherit of core_type

(* Patterns *)
and pattern = {
  ppat_desc: pattern_desc;
  ppat_loc: Location.t;
  ppat_attributes: attributes; (* ... [@id1] [@id2] *)
}

and pattern_desc =
  | Ppat_any (* _ *)
  | Ppat_var of string loc (* x *)
  | Ppat_alias of pattern * string loc (* P as 'a *)
  | Ppat_constant of constant (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant * constant
    (* 'a'..'z'

       Other forms of interval are recognized by the parser
       but rejected by the type-checker. *)
  | Ppat_tuple of pattern list (* (P1, ..., Pn)

       Invariant: n >= 2
    *)
  | Ppat_construct of Longident.t loc * pattern option
    (* C                None
       C P              Some P
       C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
    *)
  | Ppat_variant of label * pattern option
    (* `A             (None)
       `A P           (Some P)
    *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    (* { l1=P1; ...; ln=Pn }     (flag = Closed)
       { l1=P1; ...; ln=Pn; _}   (flag = Open)

       Invariant: n > 0
    *)
  | Ppat_array of pattern list (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type (* (P : T) *)
  | Ppat_type of Longident.t loc (* #tconst *)
  | Ppat_lazy of pattern (* lazy P *)
  | Ppat_unpack of string loc
    (* (module P)
       Note: (module P : S) is represented as
       Ppat_constraint(Ppat_unpack, Ptyp_package)
    *)
  | Ppat_exception of pattern (* exception P *)
  | Ppat_extension of extension (* [%id] *)
  | Ppat_open of Longident.t loc * pattern
(* M.(P) *)

(* Value expressions *)

and expression = {
  pexp_desc: expression_desc;
  pexp_loc: Location.t;
  (* Hack: made pexp_attributes mutable for use in analysis exe. Please do not use elsewhere! *)
  mutable pexp_attributes: attributes; (* ... [@id1] [@id2] *)
}

and expression_desc =
  | Pexp_ident of Longident.t loc (* x
       M.x
    *)
  | Pexp_constant of constant (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag * value_binding list * expression
    (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
    *)
  | Pexp_function of case list (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of Noloc.arg_label * expression option * pattern * expression
    (* fun P -> E1                          (Simple, None)
       fun ~l:P -> E1                       (Labelled l, None)
       fun ?l:P -> E1                       (Optional l, None)
       fun ?l:(P = E0) -> E1                (Optional l, Some E0)

       Notes:
       - If E0 is provided, only Optional is allowed.
       - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
       - "let f P = E" is represented using Pexp_fun.
    *)
  | Pexp_apply of expression * (Noloc.arg_label * expression) list
    (* E0 ~l1:E1 ... ~ln:En
       li can be empty (non labeled argument) or start with '?'
       (optional argument).

       Invariant: n > 0
    *)
  | Pexp_match of expression * case list
    (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * case list
    (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list
    (* (E1, ..., En)

       Invariant: n >= 2
    *)
  | Pexp_construct of Longident.t loc * expression option
    (* C                None
       C E              Some E
       C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
    *)
  | Pexp_variant of label * expression option
    (* `A             (None)
       `A E           (Some E)
    *)
  | Pexp_record of (Longident.t loc * expression) list * expression option
    (* { l1=P1; ...; ln=Pn }     (None)
       { E0 with l1=P1; ...; ln=Pn }   (Some E0)

       Invariant: n > 0
    *)
  | Pexp_field of expression * Longident.t loc (* E.l *)
  | Pexp_setfield of expression * Longident.t loc * expression (* E1.l <- E2 *)
  | Pexp_array of expression list (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option
    (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression (* E1; E2 *)
  | Pexp_while of expression * expression (* while E1 do E2 done *)
  | Pexp_for of pattern * expression * expression * direction_flag * expression
    (* for i = E1 to E2 do E3 done      (flag = Upto)
       for i = E1 downto E2 do E3 done  (flag = Downto)
    *)
  | Pexp_constraint of expression * core_type (* (E : T) *)
  | Pexp_coerce of expression * unit * core_type
    (* (E :> T)        (None, T)
         *)
  | Pexp_send of expression * label loc (*  E # m *)
  | Pexp_new of Longident.t loc (* new M.c *)
  | Pexp_setinstvar of label loc * expression (* x <- 2 *)
  | Pexp_override of (label loc * expression) list
    (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
    (* let module M = ME in E *)
  | Pexp_letexception of extension_constructor * expression
    (* let exception C in E *)
  | Pexp_assert of expression
    (* assert E
       Note: "assert false" is treated in a special way by the
       type-checker. *)
  | Pexp_lazy of expression (* lazy E *)
  | Pexp_poly of expression * core_type option
    (* Used for method bodies.

       Can only be used as the expression under Cfk_concrete
       for methods (not values). *)
  | Pexp_object of unit (* dummy AST node *)
  | Pexp_newtype of string loc * expression (* fun (type t) -> E *)
  | Pexp_pack of module_expr
    (* (module ME)

       (module ME : S) is represented as
       Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of override_flag * Longident.t loc * expression
    (* M.(E)
       let open M in E
       let! open M in E *)
  | Pexp_extension of extension (* [%id] *)
  | Pexp_unreachable
(* . *)

and case = {
  (* (P -> E) or (P when E0 -> E) *)
  pc_lhs: pattern;
  pc_guard: expression option;
  pc_rhs: expression;
}

(* Value descriptions *)
and value_description = {
  pval_name: string loc;
  pval_type: core_type;
  pval_prim: string list;
  pval_attributes: attributes; (* ... [@@id1] [@@id2] *)
  pval_loc: Location.t;
}

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
*)

(* Type declarations *)
and type_declaration = {
  ptype_name: string loc;
  ptype_params: (core_type * variance) list;
      (* ('a1,...'an) t; None represents  _*)
  ptype_cstrs: (core_type * core_type * Location.t) list;
      (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
  ptype_kind: type_kind;
  ptype_private: private_flag; (* = private ... *)
  ptype_manifest: core_type option; (* = T *)
  ptype_attributes: attributes; (* ... [@@id1] [@@id2] *)
  ptype_loc: Location.t;
}

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)
and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
    (* Invariant: non-empty list *)
  | Ptype_record of label_declaration list (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration = {
  pld_name: string loc;
  pld_mutable: mutable_flag;
  pld_type: core_type;
  pld_loc: Location.t;
  pld_attributes: attributes; (* l : T [@id1] [@id2] *)
}

(* { ...; l: T; ... }            (mutable=Immutable)
   { ...; mutable l: T; ... }    (mutable=Mutable)

   Note: T can be a Ptyp_poly.
*)
and constructor_declaration = {
  pcd_name: string loc;
  pcd_args: constructor_arguments;
  pcd_res: core_type option;
  pcd_loc: Location.t;
  pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
}

and constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

(*
  | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
  | C: T0                  (res = Some T0, args = [])
  | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
  | C of {...}             (res = None,    args = Pcstr_record)
  | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
  | C of {...} as t        (res = None,    args = Pcstr_record)
*)
and type_extension = {
  ptyext_path: Longident.t loc;
  ptyext_params: (core_type * variance) list;
  ptyext_constructors: extension_constructor list;
  ptyext_private: private_flag;
  ptyext_attributes: attributes; (* ... [@@id1] [@@id2] *)
}
(*
  type t += ...
*)

and extension_constructor = {
  pext_name: string loc;
  pext_kind: extension_constructor_kind;
  pext_loc: Location.t;
  pext_attributes: attributes; (* C of ... [@id1] [@id2] *)
}

and extension_constructor_kind =
  | Pext_decl of constructor_arguments * core_type option
    (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t loc
(*
         | C = D
       *)

(* Type expressions for the module language *)

(** {1 Module language} *)

and module_type = {
  pmty_desc: module_type_desc;
  pmty_loc: Location.t;
  pmty_attributes: attributes; (* ... [@id1] [@id2] *)
}

and module_type_desc =
  | Pmty_ident of Longident.t loc (* S *)
  | Pmty_signature of signature (* sig ... end *)
  | Pmty_functor of string loc * module_type option * module_type
    (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list (* MT with ... *)
  | Pmty_typeof of module_expr (* module type of ME *)
  | Pmty_extension of extension (* [%id] *)
  | Pmty_alias of Longident.t loc
(* (module M) *)

and signature = signature_item list

and signature_item = {psig_desc: signature_item_desc; psig_loc: Location.t}

and signature_item_desc =
  | Psig_value of value_description
    (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of rec_flag * type_declaration list
    (* type t1 = ... and ... and tn = ... *)
  | Psig_typext of type_extension (* type t1 += ... *)
  | Psig_exception of extension_constructor (* exception C of T *)
  | Psig_module of module_declaration (* module X : MT *)
  | Psig_recmodule of module_declaration list
    (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
    (* module type S = MT
       module type S *)
  | Psig_open of open_description (* open X *)
  | Psig_include of include_description (* include MT *)
  | Psig_class of unit (* Dummy AST node *)
  | Psig_class_type of unit (* Dummy AST node *)
  | Psig_attribute of attribute (* [@@@id] *)
  | Psig_extension of extension * attributes
(* [%%id] *)

and module_declaration = {
  pmd_name: string loc;
  pmd_type: module_type;
  pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
  pmd_loc: Location.t;
}
(* S : MT *)

and module_type_declaration = {
  pmtd_name: string loc;
  pmtd_type: module_type option;
  pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
  pmtd_loc: Location.t;
}
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and open_description = {
  popen_lid: Longident.t loc;
  popen_override: override_flag;
  popen_loc: Location.t;
  popen_attributes: attributes;
}
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
*)

and 'a include_infos = {
  pincl_mod: 'a;
  pincl_loc: Location.t;
  pincl_attributes: attributes;
}

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
    (* with type X.t = ...

       Note: the last component of the longident must match
       the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc (* with module X.Y = Z *)
  | Pwith_typesubst of Longident.t loc * type_declaration
    (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_modsubst of Longident.t loc * Longident.t loc
(* with module X.Y := Z *)

(* Value expressions for the module language *)

and module_expr = {
  pmod_desc: module_expr_desc;
  pmod_loc: Location.t;
  pmod_attributes: attributes; (* ... [@id1] [@id2] *)
}

and module_expr_desc =
  | Pmod_ident of Longident.t loc (* X *)
  | Pmod_structure of structure (* struct ... end *)
  | Pmod_functor of string loc * module_type option * module_expr
    (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type (* (ME : MT) *)
  | Pmod_unpack of expression (* (val E) *)
  | Pmod_extension of extension
(* [%id] *)

and structure = structure_item list

and structure_item = {pstr_desc: structure_item_desc; pstr_loc: Location.t}

and structure_item_desc =
  | Pstr_eval of expression * attributes (* E *)
  | Pstr_value of rec_flag * value_binding list
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
    *)
  | Pstr_primitive of value_description
    (* val x: T
       external x: T = "s1" ... "sn" *)
  | Pstr_type of rec_flag * type_declaration list
    (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension (* type t1 += ... *)
  | Pstr_exception of extension_constructor
    (* exception C of T
       exception C = M.X *)
  | Pstr_module of module_binding (* module X = ME *)
  | Pstr_recmodule of module_binding list
    (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration (* module type S = MT *)
  | Pstr_open of open_description (* open X *)
  | Pstr_class of unit (* Dummy AST node *)
  | Pstr_class_type of unit (* Dummy AST node *)
  | Pstr_include of include_declaration (* include ME *)
  | Pstr_attribute of attribute (* [@@@id] *)
  | Pstr_extension of extension * attributes
(* [%%id] *)

and value_binding = {
  pvb_pat: pattern;
  pvb_expr: expression;
  pvb_attributes: attributes;
  pvb_loc: Location.t;
}

and module_binding = {
  pmb_name: string loc;
  pmb_expr: module_expr;
  pmb_attributes: attributes;
  pmb_loc: Location.t;
}
(* X = ME *)

let optional_attr = (Location.mknoloc "res.optional", Parsetree.PStr [])
let optional_attr0 = (Location.mknoloc "res.optional", PStr [])

let add_optional_attr ~optional attrs =
  if optional then optional_attr0 :: attrs else attrs

let get_optional_attr attrs_ =
  let remove_optional_attr attrs =
    List.filter (fun a -> a <> optional_attr) attrs
  in
  let attrs = remove_optional_attr attrs_ in
  let optional = List.length attrs <> List.length attrs_ in
  (optional, attrs)
