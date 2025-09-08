@@ocaml.text(
  /* ************************************************************************ */
  /*  */
  /* OCaml */
  /*  */
  /* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
  /*  */
  /* Copyright 1996 Institut National de Recherche en Informatique et */
  /* en Automatique. */
  /*  */
  /* All rights reserved.  This file is distributed under the terms of */
  /* the GNU Lesser General Public License version 2.1, with the */
  /* special exception on linking described in the file LICENSE. */
  /*  */
  /* ************************************************************************ */

  " Abstract syntax tree produced by parsing "
)

open Asttypes

type constant =
  | Pconst_integer(string, option<char>)
  /* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
 */
  | Pconst_char(char)
  /* 'c' */
  | Pconst_string(string, option<string>)
  /* "constant"
     {delim|other constant|delim}
 */
  | Pconst_float(string, option<char>)
@@ocaml.text(
  /* 3.4 2e5 1.4e-4

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
 */

  " {1 Extension points} "
)

type rec attribute = (loc<string>, payload)
/* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
 */

and extension = (loc<string>, payload)
/* [%id ARG]
         [%%id ARG]

         Sub-language placeholder -- rejected by the typechecker.
 */

and attributes = list<attribute>

and payload =
  | PStr(structure)
  | PSig(signature) /* : SIG */
  | PTyp(core_type) /* : T */
  | PPat(pattern, option<expression>) /* ? P  or  ? P when E */

/* Type expressions */

@ocaml.text(" {1 Core language} ")
and core_type = {
  ptyp_desc: core_type_desc,
  ptyp_loc: Location.t,
  ptyp_attributes: attributes /* ... [@id1] [@id2] */,
}

and core_type_desc =
  | Ptyp_any
  /* _ */
  | Ptyp_var(string)
  /* 'a */
  | Ptyp_arrow(arg_label, core_type, core_type)
  /* T1 -> T2       Simple
           ~l:T1 -> T2    Labelled
           ?l:T1 -> T2    Optional
 */
  | Ptyp_tuple(list<core_type>)
  /* T1 * ... * Tn

           Invariant: n >= 2
 */
  | Ptyp_constr(loc<Longident.t>, list<core_type>)
  /* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
 */
  | Ptyp_object(list<object_field>, closed_flag)
  /* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
 */
  | Ptyp_class(loc<Longident.t>, list<core_type>)
  /* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
 */
  | Ptyp_alias(core_type, string)
  /* T as 'a */
  | Ptyp_variant(list<row_field>, closed_flag, option<list<label>>)
  /* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
 */
  | Ptyp_poly(list<loc<string>>, core_type)
  /* 'a1 ... 'an. T

           Can only appear in the following context:

           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...

           - Under Cfk_virtual for methods (not values).

           - As the core_type of a Pctf_method node.

           - As the core_type of a Pexp_poly node.

           - As the pld_type field of a label_declaration.

           - As a core_type of a Ptyp_object node.
 */

  | Ptyp_package(package_type)
  /* (module S) */
  | Ptyp_extension(extension)
/* [%id] */

and package_type = (loc<Longident.t>, list<(loc<Longident.t>, core_type)>)
/*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
 */

and row_field =
  | Rtag(loc<label>, attributes, bool, list<core_type>)
  /* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
 */
  | Rinherit(core_type)
/* [ T ] */

and object_field =
  | Otag(loc<label>, attributes, core_type)
  | Oinherit(core_type)

/* Patterns */

and pattern = {
  ppat_desc: pattern_desc,
  ppat_loc: Location.t,
  ppat_attributes: attributes /* ... [@id1] [@id2] */,
}

and pattern_desc =
  | Ppat_any
  /* _ */
  | Ppat_var(loc<string>)
  /* x */
  | Ppat_alias(pattern, loc<string>)
  /* P as 'a */
  | Ppat_constant(constant)
  /* 1, 'a', "true", 1.0, 1l, 1L, 1n */
  | Ppat_interval(constant, constant)
  /* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. */
  | Ppat_tuple(list<pattern>)
  /* (P1, ..., Pn)

           Invariant: n >= 2
 */
  | Ppat_construct(loc<Longident.t>, option<pattern>)
  /* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
 */
  | Ppat_variant(label, option<pattern>)
  /* `A             (None)
           `A P           (Some P)
 */
  | Ppat_record(list<(loc<Longident.t>, pattern)>, closed_flag)
  /* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
 */
  | Ppat_array(list<pattern>)
  /* [| P1; ...; Pn |] */
  | Ppat_or(pattern, pattern)
  /* P1 | P2 */
  | Ppat_constraint(pattern, core_type)
  /* (P : T) */
  | Ppat_type(loc<Longident.t>)
  /* #tconst */
  | Ppat_lazy(pattern)
  /* lazy P */
  | Ppat_unpack(loc<string>)
  /* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
 */
  | Ppat_exception(pattern)
  /* exception P */
  | Ppat_extension(extension)
  /* [%id] */
  | Ppat_open(loc<Longident.t>, pattern)
/* M.(P) */

/* Value expressions */

and expression = {
  pexp_desc: expression_desc,
  pexp_loc: Location.t,
  pexp_attributes: attributes /* ... [@id1] [@id2] */,
}

and expression_desc =
  | Pexp_ident(loc<Longident.t>)
  /* x
           M.x
 */
  | Pexp_constant(constant)
  /* 1, 'a', "true", 1.0, 1l, 1L, 1n */
  | Pexp_let(rec_flag, list<value_binding>, expression)
  /* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
 */
  | Pexp_function(list<case>)
  /* function P1 -> E1 | ... | Pn -> En */
  | Pexp_fun(arg_label, option<expression>, pattern, expression)
  /* fun P -> E1                          (Simple, None)
           fun ~l:P -> E1                       (Labelled l, None)
           fun ?l:P -> E1                       (Optional l, None)
           fun ?l:(P = E0) -> E1                (Optional l, Some E0)

           Notes:
           - If E0 is provided, only Optional is allowed.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
 */
  | Pexp_apply(expression, list<(arg_label, expression)>)
  /* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
 */
  | Pexp_match(expression, list<case>)
  /* match E0 with P1 -> E1 | ... | Pn -> En */
  | Pexp_try(expression, list<case>)
  /* try E0 with P1 -> E1 | ... | Pn -> En */
  | Pexp_tuple(list<expression>)
  /* (E1, ..., En)

           Invariant: n >= 2
 */
  | Pexp_construct(loc<Longident.t>, option<expression>)
  /* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
 */
  | Pexp_variant(label, option<expression>)
  /* `A             (None)
           `A E           (Some E)
 */
  | Pexp_record(list<(loc<Longident.t>, expression)>, option<expression>)
  /* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
 */
  | Pexp_field(expression, loc<Longident.t>)
  /* E.l */
  | Pexp_setfield(expression, loc<Longident.t>, expression)
  /* E1.l <- E2 */
  | Pexp_array(list<expression>)
  /* [| E1; ...; En |] */
  | Pexp_ifthenelse(expression, expression, option<expression>)
  /* if E1 then E2 else E3 */
  | Pexp_sequence(expression, expression)
  /* E1; E2 */
  | Pexp_while(expression, expression)
  /* while E1 do E2 done */
  | Pexp_for(pattern, expression, expression, direction_flag, expression)
  /* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
 */
  | Pexp_constraint(expression, core_type)
  /* (E : T) */
  | Pexp_coerce(expression, option<core_type>, core_type)
  /* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
 */
  | Pexp_send(expression, loc<label>)
  /* E # m */
  | Pexp_new(loc<Longident.t>)
  /* new M.c */
  | Pexp_setinstvar(loc<label>, expression)
  /* x <- 2 */
  | Pexp_override(list<(loc<label>, expression)>)
  /* {< x1 = E1; ...; Xn = En >} */
  | Pexp_letmodule(loc<string>, module_expr, expression)
  /* let module M = ME in E */
  | Pexp_letexception(extension_constructor, expression)
  /* let exception C in E */
  | Pexp_assert(expression)
  /* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. */
  | Pexp_lazy(expression)
  /* lazy E */
  | Pexp_poly(expression, option<core_type>)
  /* Used for method bodies.

           Can only be used as the expression under Cfk_concrete
           for methods (not values). */
  | Pexp_object(class_structure)
  /* object ... end */
  | Pexp_newtype(loc<string>, expression)
  /* fun (type t) -> E */
  | Pexp_pack(module_expr)
  /* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) */
  | Pexp_open(override_flag, loc<Longident.t>, expression)
  /* M.(E)
           let open M in E
           let! open M in E */
  | Pexp_extension(extension)
  /* [%id] */
  | Pexp_unreachable
/* . */

and case = {
  /* (P -> E) or (P when E0 -> E) */

  pc_lhs: pattern,
  pc_guard: option<expression>,
  pc_rhs: expression,
}

/* Value descriptions */

and value_description = {
  pval_name: loc<string>,
  pval_type: core_type,
  pval_prim: list<string>,
  pval_attributes: attributes /* ... [@@id1] [@@id2] */,
  pval_loc: Location.t,
}

/*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])
*/

/* Type declarations */

and type_declaration = {
  ptype_name: loc<string>,
  ptype_params: list<(core_type, variance)>,
  /* ('a1,...'an) t; None represents  _ */
  ptype_cstrs: list<(core_type, core_type, Location.t)>,
  /* ... constraint T1=T1'  ... constraint Tn=Tn' */
  ptype_kind: type_kind,
  ptype_private: private_flag /* = private ... */,
  ptype_manifest: option<core_type> /* = T */,
  ptype_attributes: attributes /* ... [@@id1] [@@id2] */,
  ptype_loc: Location.t,
}

/*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*/

and type_kind =
  | Ptype_abstract
  | Ptype_variant(list<constructor_declaration>)
  /* Invariant: non-empty list */
  | Ptype_record(list<label_declaration>)
  /* Invariant: non-empty list */
  | Ptype_open

and label_declaration = {
  pld_name: loc<string>,
  pld_mutable: mutable_flag,
  pld_type: core_type,
  pld_loc: Location.t,
  pld_attributes: attributes /* l : T [@id1] [@id2] */,
}

/* { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*/

and constructor_declaration = {
  pcd_name: loc<string>,
  pcd_args: constructor_arguments,
  pcd_res: option<core_type>,
  pcd_loc: Location.t,
  pcd_attributes: attributes /* C of ... [@id1] [@id2] */,
}

and constructor_arguments =
  | Pcstr_tuple(list<core_type>)
  | Pcstr_record(list<label_declaration>)

/*
  | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
  | C: T0                  (res = Some T0, args = [])
  | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
  | C of {...}             (res = None,    args = Pcstr_record)
  | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
  | C of {...} as t        (res = None,    args = Pcstr_record)
*/

and type_extension = {
  ptyext_path: loc<Longident.t>,
  ptyext_params: list<(core_type, variance)>,
  ptyext_constructors: list<extension_constructor>,
  ptyext_private: private_flag,
  ptyext_attributes: attributes /* ... [@@id1] [@@id2] */,
}
/*
  type t += ...
*/

and extension_constructor = {
  pext_name: loc<string>,
  pext_kind: extension_constructor_kind,
  pext_loc: Location.t,
  pext_attributes: attributes /* C of ... [@id1] [@id2] */,
}

and extension_constructor_kind =
  | Pext_decl(constructor_arguments, option<core_type>)
  /*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
 */
  | Pext_rebind(loc<Longident.t>)
/*
         | C = D
 */

/* Type expressions for the class language */

@ocaml.text(" {1 Class language} ")
and class_type = {
  pcty_desc: class_type_desc,
  pcty_loc: Location.t,
  pcty_attributes: attributes /* ... [@id1] [@id2] */,
}

and class_type_desc =
  | Pcty_constr(loc<Longident.t>, list<core_type>)
  /* c
   ['a1, ..., 'an] c */
  | Pcty_signature(class_signature)
  /* object ... end */
  | Pcty_arrow(arg_label, core_type, class_type)
  /* T -> CT       Simple
           ~l:T -> CT    Labelled l
           ?l:T -> CT    Optional l
 */
  | Pcty_extension(extension)
  /* [%id] */
  | Pcty_open(override_flag, loc<Longident.t>, class_type)
/* let open M in CT */

and class_signature = {
  pcsig_self: core_type,
  pcsig_fields: list<class_type_field>,
}
/* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
 */

and class_type_field = {
  pctf_desc: class_type_field_desc,
  pctf_loc: Location.t,
  pctf_attributes: attributes /* ... [@@id1] [@@id2] */,
}

and class_type_field_desc =
  | Pctf_inherit(class_type)
  /* inherit CT */
  | Pctf_val((loc<label>, mutable_flag, virtual_flag, core_type))
  /* val x: T */
  | Pctf_method((loc<label>, private_flag, virtual_flag, core_type))
  /* method x: T

           Note: T can be a Ptyp_poly.
 */
  | Pctf_constraint((core_type, core_type))
  /* constraint T1 = T2 */
  | Pctf_attribute(attribute)
  /* [@@@id] */
  | Pctf_extension(extension)
/* [%%id] */

and class_infos<'a> = {
  pci_virt: virtual_flag,
  pci_params: list<(core_type, variance)>,
  pci_name: loc<string>,
  pci_expr: 'a,
  pci_loc: Location.t,
  pci_attributes: attributes /* ... [@@id1] [@@id2] */,
}
/* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*/

and class_description = class_infos<class_type>

and class_type_declaration = class_infos<class_type>

/* Value expressions for the class language */

and class_expr = {
  pcl_desc: class_expr_desc,
  pcl_loc: Location.t,
  pcl_attributes: attributes /* ... [@id1] [@id2] */,
}

and class_expr_desc =
  | Pcl_constr(loc<Longident.t>, list<core_type>)
  /* c
   ['a1, ..., 'an] c */
  | Pcl_structure(class_structure)
  /* object ... end */
  | Pcl_fun(arg_label, option<expression>, pattern, class_expr)
  /* fun P -> CE                          (Simple, None)
           fun ~l:P -> CE                       (Labelled l, None)
           fun ?l:P -> CE                       (Optional l, None)
           fun ?l:(P = E0) -> CE                (Optional l, Some E0)
 */
  | Pcl_apply(class_expr, list<(arg_label, expression)>)
  /* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
 */
  | Pcl_let(rec_flag, list<value_binding>, class_expr)
  /* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
 */
  | Pcl_constraint(class_expr, class_type)
  /* (CE : CT) */
  | Pcl_extension(extension)
  /* [%id] */
  | Pcl_open(override_flag, loc<Longident.t>, class_expr)
/* let open M in CE */

and class_structure = {
  pcstr_self: pattern,
  pcstr_fields: list<class_field>,
}
/* object(selfpat) ... end
   object ... end           (self = Ppat_any)
 */

and class_field = {
  pcf_desc: class_field_desc,
  pcf_loc: Location.t,
  pcf_attributes: attributes /* ... [@@id1] [@@id2] */,
}

and class_field_desc =
  | Pcf_inherit(override_flag, class_expr, option<loc<string>>)
  /* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
 */
  | Pcf_val((loc<label>, mutable_flag, class_field_kind))
  /* val x = E
           val virtual x: T
 */
  | Pcf_method((loc<label>, private_flag, class_field_kind))
  /* method x = E            (E can be a Pexp_poly)
           method virtual x: T     (T can be a Ptyp_poly)
 */
  | Pcf_constraint((core_type, core_type))
  /* constraint T1 = T2 */
  | Pcf_initializer(expression)
  /* initializer E */
  | Pcf_attribute(attribute)
  /* [@@@id] */
  | Pcf_extension(extension)
/* [%%id] */

and class_field_kind =
  | Cfk_virtual(core_type)
  | Cfk_concrete(override_flag, expression)

and class_declaration = class_infos<class_expr>

/* Type expressions for the module language */

@ocaml.text(" {1 Module language} ")
and module_type = {
  pmty_desc: module_type_desc,
  pmty_loc: Location.t,
  pmty_attributes: attributes /* ... [@id1] [@id2] */,
}

and module_type_desc =
  | Pmty_ident(loc<Longident.t>)
  /* S */
  | Pmty_signature(signature)
  /* sig ... end */
  | Pmty_functor(loc<string>, option<module_type>, module_type)
  /* functor(X : MT1) -> MT2 */
  | Pmty_with(module_type, list<with_constraint>)
  /* MT with ... */
  | Pmty_typeof(module_expr)
  /* module type of ME */
  | Pmty_extension(extension)
  /* [%id] */
  | Pmty_alias(loc<Longident.t>)
/* (module M) */

and signature = list<signature_item>

and signature_item = {
  psig_desc: signature_item_desc,
  psig_loc: Location.t,
}

and signature_item_desc =
  | Psig_value(value_description)
  /*
          val x: T
          external x: T = "s1" ... "sn"
 */
  | Psig_type(rec_flag, list<type_declaration>)
  /* type t1 = ... and ... and tn = ... */
  | Psig_typext(type_extension)
  /* type t1 += ... */
  | Psig_exception(extension_constructor)
  /* exception C of T */
  | Psig_module(module_declaration)
  /* module X : MT */
  | Psig_recmodule(list<module_declaration>)
  /* module rec X1 : MT1 and ... and Xn : MTn */
  | Psig_modtype(module_type_declaration)
  /* module type S = MT
   module type S */
  | Psig_open(open_description)
  /* open X */
  | Psig_include(include_description)
  /* include MT */
  | Psig_class(list<class_description>)
  /* class c1 : ... and ... and cn : ... */
  | Psig_class_type(list<class_type_declaration>)
  /* class type ct1 = ... and ... and ctn = ... */
  | Psig_attribute(attribute)
  /* [@@@id] */
  | Psig_extension(extension, attributes)
/* [%%id] */

and module_declaration = {
  pmd_name: loc<string>,
  pmd_type: module_type,
  pmd_attributes: attributes /* ... [@@id1] [@@id2] */,
  pmd_loc: Location.t,
}
/* S : MT */

and module_type_declaration = {
  pmtd_name: loc<string>,
  pmtd_type: option<module_type>,
  pmtd_attributes: attributes /* ... [@@id1] [@@id2] */,
  pmtd_loc: Location.t,
}
/* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*/

and open_description = {
  popen_lid: loc<Longident.t>,
  popen_override: override_flag,
  popen_loc: Location.t,
  popen_attributes: attributes,
}
/* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
 */

and include_infos<'a> = {
  pincl_mod: 'a,
  pincl_loc: Location.t,
  pincl_attributes: attributes,
}

and include_description = include_infos<module_type>
/* include MT */

and include_declaration = include_infos<module_expr>
/* include ME */

and with_constraint =
  | Pwith_type(loc<Longident.t>, type_declaration)
  /* with type X.t = ...

           Note: the last component of the longident must match
           the name of the type_declaration. */
  | Pwith_module(loc<Longident.t>, loc<Longident.t>)
  /* with module X.Y = Z */
  | Pwith_typesubst(loc<Longident.t>, type_declaration)
  /* with type X.t := ..., same format as [Pwith_type] */
  | Pwith_modsubst(loc<Longident.t>, loc<Longident.t>)
/* with module X.Y := Z */

/* Value expressions for the module language */

and module_expr = {
  pmod_desc: module_expr_desc,
  pmod_loc: Location.t,
  pmod_attributes: attributes /* ... [@id1] [@id2] */,
}

and module_expr_desc =
  | Pmod_ident(loc<Longident.t>)
  /* X */
  | Pmod_structure(structure)
  /* struct ... end */
  | Pmod_functor(loc<string>, option<module_type>, module_expr)
  /* functor(X : MT1) -> ME */
  | Pmod_apply(module_expr, module_expr)
  /* ME1(ME2) */
  | Pmod_constraint(module_expr, module_type)
  /* (ME : MT) */
  | Pmod_unpack(expression)
  /* (val E) */
  | Pmod_extension(extension)
/* [%id] */

and structure = list<structure_item>

and structure_item = {
  pstr_desc: structure_item_desc,
  pstr_loc: Location.t,
}

and structure_item_desc =
  | Pstr_eval(expression, attributes)
  /* E */
  | Pstr_value(rec_flag, list<value_binding>)
  /* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
 */
  | Pstr_primitive(value_description)
  /* val x: T
   external x: T = "s1" ... "sn" */
  | Pstr_type(rec_flag, list<type_declaration>)
  /* type t1 = ... and ... and tn = ... */
  | Pstr_typext(type_extension)
  /* type t1 += ... */
  | Pstr_exception(extension_constructor)
  /* exception C of T
   exception C = M.X */
  | Pstr_module(module_binding)
  /* module X = ME */
  | Pstr_recmodule(list<module_binding>)
  /* module rec X1 = ME1 and ... and Xn = MEn */
  | Pstr_modtype(module_type_declaration)
  /* module type S = MT */
  | Pstr_open(open_description)
  /* open X */
  | Pstr_class(list<class_declaration>)
  /* class c1 = ... and ... and cn = ... */
  | Pstr_class_type(list<class_type_declaration>)
  /* class type ct1 = ... and ... and ctn = ... */
  | Pstr_include(include_declaration)
  /* include ME */
  | Pstr_attribute(attribute)
  /* [@@@id] */
  | Pstr_extension(extension, attributes)
/* [%%id] */

and value_binding = {
  pvb_pat: pattern,
  pvb_expr: expression,
  pvb_attributes: attributes,
  pvb_loc: Location.t,
}

and module_binding = {
  pmb_name: loc<string>,
  pmb_expr: module_expr,
  pmb_attributes: attributes,
  pmb_loc: Location.t,
}
@@ocaml.text(
  /* X = ME */

  " {1 Toplevel} "
)

/* Toplevel phrases */

type rec toplevel_phrase =
  | Ptop_def(structure)
  | Ptop_dir(string, directive_argument)
/* #use, #load ... */

and directive_argument =
  | Pdir_none
  | Pdir_string(string)
  | Pdir_int(string, option<char>)
  | Pdir_ident(Longident.t)
  | Pdir_bool(bool)

