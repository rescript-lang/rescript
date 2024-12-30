(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Format
open Lexing
open Location
open Parsetree

let fmt_position with_name f l =
  let fname = if with_name then l.pos_fname else "" in
  if l.pos_lnum = -1 then fprintf f "%s[%d]" fname l.pos_cnum
  else
    fprintf f "%s[%d,%d+%d]" fname l.pos_lnum l.pos_bol (l.pos_cnum - l.pos_bol)

let fmt_location f loc =
  if !Clflags.dump_location then (
    let p_2nd_name = loc.loc_start.pos_fname <> loc.loc_end.pos_fname in
    fprintf f "(%a..%a)" (fmt_position true) loc.loc_start
      (fmt_position p_2nd_name) loc.loc_end;
    if loc.loc_ghost then fprintf f " ghost")

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident s -> fprintf f "%s" s
  | Longident.Ldot (y, s) -> fprintf f "%a.%s" fmt_longident_aux y s
  | Longident.Lapply (y, z) ->
    fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z

let fmt_longident_loc f (x : Longident.t loc) =
  fprintf f "\"%a\" %a" fmt_longident_aux x.txt fmt_location x.loc

let fmt_string_loc f (x : string loc) =
  fprintf f "\"%s\" %a" x.txt fmt_location x.loc

let fmt_char_option f = function
  | None -> fprintf f "None"
  | Some c -> fprintf f "Some %c" c

let fmt_constant f x =
  match x with
  | Pconst_integer (i, m) -> fprintf f "PConst_int (%s,%a)" i fmt_char_option m
  | Pconst_char c -> fprintf f "PConst_char %02x" c
  | Pconst_string (s, None) -> fprintf f "PConst_string(%S,None)" s
  | Pconst_string (s, Some delim) ->
    fprintf f "PConst_string (%S,Some %S)" s delim
  | Pconst_float (s, m) -> fprintf f "PConst_float (%s,%a)" s fmt_char_option m

let fmt_mutable_flag f x =
  match x with
  | Immutable -> fprintf f "Immutable"
  | Mutable -> fprintf f "Mutable"

let fmt_override_flag f x =
  match x with
  | Override -> fprintf f "Override"
  | Fresh -> fprintf f "Fresh"

let fmt_closed_flag f x =
  match x with
  | Closed -> fprintf f "Closed"
  | Open -> fprintf f "Open"

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> fprintf f "Nonrec"
  | Recursive -> fprintf f "Rec"

let fmt_direction_flag f x =
  match x with
  | Upto -> fprintf f "Up"
  | Downto -> fprintf f "Down"

let fmt_private_flag f x =
  match x with
  | Public -> fprintf f "Public"
  | Private -> fprintf f "Private"

let line i f s (*...*) =
  fprintf f "%s" (String.make (2 * i mod 72) ' ');
  fprintf f s (*...*)

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n"
  | _ :: _ ->
    line i ppf "[\n";
    List.iter (f (i + 1) ppf) l;
    line i ppf "]\n"

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n"
  | Some x ->
    line i ppf "Some\n";
    f (i + 1) ppf x

let longident_loc i ppf li = line i ppf "%a\n" fmt_longident_loc li
let string i ppf s = line i ppf "\"%s\"\n" s
let string_loc i ppf s = line i ppf "%a\n" fmt_string_loc s
let arg_label i ppf = function
  | Nolabel -> line i ppf "Nolabel\n"
  | Optional s -> line i ppf "Optional \"%s\"\n" s
  | Labelled s -> line i ppf "Labelled \"%s\"\n" s

let rec core_type i ppf x =
  line i ppf "core_type %a\n" fmt_location x.ptyp_loc;
  attributes i ppf x.ptyp_attributes;
  let i = i + 1 in
  match x.ptyp_desc with
  | Ptyp_any -> line i ppf "Ptyp_any\n"
  | Ptyp_var s -> line i ppf "Ptyp_var %s\n" s
  | Ptyp_arrow (l, ct1, ct2, a) ->
    line i ppf "Ptyp_arrow\n";
    let () =
      match a with
      | None -> ()
      | Some n -> line i ppf "arity = %d\n" n
    in
    arg_label i ppf l;
    core_type i ppf ct1;
    core_type i ppf ct2
  | Ptyp_tuple l ->
    line i ppf "Ptyp_tuple\n";
    list i core_type ppf l
  | Ptyp_constr (li, l) ->
    line i ppf "Ptyp_constr %a\n" fmt_longident_loc li;
    list i core_type ppf l
  | Ptyp_variant (l, closed, low) ->
    line i ppf "Ptyp_variant closed=%a\n" fmt_closed_flag closed;
    list i label_x_bool_x_core_type_list ppf l;
    option i (fun i -> list i string) ppf low
  | Ptyp_object (l, c) ->
    line i ppf "Ptyp_object %a\n" fmt_closed_flag c;
    let i = i + 1 in
    List.iter
      (function
        | Otag (l, attrs, t) ->
          line i ppf "method %s\n" l.txt;
          attributes i ppf attrs;
          core_type (i + 1) ppf t
        | Oinherit ct ->
          line i ppf "Oinherit\n";
          core_type (i + 1) ppf ct)
      l
  | Ptyp_alias (ct, s) ->
    line i ppf "Ptyp_alias \"%s\"\n" s;
    core_type i ppf ct
  | Ptyp_poly (sl, ct) ->
    line i ppf "Ptyp_poly%a\n"
      (fun ppf -> List.iter (fun x -> fprintf ppf " '%s" x.txt))
      sl;
    core_type i ppf ct
  | Ptyp_package (s, l) ->
    line i ppf "Ptyp_package %a\n" fmt_longident_loc s;
    list i package_with ppf l
  | Ptyp_extension (s, arg) ->
    line i ppf "Ptyp_extension \"%s\"\n" s.txt;
    payload i ppf arg

and package_with i ppf (s, t) =
  line i ppf "with type %a\n" fmt_longident_loc s;
  core_type i ppf t

and pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.ppat_loc;
  attributes i ppf x.ppat_attributes;
  let i = i + 1 in
  match x.ppat_desc with
  | Ppat_any -> line i ppf "Ppat_any\n"
  | Ppat_var s -> line i ppf "Ppat_var %a\n" fmt_string_loc s
  | Ppat_alias (p, s) ->
    line i ppf "Ppat_alias %a\n" fmt_string_loc s;
    pattern i ppf p
  | Ppat_constant c -> line i ppf "Ppat_constant %a\n" fmt_constant c
  | Ppat_interval (c1, c2) ->
    line i ppf "Ppat_interval %a..%a\n" fmt_constant c1 fmt_constant c2
  | Ppat_tuple l ->
    line i ppf "Ppat_tuple\n";
    list i pattern ppf l
  | Ppat_construct (li, po) ->
    line i ppf "Ppat_construct %a\n" fmt_longident_loc li;
    option i pattern ppf po
  | Ppat_variant (l, po) ->
    line i ppf "Ppat_variant \"%s\"\n" l;
    option i pattern ppf po
  | Ppat_record (l, c) ->
    line i ppf "Ppat_record %a\n" fmt_closed_flag c;
    list i longident_x_pattern ppf l
  | Ppat_array l ->
    line i ppf "Ppat_array\n";
    list i pattern ppf l
  | Ppat_or (p1, p2) ->
    line i ppf "Ppat_or\n";
    pattern i ppf p1;
    pattern i ppf p2
  | Ppat_lazy p ->
    line i ppf "Ppat_lazy\n";
    pattern i ppf p
  | Ppat_constraint (p, ct) ->
    line i ppf "Ppat_constraint\n";
    pattern i ppf p;
    core_type i ppf ct
  | Ppat_type li ->
    line i ppf "Ppat_type\n";
    longident_loc i ppf li
  | Ppat_unpack s -> line i ppf "Ppat_unpack %a\n" fmt_string_loc s
  | Ppat_exception p ->
    line i ppf "Ppat_exception\n";
    pattern i ppf p
  | Ppat_open (m, p) ->
    line i ppf "Ppat_open \"%a\"\n" fmt_longident_loc m;
    pattern i ppf p
  | Ppat_extension (s, arg) ->
    line i ppf "Ppat_extension \"%s\"\n" s.txt;
    payload i ppf arg

and expression i ppf x =
  line i ppf "expression %a\n" fmt_location x.pexp_loc;
  attributes i ppf x.pexp_attributes;
  let i = i + 1 in
  match x.pexp_desc with
  | Pexp_ident li -> line i ppf "Pexp_ident %a\n" fmt_longident_loc li
  | Pexp_constant c -> line i ppf "Pexp_constant %a\n" fmt_constant c
  | Pexp_let (rf, l, e) ->
    line i ppf "Pexp_let %a\n" fmt_rec_flag rf;
    list i value_binding ppf l;
    expression i ppf e
  | Pexp_fun {arg_label = l; default = eo; lhs = p; rhs = e; arity} ->
    line i ppf "Pexp_fun\n";
    let () =
      match arity with
      | None -> ()
      | Some arity -> line i ppf "arity:%d\n" arity
    in
    arg_label i ppf l;
    option i expression ppf eo;
    pattern i ppf p;
    expression i ppf e
  | Pexp_apply (e, l) ->
    line i ppf "Pexp_apply\n";
    expression i ppf e;
    list i label_x_expression ppf l
  | Pexp_match (e, l) ->
    line i ppf "Pexp_match\n";
    expression i ppf e;
    list i case ppf l
  | Pexp_try (e, l) ->
    line i ppf "Pexp_try\n";
    expression i ppf e;
    list i case ppf l
  | Pexp_tuple l ->
    line i ppf "Pexp_tuple\n";
    list i expression ppf l
  | Pexp_construct (li, eo) ->
    line i ppf "Pexp_construct %a\n" fmt_longident_loc li;
    option i expression ppf eo
  | Pexp_variant (l, eo) ->
    line i ppf "Pexp_variant \"%s\"\n" l;
    option i expression ppf eo
  | Pexp_record (l, eo) ->
    line i ppf "Pexp_record\n";
    list i longident_x_expression ppf l;
    option i expression ppf eo
  | Pexp_field (e, li) ->
    line i ppf "Pexp_field\n";
    expression i ppf e;
    longident_loc i ppf li
  | Pexp_setfield (e1, li, e2) ->
    line i ppf "Pexp_setfield\n";
    expression i ppf e1;
    longident_loc i ppf li;
    expression i ppf e2
  | Pexp_array l ->
    line i ppf "Pexp_array\n";
    list i expression ppf l
  | Pexp_ifthenelse (e1, e2, eo) ->
    line i ppf "Pexp_ifthenelse\n";
    expression i ppf e1;
    expression i ppf e2;
    option i expression ppf eo
  | Pexp_sequence (e1, e2) ->
    line i ppf "Pexp_sequence\n";
    expression i ppf e1;
    expression i ppf e2
  | Pexp_while (e1, e2) ->
    line i ppf "Pexp_while\n";
    expression i ppf e1;
    expression i ppf e2
  | Pexp_for (p, e1, e2, df, e3) ->
    line i ppf "Pexp_for %a\n" fmt_direction_flag df;
    pattern i ppf p;
    expression i ppf e1;
    expression i ppf e2;
    expression i ppf e3
  | Pexp_constraint (e, ct) ->
    line i ppf "Pexp_constraint\n";
    expression i ppf e;
    core_type i ppf ct
  | Pexp_coerce (e, (), cto2) ->
    line i ppf "Pexp_coerce\n";
    expression i ppf e;
    core_type i ppf cto2
  | Pexp_send (e, s) ->
    line i ppf "Pexp_send \"%s\"\n" s.txt;
    expression i ppf e
  | Pexp_new li -> line i ppf "Pexp_new %a\n" fmt_longident_loc li
  | Pexp_setinstvar (s, e) ->
    line i ppf "Pexp_setinstvar %a\n" fmt_string_loc s;
    expression i ppf e
  | Pexp_override l ->
    line i ppf "Pexp_override\n";
    list i string_x_expression ppf l
  | Pexp_letmodule (s, me, e) ->
    line i ppf "Pexp_letmodule %a\n" fmt_string_loc s;
    module_expr i ppf me;
    expression i ppf e
  | Pexp_letexception (cd, e) ->
    line i ppf "Pexp_letexception\n";
    extension_constructor i ppf cd;
    expression i ppf e
  | Pexp_assert e ->
    line i ppf "Pexp_assert\n";
    expression i ppf e
  | Pexp_lazy e ->
    line i ppf "Pexp_lazy\n";
    expression i ppf e
  | Pexp_poly (e, cto) ->
    line i ppf "Pexp_poly\n";
    expression i ppf e;
    option i core_type ppf cto
  | Pexp_newtype (s, e) ->
    line i ppf "Pexp_newtype \"%s\"\n" s.txt;
    expression i ppf e
  | Pexp_pack me ->
    line i ppf "Pexp_pack\n";
    module_expr i ppf me
  | Pexp_open (ovf, m, e) ->
    line i ppf "Pexp_open %a \"%a\"\n" fmt_override_flag ovf fmt_longident_loc m;
    expression i ppf e
  | Pexp_extension (s, arg) ->
    line i ppf "Pexp_extension \"%s\"\n" s.txt;
    payload i ppf arg

and value_description i ppf x =
  line i ppf "value_description %a %a\n" fmt_string_loc x.pval_name fmt_location
    x.pval_loc;
  attributes i ppf x.pval_attributes;
  core_type (i + 1) ppf x.pval_type;
  list (i + 1) string ppf x.pval_prim

and type_parameter i ppf (x, _variance) = core_type i ppf x

and type_declaration i ppf x =
  line i ppf "type_declaration %a %a\n" fmt_string_loc x.ptype_name fmt_location
    x.ptype_loc;
  attributes i ppf x.ptype_attributes;
  let i = i + 1 in
  line i ppf "ptype_params =\n";
  list (i + 1) type_parameter ppf x.ptype_params;
  line i ppf "ptype_cstrs =\n";
  list (i + 1) core_type_x_core_type_x_location ppf x.ptype_cstrs;
  line i ppf "ptype_kind =\n";
  type_kind (i + 1) ppf x.ptype_kind;
  line i ppf "ptype_private = %a\n" fmt_private_flag x.ptype_private;
  line i ppf "ptype_manifest =\n";
  option (i + 1) core_type ppf x.ptype_manifest

and attributes i ppf l =
  let i = i + 1 in
  List.iter
    (fun (s, arg) ->
      line i ppf "attribute %a \"%s\"\n" fmt_location (s : _ Asttypes.loc).loc
        s.txt;
      payload (i + 1) ppf arg)
    l

and payload i ppf = function
  | PStr x -> structure i ppf x
  | PSig x -> signature i ppf x
  | PTyp x -> core_type i ppf x
  | PPat (x, None) -> pattern i ppf x
  | PPat (x, Some g) ->
    pattern i ppf x;
    line i ppf "<when>\n";
    expression (i + 1) ppf g

and type_kind i ppf x =
  match x with
  | Ptype_abstract -> line i ppf "Ptype_abstract\n"
  | Ptype_variant l ->
    line i ppf "Ptype_variant\n";
    list (i + 1) constructor_decl ppf l
  | Ptype_record l ->
    line i ppf "Ptype_record\n";
    list (i + 1) label_decl ppf l
  | Ptype_open -> line i ppf "Ptype_open\n"

and type_extension i ppf x =
  line i ppf "type_extension\n";
  attributes i ppf x.ptyext_attributes;
  let i = i + 1 in
  line i ppf "ptyext_path = %a\n" fmt_longident_loc x.ptyext_path;
  line i ppf "ptyext_params =\n";
  list (i + 1) type_parameter ppf x.ptyext_params;
  line i ppf "ptyext_constructors =\n";
  list (i + 1) extension_constructor ppf x.ptyext_constructors;
  line i ppf "ptyext_private = %a\n" fmt_private_flag x.ptyext_private

and extension_constructor i ppf x =
  line i ppf "extension_constructor %a\n" fmt_location x.pext_loc;
  attributes i ppf x.pext_attributes;
  let i = i + 1 in
  line i ppf "pext_name = \"%s\"\n" x.pext_name.txt;
  line i ppf "pext_kind =\n";
  extension_constructor_kind (i + 1) ppf x.pext_kind

and extension_constructor_kind i ppf x =
  match x with
  | Pext_decl (a, r) ->
    line i ppf "Pext_decl\n";
    constructor_arguments (i + 1) ppf a;
    option (i + 1) core_type ppf r
  | Pext_rebind li ->
    line i ppf "Pext_rebind\n";
    line (i + 1) ppf "%a\n" fmt_longident_loc li

and module_type i ppf x =
  line i ppf "module_type %a\n" fmt_location x.pmty_loc;
  attributes i ppf x.pmty_attributes;
  let i = i + 1 in
  match x.pmty_desc with
  | Pmty_ident li -> line i ppf "Pmty_ident %a\n" fmt_longident_loc li
  | Pmty_alias li -> line i ppf "Pmty_alias %a\n" fmt_longident_loc li
  | Pmty_signature s ->
    line i ppf "Pmty_signature\n";
    signature i ppf s
  | Pmty_functor (s, mt1, mt2) ->
    line i ppf "Pmty_functor %a\n" fmt_string_loc s;
    Misc.may (module_type i ppf) mt1;
    module_type i ppf mt2
  | Pmty_with (mt, l) ->
    line i ppf "Pmty_with\n";
    module_type i ppf mt;
    list i with_constraint ppf l
  | Pmty_typeof m ->
    line i ppf "Pmty_typeof\n";
    module_expr i ppf m
  | Pmty_extension (s, arg) ->
    line i ppf "Pmod_extension \"%s\"\n" s.txt;
    payload i ppf arg

and signature i ppf x = list i signature_item ppf x

and signature_item i ppf x =
  line i ppf "signature_item %a\n" fmt_location x.psig_loc;
  let i = i + 1 in
  match x.psig_desc with
  | Psig_value vd ->
    line i ppf "Psig_value\n";
    value_description i ppf vd
  | Psig_type (rf, l) ->
    line i ppf "Psig_type %a\n" fmt_rec_flag rf;
    list i type_declaration ppf l
  | Psig_typext te ->
    line i ppf "Psig_typext\n";
    type_extension i ppf te
  | Psig_exception ext ->
    line i ppf "Psig_exception\n";
    extension_constructor i ppf ext
  | Psig_module pmd ->
    line i ppf "Psig_module %a\n" fmt_string_loc pmd.pmd_name;
    attributes i ppf pmd.pmd_attributes;
    module_type i ppf pmd.pmd_type
  | Psig_recmodule decls ->
    line i ppf "Psig_recmodule\n";
    list i module_declaration ppf decls
  | Psig_modtype x ->
    line i ppf "Psig_modtype %a\n" fmt_string_loc x.pmtd_name;
    attributes i ppf x.pmtd_attributes;
    modtype_declaration i ppf x.pmtd_type
  | Psig_open od ->
    line i ppf "Psig_open %a %a\n" fmt_override_flag od.popen_override
      fmt_longident_loc od.popen_lid;
    attributes i ppf od.popen_attributes
  | Psig_include incl ->
    line i ppf "Psig_include\n";
    module_type i ppf incl.pincl_mod;
    attributes i ppf incl.pincl_attributes
  | Psig_class () -> ()
  | Psig_class_type () -> ()
  | Psig_extension ((s, arg), attrs) ->
    line i ppf "Psig_extension \"%s\"\n" s.txt;
    attributes i ppf attrs;
    payload i ppf arg
  | Psig_attribute (s, arg) ->
    line i ppf "Psig_attribute \"%s\"\n" s.txt;
    payload i ppf arg

and modtype_declaration i ppf = function
  | None -> line i ppf "#abstract"
  | Some mt -> module_type (i + 1) ppf mt

and with_constraint i ppf x =
  match x with
  | Pwith_type (lid, td) ->
    line i ppf "Pwith_type %a\n" fmt_longident_loc lid;
    type_declaration (i + 1) ppf td
  | Pwith_typesubst (lid, td) ->
    line i ppf "Pwith_typesubst %a\n" fmt_longident_loc lid;
    type_declaration (i + 1) ppf td
  | Pwith_module (lid1, lid2) ->
    line i ppf "Pwith_module %a = %a\n" fmt_longident_loc lid1 fmt_longident_loc
      lid2
  | Pwith_modsubst (lid1, lid2) ->
    line i ppf "Pwith_modsubst %a = %a\n" fmt_longident_loc lid1
      fmt_longident_loc lid2

and module_expr i ppf x =
  line i ppf "module_expr %a\n" fmt_location x.pmod_loc;
  attributes i ppf x.pmod_attributes;
  let i = i + 1 in
  match x.pmod_desc with
  | Pmod_ident li -> line i ppf "Pmod_ident %a\n" fmt_longident_loc li
  | Pmod_structure s ->
    line i ppf "Pmod_structure\n";
    structure i ppf s
  | Pmod_functor (s, mt, me) ->
    line i ppf "Pmod_functor %a\n" fmt_string_loc s;
    Misc.may (module_type i ppf) mt;
    module_expr i ppf me
  | Pmod_apply (me1, me2) ->
    line i ppf "Pmod_apply\n";
    module_expr i ppf me1;
    module_expr i ppf me2
  | Pmod_constraint (me, mt) ->
    line i ppf "Pmod_constraint\n";
    module_expr i ppf me;
    module_type i ppf mt
  | Pmod_unpack e ->
    line i ppf "Pmod_unpack\n";
    expression i ppf e
  | Pmod_extension (s, arg) ->
    line i ppf "Pmod_extension \"%s\"\n" s.txt;
    payload i ppf arg

and structure i ppf x = list i structure_item ppf x

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.pstr_loc;
  let i = i + 1 in
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
    line i ppf "Pstr_eval\n";
    attributes i ppf attrs;
    expression i ppf e
  | Pstr_value (rf, l) ->
    line i ppf "Pstr_value %a\n" fmt_rec_flag rf;
    list i value_binding ppf l
  | Pstr_primitive vd ->
    line i ppf "Pstr_primitive\n";
    value_description i ppf vd
  | Pstr_type (rf, l) ->
    line i ppf "Pstr_type %a\n" fmt_rec_flag rf;
    list i type_declaration ppf l
  | Pstr_typext te ->
    line i ppf "Pstr_typext\n";
    type_extension i ppf te
  | Pstr_exception ext ->
    line i ppf "Pstr_exception\n";
    extension_constructor i ppf ext
  | Pstr_module x ->
    line i ppf "Pstr_module\n";
    module_binding i ppf x
  | Pstr_recmodule bindings ->
    line i ppf "Pstr_recmodule\n";
    list i module_binding ppf bindings
  | Pstr_modtype x ->
    line i ppf "Pstr_modtype %a\n" fmt_string_loc x.pmtd_name;
    attributes i ppf x.pmtd_attributes;
    modtype_declaration i ppf x.pmtd_type
  | Pstr_open od ->
    line i ppf "Pstr_open %a %a\n" fmt_override_flag od.popen_override
      fmt_longident_loc od.popen_lid;
    attributes i ppf od.popen_attributes
  | Pstr_class () -> ()
  | Pstr_class_type () -> ()
  | Pstr_include incl ->
    line i ppf "Pstr_include";
    attributes i ppf incl.pincl_attributes;
    module_expr i ppf incl.pincl_mod
  | Pstr_extension ((s, arg), attrs) ->
    line i ppf "Pstr_extension \"%s\"\n" s.txt;
    attributes i ppf attrs;
    payload i ppf arg
  | Pstr_attribute (s, arg) ->
    line i ppf "Pstr_attribute \"%s\"\n" s.txt;
    payload i ppf arg

and module_declaration i ppf pmd =
  string_loc i ppf pmd.pmd_name;
  attributes i ppf pmd.pmd_attributes;
  module_type (i + 1) ppf pmd.pmd_type

and module_binding i ppf x =
  string_loc i ppf x.pmb_name;
  attributes i ppf x.pmb_attributes;
  module_expr (i + 1) ppf x.pmb_expr

and core_type_x_core_type_x_location i ppf (ct1, ct2, l) =
  line i ppf "<constraint> %a\n" fmt_location l;
  core_type (i + 1) ppf ct1;
  core_type (i + 1) ppf ct2

and constructor_decl i ppf
    {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} =
  line i ppf "%a\n" fmt_location pcd_loc;
  line (i + 1) ppf "%a\n" fmt_string_loc pcd_name;
  attributes i ppf pcd_attributes;
  constructor_arguments (i + 1) ppf pcd_args;
  option (i + 1) core_type ppf pcd_res

and constructor_arguments i ppf = function
  | Pcstr_tuple l -> list i core_type ppf l
  | Pcstr_record l -> list i label_decl ppf l

and label_decl i ppf {pld_name; pld_mutable; pld_type; pld_loc; pld_attributes}
    =
  line i ppf "%a\n" fmt_location pld_loc;
  attributes i ppf pld_attributes;
  line (i + 1) ppf "%a\n" fmt_mutable_flag pld_mutable;
  line (i + 1) ppf "%a" fmt_string_loc pld_name;
  core_type (i + 1) ppf pld_type

and longident_x_pattern i ppf (li, p, opt) =
  line i ppf "%a%s\n" fmt_longident_loc li (if opt then "?" else "");
  pattern (i + 1) ppf p

and case i ppf {pc_lhs; pc_guard; pc_rhs} =
  line i ppf "<case>\n";
  pattern (i + 1) ppf pc_lhs;
  (match pc_guard with
  | None -> ()
  | Some g ->
    line (i + 1) ppf "<when>\n";
    expression (i + 2) ppf g);
  expression (i + 1) ppf pc_rhs

and value_binding i ppf x =
  line i ppf "<def>\n";
  attributes (i + 1) ppf x.pvb_attributes;
  pattern (i + 1) ppf x.pvb_pat;
  expression (i + 1) ppf x.pvb_expr

and string_x_expression i ppf (s, e) =
  line i ppf "<override> %a\n" fmt_string_loc s;
  expression (i + 1) ppf e

and longident_x_expression i ppf (li, e, opt) =
  line i ppf "%a%s\n" fmt_longident_loc li (if opt then "?" else "");
  expression (i + 1) ppf e

and label_x_expression i ppf (l, e) =
  line i ppf "<arg>\n";
  arg_label i ppf l;
  expression (i + 1) ppf e

and label_x_bool_x_core_type_list i ppf x =
  match x with
  | Rtag (l, attrs, b, ctl) ->
    line i ppf "Rtag \"%s\" %s\n" l.txt (string_of_bool b);
    attributes (i + 1) ppf attrs;
    list (i + 1) core_type ppf ctl
  | Rinherit ct ->
    line i ppf "Rinherit\n";
    core_type (i + 1) ppf ct

let interface ppf x = list 0 signature_item ppf x

let implementation ppf x = list 0 structure_item ppf x
