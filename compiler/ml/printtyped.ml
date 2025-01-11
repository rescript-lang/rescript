(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
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
open Typedtree

let fmt_position f l =
  if l.pos_lnum = -1 then fprintf f "%s[%d]" l.pos_fname l.pos_cnum
  else
    fprintf f "%s[%d,%d+%d]" l.pos_fname l.pos_lnum l.pos_bol
      (l.pos_cnum - l.pos_bol)

let fmt_location f loc =
  if !Clflags.dump_location then (
    fprintf f "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end;
    if loc.loc_ghost then fprintf f " ghost")

let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident s -> fprintf f "%s" s
  | Longident.Ldot (y, s) -> fprintf f "%a.%s" fmt_longident_aux y s
  | Longident.Lapply (y, z) ->
    fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z

let fmt_longident f x = fprintf f "\"%a\"" fmt_longident_aux x.txt

let fmt_ident = Ident.print

let rec fmt_path_aux f x =
  match x with
  | Path.Pident s -> fprintf f "%a" fmt_ident s
  | Path.Pdot (y, s, _pos) -> fprintf f "%a.%s" fmt_path_aux y s
  | Path.Papply (y, z) -> fprintf f "%a(%a)" fmt_path_aux y fmt_path_aux z

let fmt_path f x = fprintf f "\"%a\"" fmt_path_aux x

let fmt_constant f x =
  match x with
  | Const_int i -> fprintf f "Const_int %d" i
  | Const_char c -> fprintf f "Const_char %02x" c
  | Const_string (s, None) -> fprintf f "Const_string(%S,None)" s
  | Const_string (s, Some delim) ->
    fprintf f "Const_string (%S,Some %S)" s delim
  | Const_float s -> fprintf f "Const_float %s" s
  | Const_int32 i -> fprintf f "Const_int32 %ld" i
  | Const_int64 i -> fprintf f "Const_int64 %Ld" i
  | Const_bigint (sign, i) ->
    fprintf f "Const_bigint %s" (Bigint_utils.to_string sign i)

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
  fprintf f "%s" (String.make (2 * i) ' ');
  fprintf f s (*...*)

let list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n"
  | _ :: _ ->
    line i ppf "[\n";
    List.iter (f (i + 1) ppf) l;
    line i ppf "]\n"

let array i f ppf a =
  if Array.length a = 0 then line i ppf "[]\n"
  else (
    line i ppf "[\n";
    Array.iter (f (i + 1) ppf) a;
    line i ppf "]\n")

let option i f ppf x =
  match x with
  | None -> line i ppf "None\n"
  | Some x ->
    line i ppf "Some\n";
    f (i + 1) ppf x

let longident i ppf li = line i ppf "%a\n" fmt_longident li
let string i ppf s = line i ppf "\"%s\"\n" s
let arg_label i ppf = function
  | Nolabel -> line i ppf "Nolabel\n"
  | Optional s -> line i ppf "Optional \"%s\"\n" s
  | Labelled s -> line i ppf "Labelled \"%s\"\n" s

let record_representation i ppf =
  let open Types in
  function
  | Record_regular -> line i ppf "Record_regular\n"
  | Record_float_unused -> assert false
  | Record_unboxed b -> line i ppf "Record_unboxed %b\n" b
  | Record_inlined {tag = i} -> line i ppf "Record_inlined %d\n" i
  | Record_extension -> line i ppf "Record_extension\n"

let attributes i ppf l =
  let i = i + 1 in
  List.iter
    (fun (s, arg) ->
      line i ppf "attribute \"%s\"\n" s.txt;
      Printast.payload (i + 1) ppf arg)
    l

let rec core_type i ppf x =
  line i ppf "core_type %a\n" fmt_location x.ctyp_loc;
  attributes i ppf x.ctyp_attributes;
  let i = i + 1 in
  match x.ctyp_desc with
  | Ttyp_any -> line i ppf "Ttyp_any\n"
  | Ttyp_var s -> line i ppf "Ttyp_var %s\n" s
  | Ttyp_arrow (l, ct1, ct2, _) ->
    line i ppf "Ttyp_arrow\n";
    arg_label i ppf l;
    core_type i ppf ct1;
    core_type i ppf ct2
  | Ttyp_tuple l ->
    line i ppf "Ttyp_tuple\n";
    list i core_type ppf l
  | Ttyp_constr (li, _, l) ->
    line i ppf "Ttyp_constr %a\n" fmt_path li;
    list i core_type ppf l
  | Ttyp_variant (l, closed, low) ->
    line i ppf "Ttyp_variant closed=%a\n" fmt_closed_flag closed;
    list i label_x_bool_x_core_type_list ppf l;
    option i (fun i -> list i string) ppf low
  | Ttyp_object (l, c) ->
    line i ppf "Ttyp_object %a\n" fmt_closed_flag c;
    let i = i + 1 in
    List.iter
      (function
        | OTtag (s, attrs, t) ->
          line i ppf "method %s\n" s.txt;
          attributes i ppf attrs;
          core_type (i + 1) ppf t
        | OTinherit ct ->
          line i ppf "OTinherit\n";
          core_type (i + 1) ppf ct)
      l
  | Ttyp_alias (ct, s) ->
    line i ppf "Ttyp_alias \"%s\"\n" s;
    core_type i ppf ct
  | Ttyp_poly (sl, ct) ->
    line i ppf "Ttyp_poly%a\n"
      (fun ppf -> List.iter (fun x -> fprintf ppf " '%s" x))
      sl;
    core_type i ppf ct
  | Ttyp_package {pack_path = s; pack_fields = l} ->
    line i ppf "Ttyp_package %a\n" fmt_path s;
    list i package_with ppf l

and package_with i ppf (s, t) =
  line i ppf "with type %a\n" fmt_longident s;
  core_type i ppf t

and pattern i ppf x =
  line i ppf "pattern %a\n" fmt_location x.pat_loc;
  attributes i ppf x.pat_attributes;
  let i = i + 1 in
  match x.pat_extra with
  | (Tpat_unpack, _, attrs) :: rem ->
    line i ppf "Tpat_unpack\n";
    attributes i ppf attrs;
    pattern i ppf {x with pat_extra = rem}
  | (Tpat_constraint cty, _, attrs) :: rem ->
    line i ppf "Tpat_constraint\n";
    attributes i ppf attrs;
    core_type i ppf cty;
    pattern i ppf {x with pat_extra = rem}
  | (Tpat_type (id, _), _, attrs) :: rem ->
    line i ppf "Tpat_type %a\n" fmt_path id;
    attributes i ppf attrs;
    pattern i ppf {x with pat_extra = rem}
  | (Tpat_open (id, _, _), _, attrs) :: rem ->
    line i ppf "Tpat_open \"%a\"\n" fmt_path id;
    attributes i ppf attrs;
    pattern i ppf {x with pat_extra = rem}
  | [] -> (
    match x.pat_desc with
    | Tpat_any -> line i ppf "Tpat_any\n"
    | Tpat_var (s, _) -> line i ppf "Tpat_var \"%a\"\n" fmt_ident s
    | Tpat_alias (p, s, _) ->
      line i ppf "Tpat_alias \"%a\"\n" fmt_ident s;
      pattern i ppf p
    | Tpat_constant c -> line i ppf "Tpat_constant %a\n" fmt_constant c
    | Tpat_tuple l ->
      line i ppf "Tpat_tuple\n";
      list i pattern ppf l
    | Tpat_construct (li, _, po) ->
      line i ppf "Tpat_construct %a\n" fmt_longident li;
      list i pattern ppf po
    | Tpat_variant (l, po, _) ->
      line i ppf "Tpat_variant \"%s\"\n" l;
      option i pattern ppf po
    | Tpat_record (l, _c) ->
      line i ppf "Tpat_record\n";
      list i longident_x_pattern ppf l
    | Tpat_array l ->
      line i ppf "Tpat_array\n";
      list i pattern ppf l
    | Tpat_or (p1, p2, _) ->
      line i ppf "Tpat_or\n";
      pattern i ppf p1;
      pattern i ppf p2
    | Tpat_lazy p ->
      line i ppf "Tpat_lazy\n";
      pattern i ppf p)

and expression_extra i ppf x attrs =
  match x with
  | Texp_constraint ct ->
    line i ppf "Texp_constraint\n";
    attributes i ppf attrs;
    core_type i ppf ct
  | Texp_coerce ((), cto2) ->
    line i ppf "Texp_coerce\n";
    attributes i ppf attrs;
    core_type i ppf cto2
  | Texp_open (ovf, m, _, _) ->
    line i ppf "Texp_open %a \"%a\"\n" fmt_override_flag ovf fmt_path m;
    attributes i ppf attrs
  | Texp_newtype s ->
    line i ppf "Texp_newtype \"%s\"\n" s;
    attributes i ppf attrs

and expression i ppf x =
  line i ppf "expression %a\n" fmt_location x.exp_loc;
  attributes i ppf x.exp_attributes;
  let i =
    List.fold_left
      (fun i (extra, _, attrs) ->
        expression_extra i ppf extra attrs;
        i + 1)
      (i + 1) x.exp_extra
  in
  match x.exp_desc with
  | Texp_ident (li, _, _) -> line i ppf "Texp_ident %a\n" fmt_path li
  | Texp_constant c -> line i ppf "Texp_constant %a\n" fmt_constant c
  | Texp_let (rf, l, e) ->
    line i ppf "Texp_let %a\n" fmt_rec_flag rf;
    list i value_binding ppf l;
    expression i ppf e
  | Texp_function
      {arg_label = p; arity; async; param; case = case_; partial = _} ->
    line i ppf "Texp_function\n";
    if async then line i ppf "async\n";
    (match arity with
    | Some arity -> line i ppf "arity: %d\n" arity
    | None -> ());
    line i ppf "%a" Ident.print param;
    arg_label i ppf p;
    case i ppf case_
  | Texp_apply {funct = e; args = l; partial} ->
    if partial then line i ppf "partial\n";
    line i ppf "Texp_apply\n";
    expression i ppf e;
    list i label_x_expression ppf l
  | Texp_match (e, l1, l2, _partial) ->
    line i ppf "Texp_match\n";
    expression i ppf e;
    list i case ppf l1;
    list i case ppf l2
  | Texp_try (e, l) ->
    line i ppf "Texp_try\n";
    expression i ppf e;
    list i case ppf l
  | Texp_tuple l ->
    line i ppf "Texp_tuple\n";
    list i expression ppf l
  | Texp_construct (li, _, eo) ->
    line i ppf "Texp_construct %a\n" fmt_longident li;
    list i expression ppf eo
  | Texp_variant (l, eo) ->
    line i ppf "Texp_variant \"%s\"\n" l;
    option i expression ppf eo
  | Texp_record {fields; representation; extended_expression} ->
    line i ppf "Texp_record\n";
    let i = i + 1 in
    line i ppf "fields =\n";
    array (i + 1) record_field ppf fields;
    line i ppf "representation =\n";
    record_representation (i + 1) ppf representation;
    line i ppf "extended_expression =\n";
    option (i + 1) expression ppf extended_expression
  | Texp_field (e, li, _) ->
    line i ppf "Texp_field\n";
    expression i ppf e;
    longident i ppf li
  | Texp_setfield (e1, li, _, e2) ->
    line i ppf "Texp_setfield\n";
    expression i ppf e1;
    longident i ppf li;
    expression i ppf e2
  | Texp_array l ->
    line i ppf "Texp_array\n";
    list i expression ppf l
  | Texp_ifthenelse (e1, e2, eo) ->
    line i ppf "Texp_ifthenelse\n";
    expression i ppf e1;
    expression i ppf e2;
    option i expression ppf eo
  | Texp_sequence (e1, e2) ->
    line i ppf "Texp_sequence\n";
    expression i ppf e1;
    expression i ppf e2
  | Texp_while (e1, e2) ->
    line i ppf "Texp_while\n";
    expression i ppf e1;
    expression i ppf e2
  | Texp_for (s, _, e1, e2, df, e3) ->
    line i ppf "Texp_for \"%a\" %a\n" fmt_ident s fmt_direction_flag df;
    expression i ppf e1;
    expression i ppf e2;
    expression i ppf e3
  | Texp_send (e, Tmeth_name s, eo) ->
    line i ppf "Texp_send \"%s\"\n" s;
    expression i ppf e;
    option i expression ppf eo
  | Texp_letmodule (s, _, me, e) ->
    line i ppf "Texp_letmodule \"%a\"\n" fmt_ident s;
    module_expr i ppf me;
    expression i ppf e
  | Texp_letexception (cd, e) ->
    line i ppf "Texp_letexception\n";
    extension_constructor i ppf cd;
    expression i ppf e
  | Texp_assert e ->
    line i ppf "Texp_assert";
    expression i ppf e
  | Texp_lazy e ->
    line i ppf "Texp_lazy";
    expression i ppf e
  | Texp_pack me ->
    line i ppf "Texp_pack";
    module_expr i ppf me
  | Texp_extension_constructor (li, _) ->
    line i ppf "Texp_extension_constructor %a" fmt_longident li

and value_description i ppf x =
  line i ppf "value_description %a %a\n" fmt_ident x.val_id fmt_location
    x.val_loc;
  attributes i ppf x.val_attributes;
  core_type (i + 1) ppf x.val_desc;
  list (i + 1) string ppf x.val_prim

and type_parameter i ppf (x, _variance) = core_type i ppf x

and type_declaration i ppf x =
  line i ppf "type_declaration %a %a\n" fmt_ident x.typ_id fmt_location
    x.typ_loc;
  attributes i ppf x.typ_attributes;
  let i = i + 1 in
  line i ppf "ptype_params =\n";
  list (i + 1) type_parameter ppf x.typ_params;
  line i ppf "ptype_cstrs =\n";
  list (i + 1) core_type_x_core_type_x_location ppf x.typ_cstrs;
  line i ppf "ptype_kind =\n";
  type_kind (i + 1) ppf x.typ_kind;
  line i ppf "ptype_private = %a\n" fmt_private_flag x.typ_private;
  line i ppf "ptype_manifest =\n";
  option (i + 1) core_type ppf x.typ_manifest

and type_kind i ppf x =
  match x with
  | Ttype_abstract -> line i ppf "Ttype_abstract\n"
  | Ttype_variant l ->
    line i ppf "Ttype_variant\n";
    list (i + 1) constructor_decl ppf l
  | Ttype_record l ->
    line i ppf "Ttype_record\n";
    list (i + 1) label_decl ppf l
  | Ttype_open -> line i ppf "Ttype_open\n"

and type_extension i ppf x =
  line i ppf "type_extension\n";
  attributes i ppf x.tyext_attributes;
  let i = i + 1 in
  line i ppf "ptyext_path = %a\n" fmt_path x.tyext_path;
  line i ppf "ptyext_params =\n";
  list (i + 1) type_parameter ppf x.tyext_params;
  line i ppf "ptyext_constructors =\n";
  list (i + 1) extension_constructor ppf x.tyext_constructors;
  line i ppf "ptyext_private = %a\n" fmt_private_flag x.tyext_private

and extension_constructor i ppf x =
  line i ppf "extension_constructor %a\n" fmt_location x.ext_loc;
  attributes i ppf x.ext_attributes;
  let i = i + 1 in
  line i ppf "pext_name = \"%a\"\n" fmt_ident x.ext_id;
  line i ppf "pext_kind =\n";
  extension_constructor_kind (i + 1) ppf x.ext_kind

and extension_constructor_kind i ppf x =
  match x with
  | Text_decl (a, r) ->
    line i ppf "Text_decl\n";
    constructor_arguments (i + 1) ppf a;
    option (i + 1) core_type ppf r
  | Text_rebind (p, _) ->
    line i ppf "Text_rebind\n";
    line (i + 1) ppf "%a\n" fmt_path p

and module_type i ppf x =
  line i ppf "module_type %a\n" fmt_location x.mty_loc;
  attributes i ppf x.mty_attributes;
  let i = i + 1 in
  match x.mty_desc with
  | Tmty_ident (li, _) -> line i ppf "Tmty_ident %a\n" fmt_path li
  | Tmty_alias (li, _) -> line i ppf "Tmty_alias %a\n" fmt_path li
  | Tmty_signature s ->
    line i ppf "Tmty_signature\n";
    signature i ppf s
  | Tmty_functor (s, _, mt1, mt2) ->
    line i ppf "Tmty_functor \"%a\"\n" fmt_ident s;
    Misc.may (module_type i ppf) mt1;
    module_type i ppf mt2
  | Tmty_with (mt, l) ->
    line i ppf "Tmty_with\n";
    module_type i ppf mt;
    list i longident_x_with_constraint ppf l
  | Tmty_typeof m ->
    line i ppf "Tmty_typeof\n";
    module_expr i ppf m

and signature i ppf x = list i signature_item ppf x.sig_items

and signature_item i ppf x =
  line i ppf "signature_item %a\n" fmt_location x.sig_loc;
  let i = i + 1 in
  match x.sig_desc with
  | Tsig_value vd ->
    line i ppf "Tsig_value\n";
    value_description i ppf vd
  | Tsig_type (rf, l) ->
    line i ppf "Tsig_type %a\n" fmt_rec_flag rf;
    list i type_declaration ppf l
  | Tsig_typext e ->
    line i ppf "Tsig_typext\n";
    type_extension i ppf e
  | Tsig_exception ext ->
    line i ppf "Tsig_exception\n";
    extension_constructor i ppf ext
  | Tsig_module md ->
    line i ppf "Tsig_module \"%a\"\n" fmt_ident md.md_id;
    attributes i ppf md.md_attributes;
    module_type i ppf md.md_type
  | Tsig_recmodule decls ->
    line i ppf "Tsig_recmodule\n";
    list i module_declaration ppf decls
  | Tsig_modtype x ->
    line i ppf "Tsig_modtype \"%a\"\n" fmt_ident x.mtd_id;
    attributes i ppf x.mtd_attributes;
    modtype_declaration i ppf x.mtd_type
  | Tsig_open od ->
    line i ppf "Tsig_open %a %a\n" fmt_override_flag od.open_override fmt_path
      od.open_path;
    attributes i ppf od.open_attributes
  | Tsig_include incl ->
    line i ppf "Tsig_include\n";
    attributes i ppf incl.incl_attributes;
    module_type i ppf incl.incl_mod
  | Tsig_class () -> ()
  | Tsig_class_type () -> ()
  | Tsig_attribute (s, arg) ->
    line i ppf "Tsig_attribute \"%s\"\n" s.txt;
    Printast.payload i ppf arg

and module_declaration i ppf md =
  line i ppf "%a" fmt_ident md.md_id;
  attributes i ppf md.md_attributes;
  module_type (i + 1) ppf md.md_type

and module_binding i ppf x =
  line i ppf "%a\n" fmt_ident x.mb_id;
  attributes i ppf x.mb_attributes;
  module_expr (i + 1) ppf x.mb_expr

and modtype_declaration i ppf = function
  | None -> line i ppf "#abstract"
  | Some mt -> module_type (i + 1) ppf mt

and with_constraint i ppf x =
  match x with
  | Twith_type td ->
    line i ppf "Twith_type\n";
    type_declaration (i + 1) ppf td
  | Twith_typesubst td ->
    line i ppf "Twith_typesubst\n";
    type_declaration (i + 1) ppf td
  | Twith_module (li, _) -> line i ppf "Twith_module %a\n" fmt_path li
  | Twith_modsubst (li, _) -> line i ppf "Twith_modsubst %a\n" fmt_path li

and module_expr i ppf x =
  line i ppf "module_expr %a\n" fmt_location x.mod_loc;
  attributes i ppf x.mod_attributes;
  let i = i + 1 in
  match x.mod_desc with
  | Tmod_ident (li, _) -> line i ppf "Tmod_ident %a\n" fmt_path li
  | Tmod_structure s ->
    line i ppf "Tmod_structure\n";
    structure i ppf s
  | Tmod_functor (s, _, mt, me) ->
    line i ppf "Tmod_functor \"%a\"\n" fmt_ident s;
    Misc.may (module_type i ppf) mt;
    module_expr i ppf me
  | Tmod_apply (me1, me2, _) ->
    line i ppf "Tmod_apply\n";
    module_expr i ppf me1;
    module_expr i ppf me2
  | Tmod_constraint (me, _, Tmodtype_explicit mt, _) ->
    line i ppf "Tmod_constraint\n";
    module_expr i ppf me;
    module_type i ppf mt
  | Tmod_constraint (me, _, Tmodtype_implicit, _) -> module_expr i ppf me
  | Tmod_unpack (e, _) ->
    line i ppf "Tmod_unpack\n";
    expression i ppf e

and structure i ppf x = list i structure_item ppf x.str_items

and structure_item i ppf x =
  line i ppf "structure_item %a\n" fmt_location x.str_loc;
  let i = i + 1 in
  match x.str_desc with
  | Tstr_eval (e, attrs) ->
    line i ppf "Tstr_eval\n";
    attributes i ppf attrs;
    expression i ppf e
  | Tstr_value (rf, l) ->
    line i ppf "Tstr_value %a\n" fmt_rec_flag rf;
    list i value_binding ppf l
  | Tstr_primitive vd ->
    line i ppf "Tstr_primitive\n";
    value_description i ppf vd
  | Tstr_type (rf, l) ->
    line i ppf "Tstr_type %a\n" fmt_rec_flag rf;
    list i type_declaration ppf l
  | Tstr_typext te ->
    line i ppf "Tstr_typext\n";
    type_extension i ppf te
  | Tstr_exception ext ->
    line i ppf "Tstr_exception\n";
    extension_constructor i ppf ext
  | Tstr_module x ->
    line i ppf "Tstr_module\n";
    module_binding i ppf x
  | Tstr_recmodule bindings ->
    line i ppf "Tstr_recmodule\n";
    list i module_binding ppf bindings
  | Tstr_modtype x ->
    line i ppf "Tstr_modtype \"%a\"\n" fmt_ident x.mtd_id;
    attributes i ppf x.mtd_attributes;
    modtype_declaration i ppf x.mtd_type
  | Tstr_open od ->
    line i ppf "Tstr_open %a %a\n" fmt_override_flag od.open_override fmt_path
      od.open_path;
    attributes i ppf od.open_attributes
  | Tstr_class () -> ()
  | Tstr_class_type () -> ()
  | Tstr_include incl ->
    line i ppf "Tstr_include";
    attributes i ppf incl.incl_attributes;
    module_expr i ppf incl.incl_mod
  | Tstr_attribute (s, arg) ->
    line i ppf "Tstr_attribute \"%s\"\n" s.txt;
    Printast.payload i ppf arg

and longident_x_with_constraint i ppf (li, _, wc) =
  line i ppf "%a\n" fmt_path li;
  with_constraint (i + 1) ppf wc

and core_type_x_core_type_x_location i ppf (ct1, ct2, l) =
  line i ppf "<constraint> %a\n" fmt_location l;
  core_type (i + 1) ppf ct1;
  core_type (i + 1) ppf ct2

and constructor_decl i ppf
    {cd_id; cd_name = _; cd_args; cd_res; cd_loc; cd_attributes} =
  line i ppf "%a\n" fmt_location cd_loc;
  line (i + 1) ppf "%a\n" fmt_ident cd_id;
  attributes i ppf cd_attributes;
  constructor_arguments (i + 1) ppf cd_args;
  option (i + 1) core_type ppf cd_res

and constructor_arguments i ppf = function
  | Cstr_tuple l -> list i core_type ppf l
  | Cstr_record l -> list i label_decl ppf l

and label_decl i ppf
    {ld_id; ld_name = _; ld_mutable; ld_type; ld_loc; ld_attributes} =
  line i ppf "%a\n" fmt_location ld_loc;
  attributes i ppf ld_attributes;
  line (i + 1) ppf "%a\n" fmt_mutable_flag ld_mutable;
  line (i + 1) ppf "%a" fmt_ident ld_id;
  core_type (i + 1) ppf ld_type

and longident_x_pattern i ppf (li, _, p, opt) =
  line i ppf "%a%s\n" fmt_longident li (if opt then "?" else "");
  pattern (i + 1) ppf p

and case i ppf {c_lhs; c_guard; c_rhs} =
  line i ppf "<case>\n";
  pattern (i + 1) ppf c_lhs;
  (match c_guard with
  | None -> ()
  | Some g ->
    line (i + 1) ppf "<when>\n";
    expression (i + 2) ppf g);
  expression (i + 1) ppf c_rhs

and value_binding i ppf x =
  line i ppf "<def>\n";
  attributes (i + 1) ppf x.vb_attributes;
  pattern (i + 1) ppf x.vb_pat;
  expression (i + 1) ppf x.vb_expr

and record_field i ppf = function
  | _, Overridden (li, e), opt ->
    line i ppf "%a%s\n" fmt_longident li (if opt then "?" else "");
    expression (i + 1) ppf e
  | _, Kept _, _ -> line i ppf "<kept>"

and label_x_expression i ppf (l, e) =
  line i ppf "<arg>\n";
  arg_label (i + 1) ppf l;
  match e with
  | None -> ()
  | Some e -> expression (i + 1) ppf e

and label_x_bool_x_core_type_list i ppf x =
  match x with
  | Ttag (l, attrs, b, ctl) ->
    line i ppf "Ttag \"%s\" %s\n" l.txt (string_of_bool b);
    attributes (i + 1) ppf attrs;
    list (i + 1) core_type ppf ctl
  | Tinherit ct ->
    line i ppf "Tinherit\n";
    core_type (i + 1) ppf ct

let interface ppf x = list 0 signature_item ppf x.sig_items

let implementation ppf x = list 0 structure_item ppf x.str_items

let implementation_with_coercion ppf (x, _) = implementation ppf x
