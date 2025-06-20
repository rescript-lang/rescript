@@ocaml.text(
  /* ************************************************************************ */
  /*  */
  /* OCaml */
  /*  */
  /* Alain Frisch, LexiFi */
  /*  */
  /* Copyright 2012 Institut National de Recherche en Informatique et */
  /* en Automatique. */
  /*  */
  /* All rights reserved.  This file is distributed under the terms of */
  /* the GNU Lesser General Public License version 2.1, with the */
  /* special exception on linking described in the file LICENSE. */
  /*  */
  /* ************************************************************************ */

  " Helpers to produce Parsetree fragments "
)

open Asttypes
open Parsetree
open Docstrings
let docstring_body = assert false
let docstring_loc = assert false
let text_attr = assert false
let empty_docs = assert false
let add_docs_attrs = assert false
let add_text_attrs = assert false
let empty_info = assert false
let add_info_attrs = assert false

type lid = loc<Longident.t>
type str = loc<string>
type loc = Location.t
type attrs = list<attribute>

let default_loc = ref(Location.none)

@raises(genericException)
let with_default_loc = (l, f) => {
  let old = default_loc.contents
  default_loc := l
  try {
    let r = f()
    default_loc := old
    r
  } catch {
  | exn =>
    default_loc := old
    throw(exn)
  }
}

module Const = {
  let integer = (~suffix=?, i) => Pconst_integer(i, suffix)
  let int = (~suffix=?, i) => integer(~suffix?, string_of_int(i))
  let int32 = (~suffix='l', i) => integer(~suffix, Int32.to_string(i))
  let int64 = (~suffix='L', i) => integer(~suffix, Int64.to_string(i))
  let nativeint = (~suffix='n', i) => integer(~suffix, Nativeint.to_string(i))
  let float = (~suffix=?, f) => Pconst_float(f, suffix)
  let char = c => Pconst_char(c)
  let string = (~quotation_delimiter=?, s) => Pconst_string(s, quotation_delimiter)
}

module Typ = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    ptyp_desc: d,
    ptyp_loc: loc,
    ptyp_attributes: attrs,
  }
  let attr = (d, a) => {...d, ptyp_attributes: \"@"(d.ptyp_attributes, list{a})}

  let any = (~loc=?, ~attrs=?, ()) => mk(~loc?, ~attrs?, Ptyp_any)
  let var = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ptyp_var(a))
  let arrow = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Ptyp_arrow(a, b, c))
  let tuple = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ptyp_tuple(a))
  let constr = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_constr(a, b))
  let object_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_object(a, b))
  let class_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_class(a, b))
  let alias = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_alias(a, b))
  let variant = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Ptyp_variant(a, b, c))
  let poly = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_poly(a, b))
  let package = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ptyp_package(a, b))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ptyp_extension(a))

  let force_poly = t =>
    switch t.ptyp_desc {
    | Ptyp_poly(_) => t
    | _ => poly(~loc=t.ptyp_loc, list{}, t)
    } /* -> ghost? */

  @raises(Error)
  let varify_constructors = (var_names, t) => {
    @raises(Error)
    let check_variable = (vl, loc, v) =>
      if List.mem(v, vl) {
        throw({
          open Syntaxerr
          Error(Variable_in_scope(loc, v))
        })
      }
    let var_names = List.map(v => v.txt, var_names)

    @raises(Error)
    let rec loop = t => {
      let desc = switch t.ptyp_desc {
      | Ptyp_any => Ptyp_any
      | Ptyp_var(x) =>
        check_variable(var_names, t.ptyp_loc, x)
        Ptyp_var(x)
      | Ptyp_arrow(label, core_type, core_type') =>
        Ptyp_arrow(label, loop(core_type), loop(core_type'))
      | Ptyp_tuple(lst) => Ptyp_tuple(List.map(loop, lst))
      | Ptyp_constr({txt: Longident.Lident(s)}, list{}) if List.mem(s, var_names) => Ptyp_var(s)
      | Ptyp_constr(longident, lst) => Ptyp_constr(longident, List.map(loop, lst))
      | Ptyp_object(lst, o) => Ptyp_object(List.map(loop_object_field, lst), o)
      | Ptyp_class(longident, lst) => Ptyp_class(longident, List.map(loop, lst))
      | Ptyp_alias(core_type, string) =>
        check_variable(var_names, t.ptyp_loc, string)
        Ptyp_alias(loop(core_type), string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) =>
        Ptyp_variant(List.map(loop_row_field, row_field_list), flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) =>
        List.iter(v => check_variable(var_names, t.ptyp_loc, v.txt), string_lst)
        Ptyp_poly(string_lst, loop(core_type))
      | Ptyp_package(longident, lst) =>
        Ptyp_package(longident, List.map(((n, typ)) => (n, loop(typ)), lst))
      | Ptyp_extension(s, arg) => Ptyp_extension(s, arg)
      }

      {...t, ptyp_desc: desc}
    }
    @raises(Error)
    and loop_row_field = x =>
      switch x {
      | Rtag(label, attrs, flag, lst) => Rtag(label, attrs, flag, List.map(loop, lst))
      | Rinherit(t) => Rinherit(loop(t))
      }
    @raises(Error)
    and loop_object_field = x =>
      switch x {
      | Otag(label, attrs, t) => Otag(label, attrs, loop(t))
      | Oinherit(t) => Oinherit(loop(t))
      }

    loop(t)
  }
}

module Pat = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    ppat_desc: d,
    ppat_loc: loc,
    ppat_attributes: attrs,
  }
  let attr = (d, a) => {...d, ppat_attributes: \"@"(d.ppat_attributes, list{a})}

  let any = (~loc=?, ~attrs=?, ()) => mk(~loc?, ~attrs?, Ppat_any)
  let var = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_var(a))
  let alias = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_alias(a, b))
  let constant = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_constant(a))
  let interval = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_interval(a, b))
  let tuple = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_tuple(a))
  let construct = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_construct(a, b))
  let variant = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_variant(a, b))
  let record = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_record(a, b))
  let array = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_array(a))
  let or_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_or(a, b))
  let constraint_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_constraint(a, b))
  let type_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_type(a))
  let lazy_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_lazy(a))
  let unpack = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_unpack(a))
  let open_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Ppat_open(a, b))
  let exception_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_exception(a))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Ppat_extension(a))
}

module Exp = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    pexp_desc: d,
    pexp_loc: loc,
    pexp_attributes: attrs,
  }
  let attr = (d, a) => {...d, pexp_attributes: \"@"(d.pexp_attributes, list{a})}

  let ident = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_ident(a))
  let constant = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_constant(a))
  let let_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_let(a, b, c))
  let fun_ = (~loc=?, ~attrs=?, a, b, c, d) => mk(~loc?, ~attrs?, Pexp_fun(a, b, c, d))
  let function_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_function(a))
  let apply = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_apply(a, b))
  let match_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_match(a, b))
  let try_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_try(a, b))
  let tuple = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_tuple(a))
  let construct = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_construct(a, b))
  let variant = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_variant(a, b))
  let record = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_record(a, b))
  let field = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_field(a, b))
  let setfield = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_setfield(a, b, c))
  let array = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_array(a))
  let ifthenelse = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_ifthenelse(a, b, c))
  let sequence = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_sequence(a, b))
  let while_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_while(a, b))
  let for_ = (~loc=?, ~attrs=?, a, b, c, d, e) => mk(~loc?, ~attrs?, Pexp_for(a, b, c, d, e))
  let constraint_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_constraint(a, b))
  let coerce = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_coerce(a, b, c))
  let send = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_send(a, b))
  let new_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_new(a))
  let setinstvar = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_setinstvar(a, b))
  let override = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_override(a))
  let letmodule = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_letmodule(a, b, c))
  let letexception = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_letexception(a, b))
  let assert_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_assert(a))
  let lazy_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_lazy(a))
  let poly = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_poly(a, b))
  let object_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_object(a))
  let newtype = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pexp_newtype(a, b))
  let pack = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_pack(a))
  let open_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pexp_open(a, b, c))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pexp_extension(a))
  let unreachable = (~loc=?, ~attrs=?, ()) => mk(~loc?, ~attrs?, Pexp_unreachable)

  let case = (lhs, ~guard=?, rhs) => {
    pc_lhs: lhs,
    pc_guard: guard,
    pc_rhs: rhs,
  }
}

module Mty = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    pmty_desc: d,
    pmty_loc: loc,
    pmty_attributes: attrs,
  }
  let attr = (d, a) => {...d, pmty_attributes: \"@"(d.pmty_attributes, list{a})}

  let ident = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmty_ident(a))
  let alias = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmty_alias(a))
  let signature = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmty_signature(a))
  let functor_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pmty_functor(a, b, c))
  let with_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pmty_with(a, b))
  let typeof_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmty_typeof(a))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmty_extension(a))
}

module Mod = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    pmod_desc: d,
    pmod_loc: loc,
    pmod_attributes: attrs,
  }
  let attr = (d, a) => {...d, pmod_attributes: \"@"(d.pmod_attributes, list{a})}

  let ident = (~loc=?, ~attrs=?, x) => mk(~loc?, ~attrs?, Pmod_ident(x))
  let structure = (~loc=?, ~attrs=?, x) => mk(~loc?, ~attrs?, Pmod_structure(x))
  let functor_ = (~loc=?, ~attrs=?, arg, arg_ty, body) =>
    mk(~loc?, ~attrs?, Pmod_functor(arg, arg_ty, body))
  let apply = (~loc=?, ~attrs=?, m1, m2) => mk(~loc?, ~attrs?, Pmod_apply(m1, m2))
  let constraint_ = (~loc=?, ~attrs=?, m, mty) => mk(~loc?, ~attrs?, Pmod_constraint(m, mty))
  let unpack = (~loc=?, ~attrs=?, e) => mk(~loc?, ~attrs?, Pmod_unpack(e))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pmod_extension(a))
}

module Sig = {
  let mk = (~loc=default_loc.contents, d) => {psig_desc: d, psig_loc: loc}

  let value = (~loc=?, a) => mk(~loc?, Psig_value(a))
  let type_ = (~loc=?, rec_flag, a) => mk(~loc?, Psig_type(rec_flag, a))
  let type_extension = (~loc=?, a) => mk(~loc?, Psig_typext(a))
  let exception_ = (~loc=?, a) => mk(~loc?, Psig_exception(a))
  let module_ = (~loc=?, a) => mk(~loc?, Psig_module(a))
  let rec_module = (~loc=?, a) => mk(~loc?, Psig_recmodule(a))
  let modtype = (~loc=?, a) => mk(~loc?, Psig_modtype(a))
  let open_ = (~loc=?, a) => mk(~loc?, Psig_open(a))
  let include_ = (~loc=?, a) => mk(~loc?, Psig_include(a))
  let class_ = (~loc=?, a) => mk(~loc?, Psig_class(a))
  let class_type = (~loc=?, a) => mk(~loc?, Psig_class_type(a))
  let extension = (~loc=?, ~attrs=list{}, a) => mk(~loc?, Psig_extension(a, attrs))
  let attribute = (~loc=?, a) => mk(~loc?, Psig_attribute(a))
  let text = txt => {
    let f_txt = List.filter(ds => docstring_body(ds) != "", txt)
    List.map(ds => attribute(~loc=docstring_loc(ds), text_attr(ds)), f_txt)
  }
}

module Str = {
  let mk = (~loc=default_loc.contents, d) => {pstr_desc: d, pstr_loc: loc}

  let eval = (~loc=?, ~attrs=list{}, a) => mk(~loc?, Pstr_eval(a, attrs))
  let value = (~loc=?, a, b) => mk(~loc?, Pstr_value(a, b))
  let primitive = (~loc=?, a) => mk(~loc?, Pstr_primitive(a))
  let type_ = (~loc=?, rec_flag, a) => mk(~loc?, Pstr_type(rec_flag, a))
  let type_extension = (~loc=?, a) => mk(~loc?, Pstr_typext(a))
  let exception_ = (~loc=?, a) => mk(~loc?, Pstr_exception(a))
  let module_ = (~loc=?, a) => mk(~loc?, Pstr_module(a))
  let rec_module = (~loc=?, a) => mk(~loc?, Pstr_recmodule(a))
  let modtype = (~loc=?, a) => mk(~loc?, Pstr_modtype(a))
  let open_ = (~loc=?, a) => mk(~loc?, Pstr_open(a))
  let class_ = (~loc=?, a) => mk(~loc?, Pstr_class(a))
  let class_type = (~loc=?, a) => mk(~loc?, Pstr_class_type(a))
  let include_ = (~loc=?, a) => mk(~loc?, Pstr_include(a))
  let extension = (~loc=?, ~attrs=list{}, a) => mk(~loc?, Pstr_extension(a, attrs))
  let attribute = (~loc=?, a) => mk(~loc?, Pstr_attribute(a))
  let text = txt => {
    let f_txt = List.filter(ds => docstring_body(ds) != "", txt)
    List.map(ds => attribute(~loc=docstring_loc(ds), text_attr(ds)), f_txt)
  }
}

module Cl = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    pcl_desc: d,
    pcl_loc: loc,
    pcl_attributes: attrs,
  }
  let attr = (d, a) => {...d, pcl_attributes: \"@"(d.pcl_attributes, list{a})}

  let constr = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pcl_constr(a, b))
  let structure = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcl_structure(a))
  let fun_ = (~loc=?, ~attrs=?, a, b, c, d) => mk(~loc?, ~attrs?, Pcl_fun(a, b, c, d))
  let apply = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pcl_apply(a, b))
  let let_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcl_let(a, b, c))
  let constraint_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pcl_constraint(a, b))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcl_extension(a))
  let open_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcl_open(a, b, c))
}

module Cty = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, d) => {
    pcty_desc: d,
    pcty_loc: loc,
    pcty_attributes: attrs,
  }
  let attr = (d, a) => {...d, pcty_attributes: \"@"(d.pcty_attributes, list{a})}

  let constr = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pcty_constr(a, b))
  let signature = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcty_signature(a))
  let arrow = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcty_arrow(a, b, c))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcty_extension(a))
  let open_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcty_open(a, b, c))
}

module Ctf = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, ~docs=empty_docs, d) => {
    pctf_desc: d,
    pctf_loc: loc,
    pctf_attributes: add_docs_attrs(docs, attrs),
  }

  let inherit_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pctf_inherit(a))
  let val_ = (~loc=?, ~attrs=?, a, b, c, d) => mk(~loc?, ~attrs?, Pctf_val(a, b, c, d))
  let method_ = (~loc=?, ~attrs=?, a, b, c, d) => mk(~loc?, ~attrs?, Pctf_method(a, b, c, d))
  let constraint_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pctf_constraint(a, b))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pctf_extension(a))
  let attribute = (~loc=?, a) => mk(~loc?, Pctf_attribute(a))
  let text = txt => {
    let f_txt = List.filter(ds => docstring_body(ds) != "", txt)
    List.map(ds => attribute(~loc=docstring_loc(ds), text_attr(ds)), f_txt)
  }

  let attr = (d, a) => {...d, pctf_attributes: \"@"(d.pctf_attributes, list{a})}
}

module Cf = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, ~docs=empty_docs, d) => {
    pcf_desc: d,
    pcf_loc: loc,
    pcf_attributes: add_docs_attrs(docs, attrs),
  }

  let inherit_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcf_inherit(a, b, c))
  let val_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcf_val(a, b, c))
  let method_ = (~loc=?, ~attrs=?, a, b, c) => mk(~loc?, ~attrs?, Pcf_method(a, b, c))
  let constraint_ = (~loc=?, ~attrs=?, a, b) => mk(~loc?, ~attrs?, Pcf_constraint(a, b))
  let initializer_ = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcf_initializer(a))
  let extension = (~loc=?, ~attrs=?, a) => mk(~loc?, ~attrs?, Pcf_extension(a))
  let attribute = (~loc=?, a) => mk(~loc?, Pcf_attribute(a))
  let text = txt => {
    let f_txt = List.filter(ds => docstring_body(ds) != "", txt)
    List.map(ds => attribute(~loc=docstring_loc(ds), text_attr(ds)), f_txt)
  }

  let virtual_ = ct => Cfk_virtual(ct)
  let concrete = (o, e) => Cfk_concrete(o, e)

  let attr = (d, a) => {...d, pcf_attributes: \"@"(d.pcf_attributes, list{a})}
}

module Val = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~prim=list{},
    name,
    typ,
  ) => {
    pval_name: name,
    pval_type: typ,
    pval_attributes: add_docs_attrs(docs, attrs),
    pval_loc: loc,
    pval_prim: prim,
  }
}

module Md = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    name,
    typ,
  ) => {
    pmd_name: name,
    pmd_type: typ,
    pmd_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    pmd_loc: loc,
  }
}

module Mtd = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    ~typ=?,
    name,
  ) => {
    pmtd_name: name,
    pmtd_type: typ,
    pmtd_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    pmtd_loc: loc,
  }
}

module Mb = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    name,
    expr,
  ) => {
    pmb_name: name,
    pmb_expr: expr,
    pmb_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    pmb_loc: loc,
  }
}

module Opn = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, ~docs=empty_docs, ~override=Fresh, lid) => {
    popen_lid: lid,
    popen_override: override,
    popen_loc: loc,
    popen_attributes: add_docs_attrs(docs, attrs),
  }
}

module Incl = {
  let mk = (~loc=default_loc.contents, ~attrs=list{}, ~docs=empty_docs, mexpr) => {
    pincl_mod: mexpr,
    pincl_loc: loc,
    pincl_attributes: add_docs_attrs(docs, attrs),
  }
}

module Vb = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    pat,
    expr,
  ) => {
    pvb_pat: pat,
    pvb_expr: expr,
    pvb_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    pvb_loc: loc,
  }
}

module Ci = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    ~virt=Concrete,
    ~params=list{},
    name,
    expr,
  ) => {
    pci_virt: virt,
    pci_params: params,
    pci_name: name,
    pci_expr: expr,
    pci_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    pci_loc: loc,
  }
}

module Type = {
  let mk = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~text=list{},
    ~params=list{},
    ~cstrs=list{},
    ~kind=Ptype_abstract,
    ~priv=Public,
    ~manifest=?,
    name,
  ) => {
    ptype_name: name,
    ptype_params: params,
    ptype_cstrs: cstrs,
    ptype_kind: kind,
    ptype_private: priv,
    ptype_manifest: manifest,
    ptype_attributes: add_text_attrs(text, add_docs_attrs(docs, attrs)),
    ptype_loc: loc,
  }

  let constructor = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~info=empty_info,
    ~args=Pcstr_tuple(list{}),
    ~res=?,
    name,
  ) => {
    pcd_name: name,
    pcd_args: args,
    pcd_res: res,
    pcd_loc: loc,
    pcd_attributes: add_info_attrs(info, attrs),
  }

  let field = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~info=empty_info,
    ~mut=Immutable,
    name,
    typ,
  ) => {
    pld_name: name,
    pld_mutable: mut,
    pld_type: typ,
    pld_loc: loc,
    pld_attributes: add_info_attrs(info, attrs),
  }
}

@ocaml.doc(" Type extensions ")
module Te = {
  let mk = (~attrs=list{}, ~docs=empty_docs, ~params=list{}, ~priv=Public, path, constructors) => {
    ptyext_path: path,
    ptyext_params: params,
    ptyext_constructors: constructors,
    ptyext_private: priv,
    ptyext_attributes: add_docs_attrs(docs, attrs),
  }

  let constructor = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~info=empty_info,
    name,
    kind,
  ) => {
    pext_name: name,
    pext_kind: kind,
    pext_loc: loc,
    pext_attributes: add_docs_attrs(docs, add_info_attrs(info, attrs)),
  }

  let decl = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~info=empty_info,
    ~args=Pcstr_tuple(list{}),
    ~res=?,
    name,
  ) => {
    pext_name: name,
    pext_kind: Pext_decl(args, res),
    pext_loc: loc,
    pext_attributes: add_docs_attrs(docs, add_info_attrs(info, attrs)),
  }

  let rebind = (
    ~loc=default_loc.contents,
    ~attrs=list{},
    ~docs=empty_docs,
    ~info=empty_info,
    name,
    lid,
  ) => {
    pext_name: name,
    pext_kind: Pext_rebind(lid),
    pext_loc: loc,
    pext_attributes: add_docs_attrs(docs, add_info_attrs(info, attrs)),
  }
}

module Csig = {
  let mk = (self, fields) => {
    pcsig_self: self,
    pcsig_fields: fields,
  }
}

module Cstr = {
  let mk = (self, fields) => {
    pcstr_self: self,
    pcstr_fields: fields,
  }
}
