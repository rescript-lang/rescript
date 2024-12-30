(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try
    let r = f () in
    default_loc := old;
    r
  with exn ->
    default_loc := old;
    raise exn

module Const = struct
  let integer ?suffix i = Pconst_integer (i, suffix)
  let int ?suffix i = integer ?suffix (string_of_int i)
  let int32 ?(suffix = 'l') i = integer ~suffix (Int32.to_string i)
  let int64 ?(suffix = 'L') i = integer ~suffix (Int64.to_string i)
  let nativeint ?(suffix = 'n') i = integer ~suffix (Nativeint.to_string i)
  let float ?suffix f = Pconst_float (f, suffix)
  let char c = Pconst_char (Char.code c)
  let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
end

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs ~arity a b c =
    mk ?loc ?attrs (Ptyp_arrow (a, b, c, arity))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)

  let varify_constructors var_names t =
    let check_variable vl loc v =
      if List.mem v vl then raise Syntaxerr.(Error (Variable_in_scope (loc, v)))
    in
    let var_names = List.map (fun v -> v.txt) var_names in
    let rec loop t =
      let desc =
        match t.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
        | Ptyp_arrow (label, core_type, core_type', a) ->
          Ptyp_arrow (label, loop core_type, loop core_type', a)
        | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
        | Ptyp_constr ({txt = Longident.Lident s}, []) when List.mem s var_names
          ->
          Ptyp_var s
        | Ptyp_constr (longident, lst) ->
          Ptyp_constr (longident, List.map loop lst)
        | Ptyp_object (lst, o) -> Ptyp_object (List.map loop_object_field lst, o)
        | Ptyp_alias (core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias (loop core_type, string)
        | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant
            (List.map loop_row_field row_field_list, flag, lbl_lst_option)
        | Ptyp_poly (string_lst, core_type) ->
          List.iter
            (fun v -> check_variable var_names t.ptyp_loc v.txt)
            string_lst;
          Ptyp_poly (string_lst, loop core_type)
        | Ptyp_package (longident, lst) ->
          Ptyp_package (longident, List.map (fun (n, typ) -> (n, loop typ)) lst)
        | Ptyp_extension (s, arg) -> Ptyp_extension (s, arg)
      in
      {t with ptyp_desc = desc}
    and loop_row_field = function
      | Rtag (label, attrs, flag, lst) ->
        Rtag (label, attrs, flag, List.map loop lst)
      | Rinherit t -> Rinherit (loop t)
    and loop_object_field = function
      | Otag (label, attrs, t) -> Otag (label, attrs, loop t)
      | Oinherit t -> Oinherit (loop t)
    in
    loop t
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
  let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs ~arity a b c d =
    mk ?loc ?attrs
      (Pexp_fun {arg_label = a; default = b; lhs = c; rhs = d; arity})
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a c = mk ?loc ?attrs (Pexp_coerce (a, (), c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)

  let case lhs ?guard rhs = {pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs}
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg arg_ty body =
    mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc a = mk ?loc (Psig_include a)

  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(prim = []) name typ =
    {
      pval_name = name;
      pval_type = typ;
      pval_attributes = attrs;
      pval_loc = loc;
      pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(attrs = []) name typ =
    {pmd_name = name; pmd_type = typ; pmd_attributes = attrs; pmd_loc = loc}
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?typ name =
    {pmtd_name = name; pmtd_type = typ; pmtd_attributes = attrs; pmtd_loc = loc}
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) name expr =
    {pmb_name = name; pmb_expr = expr; pmb_attributes = attrs; pmb_loc = loc}
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(override = Fresh) lid =
    {
      popen_lid = lid;
      popen_override = override;
      popen_loc = loc;
      popen_attributes = attrs;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) mexpr =
    {pincl_mod = mexpr; pincl_loc = loc; pincl_attributes = attrs}
end

module Vb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) pat expr =
    {pvb_pat = pat; pvb_expr = expr; pvb_attributes = attrs; pvb_loc = loc}
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(params = []) ?(cstrs = [])
      ?(kind = Ptype_abstract) ?(priv = Public) ?manifest name =
    {
      ptype_name = name;
      ptype_params = params;
      ptype_cstrs = cstrs;
      ptype_kind = kind;
      ptype_private = priv;
      ptype_manifest = manifest;
      ptype_attributes = attrs;
      ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(args = Pcstr_tuple [])
      ?res name =
    {
      pcd_name = name;
      pcd_args = args;
      pcd_res = res;
      pcd_loc = loc;
      pcd_attributes = attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(mut = Immutable)
      ?(optional = false) name typ =
    {
      pld_name = name;
      pld_mutable = mut;
      pld_optional = optional;
      pld_type = typ;
      pld_loc = loc;
      pld_attributes = attrs;
    }
end

(** Type extensions *)
module Te = struct
  let mk ?(attrs = []) ?(params = []) ?(priv = Public) path constructors =
    {
      ptyext_path = path;
      ptyext_params = params;
      ptyext_constructors = constructors;
      ptyext_private = priv;
      ptyext_attributes = attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) name kind =
    {
      pext_name = name;
      pext_kind = kind;
      pext_loc = loc;
      pext_attributes = attrs;
    }

  let decl ?(loc = !default_loc) ?(attrs = []) ?(args = Pcstr_tuple []) ?res
      name =
    {
      pext_name = name;
      pext_kind = Pext_decl (args, res);
      pext_loc = loc;
      pext_attributes = attrs;
    }

  let rebind ?(loc = !default_loc) ?(attrs = []) name lid =
    {
      pext_name = name;
      pext_kind = Pext_rebind lid;
      pext_loc = loc;
      pext_attributes = attrs;
    }
end
