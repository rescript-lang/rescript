(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

module Lid = struct
  type t = Longident.t

  let val_unit : t = Lident "()"

  let type_unit : t = Lident "unit"

  let type_string : t = Lident "string"

  let type_int : t = Lident "int" (* use *predef* *)

  let type_bool : t = Lident "bool" (* use *predef* *)

  let js_extern : t = Lident Primitive_modules.js_extern

  let method_callback : t = Ldot (js_extern, "Callback")

  let hidden_field n : t = Lident ("I" ^ n)

  let js_undefined : t = Lident "undefined"

  let regexp_id : t = Ldot (Lident "Stdlib_RegExp", "t")
end

module No_loc = struct
  let loc = Location.none

  let val_unit =
    Ast_helper.Exp.construct {txt = Lid.val_unit; loc} {txt = []; loc}

  let type_unit =
    Ast_helper.Typ.mk (Ptyp_constr ({txt = Lid.type_unit; loc}, []))

  let type_int = Ast_helper.Typ.mk (Ptyp_constr ({txt = Lid.type_int; loc}, []))

  let type_string =
    Ast_helper.Typ.mk (Ptyp_constr ({txt = Lid.type_string; loc}, []))

  let type_bool =
    Ast_helper.Typ.mk (Ptyp_constr ({txt = Lid.type_bool; loc}, []))
end

type 'a lit = ?loc:Location.t -> unit -> 'a

type expression_lit = Parsetree.expression lit

type core_type_lit = Parsetree.core_type lit

let val_unit ?loc () =
  match loc with
  | None -> No_loc.val_unit
  | Some loc ->
    Ast_helper.Exp.construct {txt = Lid.val_unit; loc} {txt = []; loc}

let type_unit ?loc () =
  match loc with
  | None -> No_loc.type_unit
  | Some loc ->
    Ast_helper.Typ.mk ~loc (Ptyp_constr ({txt = Lid.type_unit; loc}, []))

let type_string ?loc () =
  match loc with
  | None -> No_loc.type_string
  | Some loc ->
    Ast_helper.Typ.mk ~loc (Ptyp_constr ({txt = Lid.type_string; loc}, []))

let type_bool ?loc () =
  match loc with
  | None -> No_loc.type_bool
  | Some loc ->
    Ast_helper.Typ.mk ~loc (Ptyp_constr ({txt = Lid.type_bool; loc}, []))

let type_int ?loc () =
  match loc with
  | None -> No_loc.type_int
  | Some loc ->
    Ast_helper.Typ.mk ~loc (Ptyp_constr ({txt = Lid.type_int; loc}, []))

let type_float =
  Ast_helper.Typ.mk
    (Ptyp_constr ({txt = Lident "float"; loc = Location.none}, []))
